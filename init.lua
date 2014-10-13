--[[
    icht, I CAN HAZ TEMPLATEZ
    compiles jinja2 templates into basic lua code
    Neither completely stable nor standards compliant (I really should read the docs one day)
]]
local Object = require "luvit.core".Object
local ffi = require "ffi"
require "luvit.utils".DUMP_MAX_DEPTH = 3

local function isAlpha(c)
    if type(c) == "nil" then return false end
    return (c >= 65 and c <= 90) or (c >= 97 and c <= 122)
end

local function isNumeric(c)
    if type(c) == "nil" then return false end
    return (c >= 48 and c <= 57)
end

local function isAlphaNumeric(c)
    if type(c) == "nil" then return false end
    return (c >= 48 and c <= 57) or (c >= 65 and c <= 90) or (c >= 97 and c <= 122)
end

local function isNewline(c)
    if type(c) == "nil" then return false end
    return c == 10 or c == 13
end

local function isWhiteSpace(c)
    if type(c) == "nil" then return false end
    return c == 20 or c == 32 or c == 10 or c == 13 or c == 9 or c == 11 or c == 12 -- space, \n, \r, \t, \v, \f
end

-- We manually allocate and deallocate buffers
ffi.cdef [[
    void *malloc(size_t len);
    void free(void *);
    
    typedef struct _ichts_buffer_s
    {
        size_t length;
        unsigned int referenceCount;
        char buffer[?];
    } _ichts_buffer;
    
    typedef struct _ichts_buffer_container_s
    {
        _ichts_buffer *buffer;
        char *head, *tail;
        int line, col;
    } _ichts_buffer_container;
]]

local function refBuffer(buf)
    buf.referenceCount = buf.referenceCount + 1
--     p("ref", buf.referenceCount)
end

local function unRefBuffer(buf)
    buf.referenceCount = buf.referenceCount - 1
--     p("unref", buf.referenceCount)
    if buf.referenceCount < 1 then
--         p "REF END"
        ffi.C.free(buf)
    end
end

local Buffer
Buffer = ffi.metatype("_ichts_buffer_container", {
        __gc = function(self)
            unRefBuffer(self.buffer)
        end,
        __index = {
            new = function(self, buffer, length)
                if type(buffer) == "string" then
                    length = length or #buffer
                    local b = ffi.cast("_ichts_buffer *", ffi.C.malloc(ffi.sizeof("_ichts_buffer", length)))
                    b.length = length
                    b.referenceCount = 0
                    ffi.copy(b.buffer, buffer, length)
                    buffer = b
                elseif ffi.typeof(buffer) == Buffer then
                    buffer = buffer.buffer
                else
                    p(buffer, self, ffi.typeof(buffer))
                    assert(false, "Not supported for now")
                end
                
                refBuffer(buffer)
                local new = ffi.new(self, {
                    buffer = buffer,
                })
                new:reset()
                return new
            end,
            save = function(self)
                return self:new(self)
            end,
            restore = function(self, saved)
                assert(self.buffer == saved.buffer, "wut, not same?")
                self.head = saved.head
                self.tail = saved.tail
                self.line = saved.line
                self.col = saved.col
            end,
            sub = function(self, head, tail)
                local s = self:new(self)
                s.head = head
                s.tail = tail
                return s
            end,
            passed = function(self, saved)
                return self.head >= saved.head
            end,
            next = function(self)
                local char = self:peek()
                self:skip(char)
                return char
            end,
            peek = function(self)
                return self.head ~= nil and self.head[0] or nil
            end,
            skip = function(self, char)
                if self:left() then
                    if char and isNewline(char) then
                        self.line, self.col = self.line + 1, 0
                    end
                    self.col = self.col + 1
                    self.head = self.head + 1
                    if self.head >= self.tail then
                        self.head = nil
                    end
                end
            end,
            left = function(self)
                return self.head ~= nil 
            end,
            readWhile = function(self, condition)
                local oldHead = self.head
                
                while self:left() and condition(self:peek()) do
                    self:skip()
                end
                
                return self:sub(oldHead, self.head)
            end,
            readUntilNewline = function(self)
                return self:readWhile(function(c)
                    return not isNewline(c)
                end)
            end,
            length = function(self)
                return self.tail - self.head
            end,
            toString = function(self)
                return ffi.string(self.head, self:length())
            end,
            reset = function(self)
                self.line, self.col = 1, 0
                self.head = self.buffer.buffer
                self.tail = self.buffer.buffer + self.buffer.length
            end
        }
    }
)

local TableBuffer = Object:extend()

function TableBuffer:initialize(buffer)
    self.buffer = {}
    for i, value in pairs(buffer) do
        self.buffer[#buffer-i+1] = value
    end
end

function TableBuffer:peek()
    return self.buffer[#self.buffer]
end

function TableBuffer:skip()
    self.buffer[#self.buffer] = nil
end

function TableBuffer:next()
    local saved = self:peek()
    self:skip()
    return saved
end

function TableBuffer:left()
    return #self.buffer > 0
end

local Stack = Object:extend()

function Stack:initialize()
    self.stack = {}
    self.location = 0
end

function Stack:push(status)
    self.location = self.location + 1
    self.stack[self.location] = status
end

function Stack:peek()
    return self.stack[self.location]
end

function Stack:pop()
    local v = self:peek()
    self.stack[self.location] = nil
    self.location = self.location - 1
    return v
end

function Stack:swap(status)
    self.stack[self.location] = status
end

function Stack:is(s)
    return self:peek() == s
end

function Stack:isOneOf(t)
    for k, status in pairs(t) do
        if self:is(status) then
            return true
        end
    end
    return false
end

local table = require "table"
local string = require "string"
local TAG_OPEN          = string.byte "{"
local TAG_CLOSE         = string.byte "}"
local TAG_OPEN_ECHO     = string.byte "{"
local TAG_CLOSE_ECHO    = string.byte "}"
local TAG_OPEN_CONTROL  = string.byte "%"
local TAG_OPEN_COMMENT  = string.byte "#"

-- token type / what are we currently reading
local tokens = {
    "TOKEN_BLOB",
    "TOKEN_MIXED_OPEN",
    "TOKEN_CONTROL_INNER",
    "TOKEN_ECHO_INNER",
    "TOKEN_COMMENT_INNER",
    "TOKEN_MIXED_CLOSE",
}

local Scanner = require "luvit.core".Object:extend()

function Scanner:initialize()
    self.tokens = {}
    self.status = Stack:new()
    self.status:push("TOKEN_BLOB")
    self.leftovers = nil
end

function Scanner:addToken(type, data)
    self.tokens[#self.tokens + 1] = {type, data}
end

function Scanner:packTokens()
    local removed = {}
    for i, token in pairs(self.tokens) do
        if token[1] == "TOKEN_MIXED_OPEN" and (token[2] == "TOKEN_CONTROL_INNER" or token[2] == "TOKEN_COMMENT_INNER") then
            local passed = false
            local merge = token[2] == "TOKEN_COMMENT_INNER"
            for j, token2 in pairs(self.tokens) do
                if passed and token2[1] == "TOKEN_MIXED_CLOSE" then
                    self.tokens[j] = removed
                    break
                elseif passed and merge then
                    token[#token+1] = token2
                    self.tokens[j] = removed
                elseif token2 == token then
                    if merge then
                        table.remove(token, 1)
                    else
                        self.tokens[j] = removed
                    end
                    passed = true
                end
            end
        elseif token[1] == "TOKEN_MIXED_OPEN" and token[2] == "TOKEN_ECHO_INNER" then
            local passed = false
            for j, token2 in pairs(self.tokens) do
                if passed and token2[1] == "TOKEN_MIXED_CLOSE" then
                    token2[1] = "TOKEN_ECHO_CLOSE"
                    break
                elseif token2 == token then
                    table.remove(token, 1)
                    token[1] = "TOKEN_ECHO_OPEN"
                    passed = true
                end
            end
        end
    end
    
    local shift = 0
    local length = #self.tokens
    for i = 1, length do
        if self.tokens[i] == removed then
            shift = shift + 1
            self.tokens[i] = nil
        elseif shift > 0 then -- In case of mid-table removed elements
            self.tokens[i-shift] = self.tokens[i]
            self.tokens[i] = nil
        end
    end
end

function Scanner:toPrintable(buffer, sep)
    local out = {}
    
    local s = buffer:save()
    while buffer:left() do
        out[#out+1] = string.char(buffer:next())
    end
    buffer:restore(s)
    
    return table.concat(out, sep or "")
end

function Scanner:error(chunk, msg, expecting)
    expecting = expecting or {}
    local position = chunk:save()
    local c = ("%q"):format(self:toPrintable {chunk:peek()}):gsub("\\\n","\\n")
    chunk:reset()
    local lastNewline
    while chunk:left() do
        if not chunk:passed(position) then
            local c = chunk:next()
            if isNewline(c) then
                lastNewline = chunk:save()
            end
        else
            break
        end
    end
    
    if lastNewline then
        chunk:restore(lastNewline)
    else
        chunk:reset()
        lastNewline = true
    end
    
    error(("%%s:%i:%i: %s%s%s%s\n%s"):format(
        --file,
        position[3], position[4],
        msg,
        expecting and " (Expecting: \""..self:toPrintable(expecting) .. "\" Got:" .. c.. ")" or "",
        lastNewline and "\n\n" or "",
        lastNewline and self:toPrintable(chunk:readUntilNewline()) or "",
        lastNewline and ("-"):rep(position[4]).."^\n" or ""
    ))
end

function Scanner:readRawData(chunk)
    local blob = chunk:readWhile(function(char)
        return char ~= TAG_OPEN
    end)
    
    if blob:length() > 0 then
        self:addToken("TOKEN_BLOB", blob)
    end
    
    if chunk:next() == TAG_OPEN then
        self.status:push("TOKEN_MIXED_OPEN")
    end
end

function Scanner:readOpenTag(chunk)
    assert(self.status:is("TOKEN_MIXED_OPEN"), "reading when not in TOKEN_MIXED_OPEN state")
    local c = chunk:peek()
    if c == TAG_OPEN then
        self:addToken("TOKEN_MIXED_OPEN", "TOKEN_ECHO_INNER")
        self.status:swap("TOKEN_ECHO_INNER")
    elseif c == TAG_OPEN_CONTROL then
        self:addToken("TOKEN_MIXED_OPEN", "TOKEN_CONTROL_INNER")
        self.status:swap("TOKEN_CONTROL_INNER")
    elseif c == TAG_OPEN_COMMENT then
        self:addToken("TOKEN_MIXED_OPEN", "TOKEN_COMMENT_INNER")
        self.status:swap("TOKEN_COMMENT_INNER")
    else
        return self:error(chunk, "Invalid opening tag", {TAG_OPEN_ECHO, TAG_OPEN_CONTROL})
    end
    chunk:skip()
end

function Scanner:readClosingTag(chunk)
    local close = self:getCurrentClosingTag()
    
    assert(chunk:next() == close)
    
    if chunk:next() == TAG_CLOSE then
        if self.status ~= "TOKEN_COMMENT_INNER" then
            self:addToken("TOKEN_MIXED_CLOSE")
            self:packTokens()
        end
        self.status:pop()
        return true
    end
    
    return false
end

function Scanner:skipWhitespace(chunk)
    chunk:readWhile(function(c)
        return isWhiteSpace(c)
    end)
end

function Scanner:skipComment(chunk)
    chunk:readWhile(function(c)
        return c ~= TAG_OPEN_COMMENT
    end)

    if chunk:peek() == TAG_OPEN_COMMENT then
        local saved = chunk:save()
        chunk:skip()
        if not chunk:left() then
            chunk:restore(saved)
        elseif chunk:next() == TAG_CLOSE then
            self.status:pop()
        end        
    end
end

function Scanner:isStartOfWord(c)
    return isAlphaNumeric(c)
end

function Scanner:parseWord(buffer)
    local word = self:toPrintable(buffer:readWhile(function(c)
        return self:isStartOfWord(c)
    end))
    
    if self.tokens[#self.tokens][1] == "TOKEN_WORD_PART" then
        self.tokens[#self.tokens][2] = self.tokens[#self.tokens][2] .. word
        if buffer:left() then
            self.tokens[#self.tokens][1] = "TOKEN_WORD"
        end
    else
        self:addToken(buffer:left() and "TOKEN_WORD" or "TOKEN_WORD_PART", word)
    end
end

function Scanner:isStartOfString(c)
    return c == string.byte '"' or c == string.byte "'"
end

function Scanner:parseString(buffer)
    if not self.status:isOneOf {"TOKEN_STRING", "TOKEN_STRING_SINGLE"} then
        local single = buffer:next() == string.byte "'"
        self.status:push(single and "TOKEN_STRING_SINGLE" or "TOKEN_STRING")
        self:parseString(buffer)
    else
        self:addToken("TOKEN_STRING", buffer:readWhile(function(c)
            return (self.status:is("TOKEN_STRING_SINGLE") and c ~= string.byte "'") or (self.status:is("TOKEN_STRING") and c ~= string.byte '"')
        end))
        
        local c = buffer:peek()
        if (self.status:is("TOKEN_STRING_SINGLE") and c == string.byte "'") or (self.status:is("TOKEN_STRING") and c == string.byte '"') then
            buffer:skip()
            local oldStatus = self.status:pop()
            assert("TOKEN_STRING" == oldStatus)
        end
    end
end

function Scanner:getCurrentClosingTag()
    return (self.status:is("TOKEN_CONTROL_INNER") and TAG_OPEN_CONTROL) or (self.status:is("TOKEN_ECHO_INNER") and TAG_CLOSE_ECHO)
end

function Scanner:mayBeClosingTag(c)
    return self:getCurrentClosingTag() == c
end

function Scanner:isSyntaxChar(c)
    return c == string.byte "," or c == string.byte "|"
end

function Scanner:parseSyntaxChar(chunk)
    local str = self:toPrintable(chunk:readWhile(function(c)
        return self:isSyntaxChar(c)
    end))
    -- TODO: allow syntax seperated over 2 chunks
    self:addToken("TOKEN_SYNTAX", str)
end

function Scanner:readBody(chunk)
    self:skipWhitespace(chunk)
    if chunk:left() then
        if self.status:is("TOKEN_COMMENT_INNER") then
            chunk:readWhile(function(c)
                return not self:mayBeClosingTag(c)
            end)
            
            if self:mayBeClosingTag(chunk:peek()) then
                self:readClosingTag(chunk)
            end
        elseif self.status:isOneOf {"TOKEN_CONTROL_INNER", "TOKEN_ECHO_INNER"} then
            local c = chunk:peek()
            if self:isStartOfWord(c) then
                self:parseWord(chunk)
            elseif isNumeric(c) then
                self:parseNumber(chunk)
            elseif self:isStartOfString(c) then
                self:parseString(chunk)
            elseif self:isSyntaxChar(c) then
                self:parseSyntaxChar(chunk)
            else
                local saved = chunk:save()
                if self:mayBeClosingTag(c) then
                    if not self:readClosingTag(chunk) then
                        local hadLeft = chunk:left()
                        p(chunk, saved)
                        chunk:restore(saved)
                        
                        if hadLeft then
                            -- Try something else
                            p("E", self:toPrintable({c}), chunk.currentPosition - chunk.buffer, self.status)
                            chunk:skip()
                        end
                    end
                else
                    p("C", self:toPrintable({c}), chunk.currentPosition - chunk.buffer, self.status)
                    chunk:skip()
                end
            end
        end
    end
end

function Scanner:parseChunk(chunk)
    while not self.abort and chunk:left() do
        -- Looking for an opening tag
        if self.status:is("TOKEN_BLOB") then
            self:readRawData(chunk)
        elseif self.status:is("TOKEN_MIXED_OPEN") then
            self:readOpenTag(chunk)
        elseif self.status:is("TOKEN_COMMENT_INNER") then
            self:skipComment(chunk)
        else
            self:readBody(chunk)
        end
    end
    self.abort = false
end

local stream = require "luvit.fs".createReadStream("resources/lua/web/templates/test.html.tpl")

local Parser = Object:extend()

function Parser:initialize()
    self.statusStack = Stack:new()
    self.statusStack:push("TOKEN_NONE")
    self.currentToken = { type = "TOKEN_ROOT", parent = nil, children = {} }
    self.tree = self.currentToken
end

function Parser:startToken(tokenName)
    self.statusStack:push(tokenName)
    self.currentToken = { type = tokenName, parent = self.currentToken, children = {} }
end

function Parser:endToken()
    local p = self.currentToken.parent
    local new = {}
    
    if not p then
        _G.p(self)
    end
    
    for k, v in pairs(self.currentToken) do
        new[k] = k ~= "parent" and k:sub(1, 1) ~= "_" and v or nil
    end
    
    p.children[#p.children+1] = new
    self.currentToken = p
    self.statusStack:pop()
end

local wordStatus = {
    ["for"] = "TOKEN_FOR",
    ["extends"] = "TOKEN_EXTENDS",
    ["block"] = "TOKEN_BLOCK",
    
    ["endfor"] = "TOKEN_INVALID",
    ["endblock"] = "TOKEN_INVALID",
}

function Parser:parse(tokens)
    while tokens:left() do
        local t = tokens:peek()
        local token = self.currentToken

        if self.statusStack:isOneOf {"TOKEN_NONE", "TOKEN_INNER", "TOKEN_CONTROL_INNER"} then
            if t[1] == "TOKEN_BLOB" then
                self:startToken("TOKEN_BLOB")
                    self.currentToken.value = tokens:next()[2]
                self:endToken()
            elseif t[1] == "TOKEN_WORD" then
                local status = wordStatus[t[2]]
                if status == "TOKEN_INVALID" then
                    self.statusStack:pop() -- Try again
                else
                    self:startToken(status)
                end
            elseif t[1] == "TOKEN_ECHO_OPEN" then
                self:startToken("TOKEN_ECHO")
                tokens:skip()
            elseif t[1] == "TOKEN_MIXED_CLOSE" then
                local status = self.statusStack:peek()
                if status ~= "TOKEN_CONTROL_INNER" then
                    p(status, self.statusStack, self.currentToken)
                    error "Invalid status "
                end
                self:endToken()
                tokens:skip()
            elseif t[1] == "TOKEN_COMMENT_INNER" then
                tokens:skip()
            else
                p("UNKOWN TOKEN TYPE", t)
                error ""
            end
        elseif self.statusStack:is("TOKEN_ECHO") then
            if t[1] == "TOKEN_STRING" then
                self:startToken("TOKEN_STRING")
                    self.currentToken.value = tokens:next()[2]
                self:endToken()
            elseif t[1] == "TOKEN_WORD" then
                self:startToken("TOKEN_WORD")
            elseif t[1] == "TOKEN_ECHO_CLOSE" then
                self:endToken()
                tokens:skip()
            elseif t[1] == "TOKEN_SYNTAX" then
                if t[2] == "|" then
                    -- Pop the "self" value off the echo token
                    local lastChild = self.currentToken.children[#self.currentToken.children]
                    self.currentToken.children[#self.currentToken.children] = nil
                    self:startToken("TOKEN_CALL")
                    self.currentToken.isFilter = true
                    self.currentToken.haveName = false
                    self.currentToken.children = { [2] = lastChild }
                    tokens:skip()
                end
            else
                p(t)
                error "Invalid token in echo"
            end
        elseif self.statusStack:is("TOKEN_WORD") then
            if t[1] == "TOKEN_WORD" then
                token.value = tokens:next()[2]
                self:endToken()
            elseif t[1] == "TOKEN_SYNTAX" then
                if t[2] == "|" then
                    self:endToken() -- Someone else will deal with this
                else
                    p(t)
                    error "Unkown syntax char"
                end
            elseif t[1] == "TOKEN_ECHO_CLOSE" then
                self:endToken()
            else
                p(self.statusStack, t)
                error "Unkown token when reading word"
            end
        elseif self.statusStack:is("TOKEN_CALL") then
            if t[1] == "TOKEN_WORD" then
                if not token.haveName then
                    token.children[1] = {type = "TOKEN_WORD", value = t[2]} -- TODO: less hacky
                else
                    
                end
                tokens:skip()
            elseif t[1] == "TOKEN_ECHO_CLOSE" then
                self:endToken()
            else
                p(t)
                error "Unkown token when doing call"
            end
        elseif self.statusStack:is("TOKEN_EXTENDS") then
            if not token._parsed_word then
                token._parsed_word = true
                tokens:skip()
            elseif not token.path then
                if t[1] == "TOKEN_STRING" then
                    token.path = tokens:next()[2]
                    self:endToken()
                else
                    p(tokens:next())
                    error "Unexpected"
                end
            else
                p(tokens:next())
                error "How !?"
            end
        elseif self.statusStack:is("TOKEN_BLOCK") then
            if not token._parsed_word then
                token._parsed_word = true
                tokens:skip()
            elseif not self.name then
                if t[1] == "TOKEN_STRING" or t[1] == "TOKEN_WORD" then
                    token.path = tokens:next()[2]
                    self.statusStack:swap("TOKEN_BLOCK_END")
                    self.statusStack:push("TOKEN_INNER")
                else
                    p(tokens:next())
                    error "Unexpected"
                end
            else
                p(tokens:next())
                error "Unexpected"
            end
        elseif self.statusStack:is("TOKEN_BLOCK_END") then
            assert(token.type == "TOKEN_BLOCK", "Unexpected endfor: "..token.type)
            tokens:skip()
            self:endToken()
        elseif self.statusStack:is("TOKEN_FOR") then
            if not token._parsed_word then
                token._parsed_word = true
                token.unpack = {}
                tokens:skip()
            elseif not token._parsed_in then
                if t[1] == "TOKEN_WORD" then
                    if t[2] ~= "in" then
                        token.unpack[#token.unpack+1] = tokens:next()[2]
                    else
                        tokens:skip()
                        token._parsed_in = true
                    end
                elseif t[1] == "TOKEN_SYNTAX" then
                    assert(tokens:next()[2] == ",")
                else
                    p(tokens:next())
                    error "?"
                end
            elseif not token._parsed_from then
                token._parsed_from = true
                token.from = tokens:next()[2]
                self.statusStack:swap("TOKEN_FOR_END")
                self.statusStack:push("TOKEN_INNER")
            else
                p(tokens:peek())
                error "how?!"
            end
        elseif self.statusStack:is("TOKEN_FOR_END") then
            assert(token.type == "TOKEN_FOR", "Unexpected endfor: "..token.type)
            tokens:skip()
            self:endToken()
        else
            p(self.tree)
            p("INVALID STATUS", self.statusStack:peek(), tokens:next())
            error ""
        end
    end
end

local LuaCompiler = Object:extend()

function LuaCompiler:initialize(options)
    self.output = {}
    self.options = options or {
        writeHeader = true
    }
    if self.options.writeHeader then
        self:add(("return { source=%q, run = function(ichtRE)"):format("TODO" --TODO: implement
        ))
    end
    self.locals = {}
end

function LuaCompiler:add(i)
    self.output[#self.output+1] = i
end

function LuaCompiler:formatArgument(value, isString)
    if type(value) == "cdata" then
        value = value:toString()
        isString = true
    end
    if isString then
        value = ("%q"):format(value):gsub("\\\n", "\\n")
    else
        p(value, value:gsub("^(.-)[.]", "\\1"))
        if not self.locals[value:gsub("^(.-)[.]", "\\1")] then
            p(self.locals)
            value = "ichtRE."..value
        end
    end
    return value
end

function LuaCompiler:compileBlob(instance)
    self:add(("ichtRE.write %s"):format(self:formatArgument(instance.value, true)))
end

function LuaCompiler:compileString(instance)
    self:add(self:formatArgument(instance.value, true))
end

function LuaCompiler:compileExtends(instance)
    self:add(("ichtRE.extends %s"):format(self:formatArgument(instance.path)))
end

function LuaCompiler:compileBlock(instance)
    self:add(("ichtRE._block(%q, function(BLOCK_NAME, PARENT)"):format(instance.path))
    self:compileChildren(instance)
    self:add("end)")
end

function LuaCompiler:compileFor(instance)
    self:add(("ichtRE._for(%s, function(%s)"):format(self:formatArgument(instance.from), table.concat(instance.unpack, ", ")))
    self.locals = { __index = self.locals }
    for k, v in pairs(instance.unpack) do
        self.locals[v] = true
    end
    setmetatable(self.locals, self.locals)
    self:compileChildren(instance)
    self.locals = self.locals.__index
    self:add(("end, %s)"):format(#instance.unpack > 1 and "true" or "false"))
end

function LuaCompiler:compileChildren(tree)
    for k, v in pairs(tree.children) do
        self:compile(v)
    end
end

function LuaCompiler:compileEcho(tree)
    self:add("ichtRE.write (")
    for k, child in pairs(tree.children) do
        self:compile(child)
        self:add(", ")
    end
    self:add("nil)")
end

function LuaCompiler:compileCall(tree)
    local didOpen = false
    for k, child in pairs(tree.children) do
        if not didOpen then
            self:compileWord(child)
            self:add("(")
            didOpen = true
        else
            self:compile(child)
            self:add(",")
        end
    end
    if didOpen then
        self:add("nil)")
    end
end

function LuaCompiler:compileWord(tree)
    if tree.value then
        self:add(self:formatArgument(tree.value, false))
    else
        p("No value?!", tree)
    end
end

function LuaCompiler:compile(tree)
    if tree.type == "TOKEN_ROOT" then
        self:compileChildren(tree)
    elseif tree.type == "TOKEN_BLOB" then
        self:compileBlob(tree)
    elseif tree.type == "TOKEN_STRING" then
        self:compileString(tree)
    elseif tree.type == "TOKEN_EXTENDS" then
        self:compileExtends(tree)
    elseif tree.type == "TOKEN_BLOCK" then
        self:compileBlock(tree)
    elseif tree.type == "TOKEN_FOR" then
        self:compileFor(tree)
    elseif tree.type == "TOKEN_ECHO" then
        self:compileEcho(tree)
    elseif tree.type == "TOKEN_CALL" then
        self:compileCall(tree)
    elseif tree.type == "TOKEN_WORD" then
        self:compileWord(tree)
    else
        p(tree)
        error "Unkown thing to compile."
    end
end

function LuaCompiler:finish()
    if self.options.writeHeader then
        self:add("end}")
    end
end

function LuaCompiler:toString()
    return table.concat(self.output, " ")
end

function LuaCompiler:run()
    return assert(loadstring(self:toString()))
end

stream:on("data", function(chunk)
    local buffer = Buffer:new(chunk)
    local scanner = Scanner:new()
    scanner:parseChunk(buffer)
    p(scanner.tokens)
    local parser = Parser:new()
    parser:parse(TableBuffer:new(scanner.tokens))
    local maxDepth = require "luvit.utils".DUMP_MAX_DEPTH
    require "luvit.utils".DUMP_MAX_DEPTH = 100
    p(parser.tree, parser.currentToken)
    require "luvit.utils".DUMP_MAX_DEPTH = maxDepth
    
    local compiler = LuaCompiler:new()
    compiler:compile(parser.tree)
    compiler:finish()
    
    local code = compiler:toString()
    p(code)
    
    require "luvit.fs".writeFileSync("resources/lua/web/templates/test.html.tpl.lua", code)
    
    local f = compiler:run()
    p(f, f())
end)

require "luvit.timer".setInterval(1000, function() p "collectgarbage" collectgarbage() end)
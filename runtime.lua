--[[
    Icht runtime
]]

local Object = require "luvit.core".Object

local function makeScope(parent)
    return setmetatable({}, {__index=parent})
end

local Runtime = Object:extend()

function Runtime:initialize(parent)
    self.parent = parent
    self.blocks = parent and parent.blocks or {}
    self.isExtending = false
end

function Runtime:run(info, cb)
    local scope = makeScope(self)
    scope._rootscope = scope
    scope.source = info.source
    scope._blocks = {}
    info.run(scope)
    cb()
end

function Runtime:_extends(parent)
    assert(not runtime.isExtending, "Cannot extend multiple templates.")
    self._rootscope.isExtending = parent
end

function Runtime:write(...)
    if not self.isExtending then
        for k, str in pairs({...}) do
            p("OUT self:onOutput(", tostring(str))
        end
    end
end

function Runtime:_block(name, child)
    if self.isExtending then
        self._blocks[name] = function(runtime, ...) child(self, name, ...) end
    elseif self._blocks[name] then
        self._blocks[name](runtime, child)
    else
        child(self, name)
    end
end

function Runtime:_for(v, child, doUnpack)
    if type(v) == "table" then
        for k, v in pairs(v) do
            if doUnpack then
                child(unpack(v))
            else
                child(v)
            end
        end
    else
        p(v)
        error "Unkown varible to for."
    end
end

function Runtime:_runParent()
    if self.isExtending then
        self:runParent(self.isExtending, childRuntime)
    end
end

return {
    Runtime = Runtime,
}
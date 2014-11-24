local uv = require "luvit.uv"
local fs = require "luvit.fs"
local core = require "luvit.core"
local Emitter, Object, Error = core.Emitter, core.Object, core.Error

local SavedTemplate = Object:extend()

function SavedTemplate:initialize(options)
    self.vpath = options.vpath
    self.valid = options.valid -- If compiled version is newer than the source
    self.loaded = nil -- The loaded version of the template
end

function SavedTemplate:openSourceStream()
    assert(false, "Not implemented")
end

function SavedTemplate:openCompiledStream()
    assert(false, "Not implemented")
end

function SavedTemplate:openDestinationStream()
    assert(false, "Not implemented")
end

local FileSystemSavedTemplate = SavedTemplate:extend()

function FileSystemSavedTemplate:initialize(options)
    SavedTemplate.initialize(self, options)
    self.sourcePath = options.sourcePath
    self.compiledPath = options.compiledPath
end

function FileSystemSavedTemplate:toSource()
    return "file:"..self.vpath
end

function FileSystemSavedTemplate:openSourceStream(cb)
    cb(nil, fs.createReadStream(self.sourcePath))
end

function FileSystemSavedTemplate:openCompiledStream(cb)
    cb(nil, fs.createReadStream(self.compiledPath))
end

function FileSystemSavedTemplate:openDestinationStream(cb)
    cb(nil, fs.createWriteStream(self.compiledPath))
end

function FileSystemSavedTemplate:load(cb)
    local f, err = loadfile(self.compiledPath)

    if not f then
        cb(err)
    else
        cb(nil, f())
    end
end

function SavedTemplate:addHandlerType(name)
    if name == "change" and self._subscribe then
        self._subscribe(function(file)
            self:on("change", file)
        end)
        self._subscribe = false
    end
end

local Mount = Emitter:extend()

function Mount:initialize(options)
    self.path = options.path
    self.vpath = options.vpath
    self.isWritable = options.isWritable or false

-- TODO
--     if options.watch == true or (options.watch == nil and not self.isWritable) then
--         self.watcher = uv.Watcher:new(self.path)
-- 
--         self.watcher:on("change", function (event, path)
--             p("on_change", {event=event,path=path})
--             if event == "modify" then
--                 self:emit("invalidate", self:toVirtualPath(path))
--             end
--         end)
-- 
--         p(self.watcher)
--     end
end

function Mount:inMount(vpath)
    return vpath:match(self.vpath)
end

local function escapeNeedle(str)
    return str:gsub("[%(%)%.%%%+%-%*%?%[%]%^%$]", function(c) return "%" .. c end)
end

function Mount:toVirtualPath(path)
    return self.vpath..path:gsub("^"..escapeNeedle(self.path), "")
end
function Mount:toPath(vpath)
    return self.path..vpath:gsub("^"..escapeNeedle(self.vpath), "")
end

local TemplateStore = Object:extend()

function TemplateStore:findTemplate(vpath, cb)
    assert(false, "Not implemented")
end

function TemplateStore:invalidateTemplate(savedTemplate, cb)
    assert(false, "Not implemented")
end

local FilesystemTemplateStore = TemplateStore:extend()

function FilesystemTemplateStore:initialize()
    self.mounts = {}
    self.compiledExt = ".lua"
end

function FilesystemTemplateStore:mount(mount)
    self.mounts[#self.mounts+1] = mount
end

function FilesystemTemplateStore:unmount(targetMount)
    local passed = false
    for i, mount in pairs(self.mounts) do
        if mount == targetMount then
            passed = true
        end
        if passed then
            self.mounts[i] = self.mounts[i+1]
        end
    end
end

function FilesystemTemplateStore:find(vpath, shouldBeWriteable, cb)
    local found = false
    local left = 0
    local err = Error:new("Vpath was not in any mount.")
    err.vpath = vpath
    err.tried = {}

    local writables = shouldBeWriteable and {}

    for i, mount in pairs(self.mounts) do
        local isInside = mount:inMount(vpath)
        local isWritable = mount.isWritable
        local attempt = #err.tried+1

        if isInside and (not shouldBeWriteable or mount.isWritable) then
            left = left + 1
            local path = mount:toPath(vpath)
            fs.stat(path, function(err2, info)
                if found then
                    return
                end
                left = left - 1
                if err2 then
                    err.tried[attempt].error = err2
                    if writables then
                        writables[#writables+1] = path
                    end
                    if left == 0 then
                        err.message = "Path was not in any mount."
                        cb(err, writables and writables[1] or nil)
                    end
                else
                    found = true
                    cb(nil, path, mount, info)
                end
            end)
        end

        err.tried[attempt] = {
            isInside = isInside,
            isWritable = isWritable,
            mount = mount
        }
    end

    if left == 0 then
        cb(err)
    end
end

function FilesystemTemplateStore:findTemplate(vpath, cb)
    self:find(vpath .. self.compiledExt, true, function(err, compiledPath, mount, info)
        local template = FileSystemSavedTemplate:new {
            vpath = vpath,
            valid = true,
            compiledPath = compiledPath,
        }

        if err then
            self:invalidateTemplate(template, function(err2, savedTemplate)
                if err2 then
                    err2.preveous = err
                    return cb(err2)
                else
                    return cb(nil, savedTemplate)
                end
            end)
        else
            return cb(nil, template)
        end
    end)
end

function FilesystemTemplateStore:invalidateTemplate(savedTemplate, cb)
    if not savedTemplate.valid or savedTemplate.sourcePath then
        savedTemplate.valid = false
        cb(nil, savedTemplate)
    end

    self:find(savedTemplate.vpath, false, function(err, path, mount, info)
        if err then
            return cb(err)
        else
            savedTemplate.valid = false
            savedTemplate.sourcePath = path
            cb(nil, savedTemplate)
        end
    end)
end

local CachedTemplateStore = TemplateStore:extend()

function CachedTemplateStore:initialize(parent)
    self.parent = parent
    self.cache = {}
end

function CachedTemplateStore:findTemplate(vpath, cb)
    if self.cache[vpath] then
        return cb(nil, self.cache[vpath])
    else
        return self.parent:findTemplate(vpath, function(err, savedTemplate)
            savedTemplate[vpath] = not err and savedTemplate or nil
            cb(err, savedTemplate)
        end)
    end
end

function CachedTemplateStore:invalidateTemplate(savedTemplate, cb)
    self.cache[savedTemplate.vpath] = nil
    return self.parent:invalidateTemplate(savedTemplate, cb)
end

return {
    Mount = Mount,
    FilesystemTemplateStore = FilesystemTemplateStore,
    CachedTemplateStore = CachedTemplateStore,
}
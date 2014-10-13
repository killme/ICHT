--[[
    Icht runtime
]]

local function makeRuntime (parent)
    local runtime = {__index = parent}
    setmetatable(runtime, runtime)
    runtime._blocks = runtime._blocks or {}
    runtime.isExtending = false

    runtime._extends = function(parent)
        assert(not runtime.isExtending, "Cannot extend multiple templates.")
        runtime.isExtending = parent
    end
    runtime.write = function(str)
        if not runtime.isExtending then
            runtime:onOutput(tostring(str))
        end
    end
    runtime._block = function(name, child)
        if runtime.isExtending then
            runtime._blocks[name] = function(runtime, ...) child(runtime, name, ...) end
        elseif runtime._blocks[name] then
            runtime._blocks[name](runtime, child)
        end
    end
    runtime._for = function(v, child, doUnpack)
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
    runtime._runParent = function()
        if runtime.isExtending then
            local childRuntime = makeRuntime(runtime)
            runtime:runParent(runtime.isExtending, childRuntime)
        end
    end

    return runtime
end

return {
    makeRuntime = makeRuntime
}
local M = {}

M.set = function(mode, lhs, rhs, opts)
    if not opts then
        opts = { silent = true }
    end
    return vim.keymap.set(mode, lhs, rhs, opts)
end

M.map = function(lhs, rhs, opts)
    return M.set("", lhs, rhs, opts)
end

M.nmap = function(lhs, rhs, opts)
    return M.set("n", lhs, rhs, opts)
end

M.vmap = function(lhs, rhs, opts)
    return M.set("v", lhs, rhs, opts)
end

M.xmap = function(lhs, rhs, opts)
    return M.set("x", lhs, rhs, opts)
end

M.del = function(mode, lhs, opts)
    return vim.keymap.del(mode, lhs, opts)
end

M.unmap = function(lhs, opts)
    return M.del("", lhs, opts)
end

M.nunmap = function(lhs, opts)
    return M.del("n", lhs, opts)
end

M.lazy = function(func, ...)
    local vals = { ... }
    return function()
        return pcall(func, unpack(vals))
    end
end

return M

local M = {}

M.hi = function(name, val)
    vim.api.nvim_set_hl(0, name, val)
end

M.keymap = function(mode, lhs, rhs, opts)
    if opts == nil then
        opts = { silent = true }
    end

    vim.keymap.set(mode, lhs, rhs, opts)
end

M.map = function(lhs, rhs, opts)
    M.keymap("", lhs, rhs, opts)
end

M.nmap = function(lhs, rhs, opts)
    M.keymap("n", lhs, rhs, opts)
end

M.vmap = function(lhs, rhs, opts)
    M.keymap("v", lhs, rhs, opts)
end

M.xmap = function(lhs, rhs, opts)
    M.keymap("x", lhs, rhs, opts)
end

M.new_cmd = function(name, command, opts)
    if opts == nil then
        opts = {}
    end
    return vim.api.nvim_create_user_command(name, command, opts)
end

M.load_cfg = function(name)
    return require("vdk.plugins.configs." .. name)
end

return M

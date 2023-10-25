_G.kit = {}

kit.augroup = function(name, opts)
  return vim.api.nvim_create_augroup(name, opts or {})
end

kit.autocmd = function(event, opts)
  return vim.api.nvim_create_autocmd(event, opts or {})
end

kit.create_cmd = function(name, command, opts)
  if opts == nil then
    opts = {}
  end
  return vim.api.nvim_create_user_command(name, command, opts)
end

kit.call_at_ft = function(ft, callback)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = ft,
    callback = callback,
  })
end

kit.set_hl = function(name, value)
  return vim.api.nvim_set_hl(0, name, value)
end

kit.wrap = function(fn, ...)
  local args = { ... }
  return function()
    return fn(unpack(args))
  end
end

kit.equal = function(lhs, rhs)
  return lhs == rhs
end

kit.not_equal = function(lhs, rhs)
  return not kit.equal(lhs, rhs)
end

kit.any_of = function(lhs, cmp, ...)
  for _, val in ipairs({ ... }) do
    if cmp(lhs, val) then
      return true
    end
  end
  return false
end

kit.require_on_index = function(require_path)
  return setmetatable({}, {
    __index = function(_, key)
      return require(require_path)[key]
    end,

    __newindex = function(_, key, value)
      require(require_path)[key] = value
    end,
  })
end

kit.require_on_module_call = function(require_path)
  return setmetatable({}, {
    __call = function(_, ...)
      return require(require_path)(...)
    end,
  })
end

kit.require_on_exported_call = function(require_path)
  return setmetatable({}, {
    __index = function(_, k)
      return function(...)
        return require(require_path)[k](...)
      end
    end,
  })
end

return _G.kit

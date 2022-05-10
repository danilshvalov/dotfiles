-- :fennel:1651221107
local bufline = require("bufferline")
local _local_1_ = bufline
local bufcycle = _local_1_["cycle"]
local bufmove = _local_1_["move"]
local go_to_buffer = _local_1_["go_to_buffer"]
local _local_2_ = require("bufdelete")
local bufdelete = _local_2_["bufdelete"]
local _local_3_ = require("floppy.utils")
local nmap = _local_3_["nmap"]
local api = vim.api
local function close_buffer(id)
  local res, _ = pcall(vim.cmd, ("bd" .. tostring(id)))
  if not res then
    return print("Cannot close buffer:", api.nvim_buf_get_name(id))
  else
    return nil
  end
end
local function is_valid_buf(buf)
  return (api.nvim_buf_is_valid(buf) and vim.bo[buf].buflisted)
end
local function valid_buffers()
  return vim.tbl_filter(is_valid_buf, api.nvim_list_bufs())
end
local function close_others()
  local bufnr = api.nvim_get_current_buf()
  local buffers = valid_buffers()
  if (#buffers > 1) then
    for _, id in pairs(buffers) do
      if (id ~= bufnr) then
        close_buffer(id)
      else
      end
    end
    return nil
  else
    return nil
  end
end
local function setup_mappings()
  local function _7_(...)
    return bufdelete(0, false, ...)
  end
  nmap("<Leader>q", _7_)
  local function _8_(...)
    return bufcycle(-1, ...)
  end
  nmap("<C-j>", _8_)
  local function _9_(...)
    return bufcycle(1, ...)
  end
  nmap("<C-k>", _9_)
  local function _10_(...)
    return bufmove(-1, ...)
  end
  nmap("<C-A-j>", _10_)
  local function _11_(...)
    return bufmove(1, ...)
  end
  nmap("<C-A-k>", _11_)
  local function _12_(...)
    return close_others(...)
  end
  nmap("<Leader>co", _12_)
  for n = 1, 9 do
    local function _13_(...)
      return go_to_buffer(n, ...)
    end
    nmap(("<Leader>" .. tostring(n)), _13_)
  end
  return nil
end
local function setup_bufferline()
  local function _14_(count, _, _0, _1)
    return tostring(count)
  end
  local function _15_(opts)
    return opts.ordinal
  end
  return bufline.setup({options = {show_buffer_icons = false, show_close_icon = false, diagnostics = "nvim_lsp", diagnostics_update_in_insert = true, diagnostics_indicator = _14_, numbers = _15_, tab_size = 0}})
end
local function setup()
  setup_bufferline()
  return setup_mappings()
end
return {setup = setup}
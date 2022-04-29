-- :fennel:1651221107
local null_ls = require("null-ls")
local api = vim.api
local function is_buffer_ready(bufnr)
  return (api.nvim_buf_is_loaded(bufnr) and not api.nvim_buf_get_option(bufnr, "modified"))
end
local function error_notify(kind, err)
  local message
  local function _1_()
    return false
  end
  message = ((_1_() and err) or err.message)
  return vim.notify((kind .. ": " .. message), vim.log.levels.WARN)
end
local function update_buffer(bufnr, res)
  vim.lsp.util.apply_text_edits(res, bufnr, "utf-16")
  local function _2_()
    return vim.cmd("silent noautocmd update")
  end
  return api.nvim_buf_call(bufnr, _2_)
end
local function format_callback(bufnr, err, res)
  if (is_buffer_ready(bufnr) and res) then
    return update_buffer(bufnr, res)
  else
    return nil
  end
end
local function parse_buffer_id(bufnr)
  local function _4_()
    return nil
  end
  return (_4_() or api.nvim_get_current_buf())
end
local function apply_format(bufnr)
  local bufnr0 = parse_buffer_id(bufnr)
  local function _5_(...)
    return format_callback(bufnr0, ...)
  end
  return vim.lsp.buf_request(bufnr0, "textDocument/formatting", {textDocument = {uri = vim.uri_from_bufnr(bufnr0)}}, _5_)
end
local enable_format = true
local function toggle_format()
  enable_format = not enable_format
  return nil
end
api.nvim_create_user_command("ToggleFormat", toggle_format, {})
vim.keymap.set("", "<Leader>tf", toggle_format, {silent = true})
local function make_sources()
  local builtins = null_ls.builtins
  local formatting = builtins.formatting
  return {builtins.code_actions.gitsigns, builtins.diagnostics.eslint, formatting.stylua, formatting.prettier.with({filetypes = {"java", "markdown"}}), formatting.prettierd.with({filetypes = {"json", "javascript"}}), formatting.clang_format, formatting.black, formatting.latexindent.with({extra_args = {"-g /dev/null"}}), formatting.taplo, formatting.fnlfmt}
end
local function format_file()
  if enable_format then
    return vim.lsp.buf.formatting()
  else
    return nil
  end
end
local function on_attach(client, bufnr)
  if client.supports_method("textDocument/formatting") then
    local group_id = api.nvim_create_augroup("LspFormatting", {})
    return api.nvim_create_autocmd({"BufWritePost"}, {group = group_id, callback = format_file})
  else
    return nil
  end
end
local function setup()
  return null_ls.setup({sources = make_sources(), on_attach = on_attach})
end
return {setup = setup}
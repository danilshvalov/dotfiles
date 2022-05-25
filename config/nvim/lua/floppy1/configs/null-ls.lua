-- :fennel:1652561830
local null_ls = require("null-ls")
local api = vim.api
local enable_format = true
local function toggle_format()
  enable_format = not enable_format
  return nil
end
vim.api.nvim_create_user_command("ToggleFormat", toggle_format, {force = true})
vim.keymap.set("", "<Leader>tf", toggle_format, {silent = true})
local function make_sources()
  local builtins = null_ls.builtins
  local formatting = builtins.formatting
  return {builtins.code_actions.gitsigns, builtins.diagnostics.eslint, builtins.diagnostics.codespell, formatting.stylua, formatting.prettier.with({filetypes = {"java", "markdown"}}), formatting.prettierd.with({filetypes = {"json", "javascript", "yaml"}}), formatting.clang_format, formatting.black, formatting.latexindent.with({extra_args = {"-g /dev/null"}}), formatting.taplo}
end
local function lsp_formatting(bufnr)
  return vim.lsp.buf.format({async = true})
end
local function format_file()
  if enable_format then
    return lsp_formatting()
  else
    return nil
  end
end
local group_id = api.nvim_create_augroup("LspFormatting", {})
local function on_attach(client, bufnr)
  if client.supports_method("textDocument/formatting") then
    vim.keymap.set("", "<F3>", lsp_formatting, {silent = true})
    api.nvim_clear_autocmds({group = group_id, buffer = bufnr})
    return api.nvim_create_autocmd({"BufWritePost"}, {group = group_id, callback = format_file, buffer = bufnr})
  else
    return nil
  end
end
local function setup()
  return null_ls.setup({sources = make_sources(), on_attach = on_attach})
end
return {setup = setup}
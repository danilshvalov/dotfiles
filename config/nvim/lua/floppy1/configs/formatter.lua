require("formatter").setup({
    filetype = {
        java = {
            -- prettier
            function()
                return {
                    exe = "prettier",
                    args = { "--stdin-filepath", vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)) },
                    stdin = true,
                }
            end,
        },
        typescriptreact = {
            -- prettier
            function()
                return {
                    exe = "prettier",
                    args = { "--stdin-filepath", vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)) },
                    stdin = true,
                }
            end,
        },
        json = {
            -- prettier
            function()
                return {
                    exe = "prettier",
                    args = { "--stdin-filepath", vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)) },
                    stdin = true,
                }
            end,
        },
        lua = {
            function()
                return {
                    exe = "stylua",
                    -- args = { "--config-path", vim.fn.expand("~/stylua.toml"), "-" },
                    args = { "-s", "-" },
                    stdin = true,
                }
            end,
        },
        python = {
            function()
                return {
                    exe = "python3 -m black",
                    args = {
                        "--line-length",
                        79,
                        "--stdin-filename",
                        vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)),
                        "-",
                    },
                    stdin = true,
                }
            end,
        },
        cpp = {
            function()
                return {
                    exe = "clang-format",
                    args = {
                        "--assume-filename",
                        vim.api.nvim_buf_get_name(0),
                        "--fallback-style=Chromium",
                    },
                    stdin = true,
                    cwd = vim.fn.expand("%:p:h"),
                }
            end,
        },
        tex = {
            function()
                return {
                    exe = "latexindent",
                    args = {
                        "-",
                    },
                    stdin = true,
                }
            end,
        },
        rust = {
            function()
                return {
                    exe = "rustfmt",
                    stdin = true,
                }
            end,
        },
        toml = {
            -- prettier
            function()
                return {
                    exe = "taplo",
                    args = { "fmt", "-" },
                    stdin = true,
                }
            end,
        },
        markdown = {
            function()
                return {
                    exe = "prettier",
                    args = { "--stdin-filepath", vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)) },
                    stdin = true,
                }
            end,
        },
    },
})

-- silent format
require("formatter.util").print = function() end

local au = require("au")
local should_format = true

vim.api.nvim_exec(
    [[
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | FormatWrite
augroup END
]],
    true
)

-- au.group("FormatAutogroup", {
--     {
--         "BufWritePre",
--         "* undojoin | FormatWrite",
--         ""
--         -- "*.java,*.lua,*.py,*.cpp,*.h,*.tex,*.rs,*.toml,*.json,*.md",
--         -- function()
--         --     if should_format then
--         --         vim.api.nvim_command("FormatWrite")
--         --     end
--         -- end,
--     },
-- })

vim.api.nvim_add_user_command("ToggleFormat", function()
    should_format = not should_format
end, {})

vim.api.nvim_set_keymap("", "<F3>", "<Cmd>Format<CR>", { silent = true, noremap = true })

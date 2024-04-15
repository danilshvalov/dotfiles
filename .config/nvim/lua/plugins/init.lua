local function is_local_session()
  return vim.env["SSH_TTY"] == nil
end

return {
  {
    "Wansmer/langmapper.nvim",
    config = function()
      local langmapper = require("langmapper")

      langmapper.setup({
        hack_keymap = true,
      })
      langmapper.hack_get_keymap()

      local function escape(str)
        local escape_chars = [[;,."|\]]
        return vim.fn.escape(str, escape_chars)
      end

      local en = [[`qwertyuiop[]asdfghjkl;'zxcvbnm]]
      local ru = [[ёйцукенгшщзхъфывапролджэячсмить]]
      local en_shift = [[~QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>]]
      local ru_shift = [[ËЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ]]

      vim.opt.langmap = vim.fn.join({
        escape(ru_shift) .. ";" .. escape(en_shift),
        escape(ru) .. ";" .. escape(en),
      }, ",")
    end,
  },
  {
    "danilshvalov/keymap.nvim",
    dependencies = { "anuvyklack/keymap-amend.nvim" },
    keymap = function(map)
      map:set("<leader>l", vim.cmd.Lazy)

      map:mode("i"):set("<C-ч><C-г>", "<C-x><C-u>")
      map:mode("i"):set("<C-k>", "<C-p>"):set("<C-j>", "<C-n>")

      -- see https://vi.stackexchange.com/questions/5605/how-to-fix-cmap-breaking-cabbrev
      map:mode("c"):set("<CR>", "<C-]><CR>")

      map:set("х", "[")

      map:set("Y", "y$")
      map:mode({ "i", "n", "t" }):set("<A-j>", vim.cmd.bnext):set("<A-k>", vim.cmd.bprev)
      map:set("<C-b>", "<Cmd>b#<CR>")

      map:ft("help"):set("q", vim.cmd.bd, { nowait = true })
      map:mode("nv"):set("<Space>", "<Leader>", { remap = true })
      map:set("<C-g>", "2<C-g>")

      map
        :mode("nvo")
        :set("H", "^", { desc = "Start of line" })
        :set("L", "$", { desc = "End of line" })

      map:prefix("<leader>t", "+toggle"):set("w", function()
        if not vim.opt_local.formatoptions:get().a then
          vim.opt_local.formatoptions:append("a")
        else
          vim.opt_local.formatoptions:remove("a")
        end
      end, { desc = "Toggle wrap" })

      map
        :new({ mode = "nv", expr = true })
        :set("k", "(v:count? 'k' : 'gk')")
        :set("j", "(v:count? 'j' : 'gj')")

      map
        :prefix("<leader>f")
        :set(".", kit.wrap(vim.fn.system, "open ."), { desc = "Open in finder" })

      map
        :prefix("<leader>d", "+dir")
        :set("c", function()
          local path, _ = vim.fn.expand("%:p:h"):gsub("oil://", "")
          vim.cmd.cd(path)
          vim.notify("Directory: " .. vim.fn.expand("%:p:~:h"):gsub("oil://", ""))
        end, { desc = "Set cwd to current file directory" })
        :set("y", function()
          vim.fn.setreg("+", vim.fn.expand("%:p:h"))
          vim.notify("Copied directory: " .. vim.fn.expand("%:p:~:h"))
        end, { desc = "Yank cwd" })

      map:set("<Esc>", vim.cmd.noh)
    end,
    init = function()
      _G.map = setmetatable({}, {
        __index = function(_, key)
          return require("keymap").map[key]
        end,
      })
    end,
  },
  {
    "ggandor/leap.nvim",
    dependencies = "tpope/vim-repeat",
    keymap = function(map)
      map:mode("nvo"):set("s", function()
        require("leap").leap({ target_windows = { vim.fn.win_getid() } })
      end)
    end,
    config = function()
      require("leap.util")["get-input"] = function()
        local ok, ch = pcall(vim.fn.getcharstr)
        if ok and ch ~= vim.api.nvim_replace_termcodes("<esc>", true, false, true) then
          if #ch > 1 then
            return require("langmapper.utils").translate_keycode(ch, "default", "ru")
          else
            return ch
          end
        end
      end
    end,
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = { "mfussenegger/nvim-jdtls", "folke/neodev.nvim" },
    config = function()
      local lspconfig = require("lspconfig")

      local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
      for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
      end

      local disable_format = function(client)
        local caps = client.server_capabilities
        caps.document_formatting = false
        caps.document_range_formatting = false
        caps.documentFormattingProvider = false
      end

      vim.diagnostic.config({
        virtual_text = true,
        signs = true,
        underline = true,
        update_in_insert = false,
        severity_sort = false,
      })

      local caps = vim.lsp.protocol.make_client_capabilities()
      caps.offsetEncoding = { "utf-16" }

      lspconfig.clangd.setup({
        cmd = {
          "clangd",
          "--background-index",
          "-j=8",
          "--completion-style=detailed",
          "--clang-tidy",
          "--all-scopes-completion",
        },
        capabilities = caps,
        on_attach = disable_format,
      })

      lspconfig.csharp_ls.setup({ on_attach = disable_format })

      lspconfig.texlab.setup({ on_attach = disable_format })
      lspconfig.clojure_lsp.setup({})
      lspconfig.cmake.setup({ on_attach = disable_format })
      lspconfig.pylsp.setup({})

      lspconfig.lua_ls.setup({
        settings = {
          Lua = {
            runtime = {
              version = "LuaJIT",
            },
            diagnostics = {
              globals = { "vim", "kit" },
            },
            telemetry = {
              enable = false,
            },
            hint = {
              enable = true,
            },
            completion = {
              callSnippet = "Replace",
            },
            workspace = {
              checkThirdParty = false,
            },
          },
        },
      })

      lspconfig.rust_analyzer.setup({
        settings = {
          ["rust-analyzer"] = {
            checkOnSave = {
              enabled = true,
              command = "clippy",
            },
          },
        },
      })

      lspconfig.marksman.setup({})
      lspconfig.sqlls.setup({})
    end,
  },
  {
    "nvim-tree/nvim-web-devicons",
    config = function()
      require("nvim-web-devicons").setup({
        default = true,
        color_icons = true,
      })
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      local treesitter = require("nvim-treesitter.configs")

      treesitter.setup({
        ensure_installed = {
          "cpp",
          "python",
          "json",
          "http",
          "rust",
          "java",
          "lua",
          "html",
          "latex",
          "org",
          "markdown",
          "markdown_inline",
        },
        highlight = {
          enable = true,
          disable = { "yaml" },
          additional_vim_regex_highlighting = { "org", "cpp" },
        },
        indent = {
          enable = true,
          disable = { "python", "java", "yaml", "sql", "latex", "markdown" },
        },
        autopairs = {
          enable = true,
        },
      })
    end,
  },
  {
    "stevearc/conform.nvim",
    config = function()
      local conform = require("conform")

      local black
      if vim.fn.executable("taxi-black") == 1 then
        black = "taxi_format"
      else
        black = "black"
      end

      local clang_format
      if vim.fn.executable("taxi-format") == 1 then
        clang_format = "taxi_format"
      else
        clang_format = "clang-format"
      end

      conform.setup({
        formatters = {
          taxi_format = {
            command = "taxi-format",
            args = {
              "--quiet",
              "--force",
              "$FILENAME",
              "-",
            },
            stdin = false,
          },
          black = {
            prepend_args = {"--line-length", "79"}
          },
        },
        formatters_by_ft = {
          lua = { "stylua" },
          python = { black },
          markdown = { "prettier" },
          cpp = { clang_format },
          tex = { "latexindent" },
          html = { "prettier" },
          json = { "prettier" },
        },
      })

      local function format_buffer()
        local bufnr = vim.api.nvim_get_current_buf()
        conform.format({
          async = true,
          bufnr = bufnr,
          timeout_ms = 5000,
          lsp_fallback = true,
        }, function(error)
          if not error then
            vim.api.nvim_buf_call(bufnr, function()
              vim.cmd.write()
            end)
          end
        end)
      end

      map:set("<leader>cf", format_buffer)
    end,
  },
  -- {
  --   "nvimtools/none-ls.nvim",
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --   },
  --   keymap = function(map)
  --     map:set("<leader>cf", function()
  --       vim.lsp.buf.format({ async = true, timeout_ms = 5000 })
  --     end)
  --   end,
  --   config = function()
  --     local null_ls = require("null-ls")
  --     local builtins = null_ls.builtins
  --     local formatting = builtins.formatting
  --     local helpers = require("null-ls.helpers")

  --     local black
  --     if vim.fn.executable("taxi-black") == 1 then
  --       black = helpers.make_builtin({
  --         name = "taxi-black",
  --         method = { null_ls.methods.FORMATTING },
  --         filetypes = { "python" },
  --         generator_opts = {
  --           command = "taxi-black",
  --           to_temp_file = true,
  --           args = function()
  --             return {
  --               "--quiet",
  --               "--force",
  --               "$FILENAME",
  --               "-",
  --             }
  --           end,
  --         },
  --         factory = helpers.formatter_factory,
  --       })
  --     else
  --       black = formatting.black.with({
  --         extra_args = { "--line-length", "80" },
  --       })
  --     end

  --     local clang_format
  --     if vim.fn.executable("taxi-clang-format") == 1 then
  --       clang_format = helpers.make_builtin({
  --         name = "taxi-clang-format",
  --         method = { null_ls.methods.FORMATTING },
  --         filetypes = { "cpp" },
  --         generator_opts = {
  --           command = "taxi-clang-format",
  --           to_temp_file = true,
  --           args = function()
  --             return {
  --               "--quiet",
  --               "--force",
  --               "$FILENAME",
  --               "-",
  --             }
  --           end,
  --         },
  --         factory = helpers.formatter_factory,
  --       })
  --     else
  --       clang_format = formatting.clang_format
  --     end

  --     null_ls.setup({
  --       sources = {
  --         formatting.stylua,
  --         -- formatting.mdformat,
  --         formatting.prettier.with({ filetypes = { "java", "css", "html", "markdown" } }),
  --         formatting.prettierd.with({
  --           filetypes = { "json", "javascript", "markdown", "scss" },
  --         }),
  --         formatting.phpcsfixer,
  --         clang_format,
  --         black,
  --         -- TODO: replace with texlab
  --         -- formatting.latexindent.with({
  --         --   extra_args = { "-g", "/dev/null", "--local" },
  --         -- }),
  --         formatting.fnlfmt,
  --         formatting.csharpier,
  --         null_ls.builtins.diagnostics.sqlfluff.with({
  --           extra_args = { "--dialect", "postgres" },
  --         }),
  --         formatting.sqlfluff.with({
  --           extra_args = { "--dialect", "postgres" },
  --         }),
  --       },
  --     })
  --   end,
  -- },
  {
    "nvim-lualine/lualine.nvim",
    config = function()
      local lualine = require("lualine")
      local colors = require("tokyonight.colors").setup()

      local symbols = {
        modified = "**",
        readwrite = "RW",
        readonly = "RO",
        terminal = "TERM",
        fzf = "FZF",
      }

      local function modified_color()
        return {
          fg = colors.black,
          bg = vim.bo.modified and colors.yellow or colors.blue,
        }
      end

      local line_sections = {
        lualine_a = {
          {
            function()
              if vim.bo.filetype == "toggleterm" then
                return symbols.terminal
              elseif vim.bo.filetype == "fzf" then
                return symbols.fzf
              elseif vim.bo.modified then
                return symbols.modified
              elseif vim.bo.modifiable == false or vim.bo.readonly == true then
                return symbols.readonly
              else
                return symbols.readwrite
              end
            end,
            color = modified_color,
          },
        },
        lualine_b = {},
        lualine_c = {
          {
            "diagnostics",
            draw_empty = true,
            fmt = function(str)
              local result = vim.split(str, " ", { plain = true, trimempty = true })
              for _ = 1, 4 - #result / 2 do
                table.insert(result, "   ")
              end
              return table.concat(result, " ")
            end,
          },
          "%=",
          {
            "filename",
            file_status = false,
            path = 3,
            fmt = function(str)
              if
                str == "[No Name]" or vim.tbl_contains({ "toggleterm", "fzf" }, vim.bo.filetype)
              then
                return ""
              end
              return str
            end,
          },
          function()
            return string.rep(" ", 12)
          end,
        },
        lualine_x = {},
        lualine_y = {},
        lualine_z = {
          {
            function()
              local line = vim.fn.line(".")
              local col = vim.fn.virtcol(".")
              return string.format("%d:%d", line, col)
            end,
            color = modified_color,
          },
        },
      }

      vim.o.showmode = false

      lualine.setup({
        options = {
          theme = "auto",
          globalstatus = true,
          component_separators = "",
          section_separators = "",
        },
        sections = line_sections,
      })
    end,
  },
  {
    "windwp/nvim-autopairs",
    config = function()
      local npairs = require("nvim-autopairs")
      local cond = require("nvim-autopairs.conds")
      local Rule = require("nvim-autopairs.rule")

      local ignore_regexp = "[%w%.А-Я%а-яЁё%(%[%{%'%\"]"

      npairs.setup({
        enable_check_bracket_line = false,
        fast_wrap = {
          map = "<A-x>",
          pattern = string.gsub("[%'%\"%)%>%]%)%}%%s%^]", "%s+", ""),
        },
        ignored_next_char = string.gsub(ignore_regexp, "%s+", ""),
        disable_filetype = {},
      })

      npairs.add_rule(Rule("/*", "*/"))
      npairs.add_rule(Rule('r#"', '"#', "rust"))

      npairs.add_rule(Rule('"', "", "tex"))

      npairs.add_rule(Rule("~", "~", "org"):with_move(cond.done))

      npairs.add_rule(Rule("<<", ">>", "tex"))

      local function is_cpp_class(opts)
        return opts.line:match("struct%s+") ~= nil or opts.line:match("class%s+") ~= nil
      end

      npairs.get_rule("{"):replace_endpair(function(opts)
        if is_cpp_class(opts) then
          return "};"
        else
          return "}"
        end
      end)

      npairs.add_rules({
        Rule("<", ">"):with_pair(cond.before_regex("%w+")):with_move(function(opts)
          return opts.char == ">"
        end),
      })

      local big_pairs = {
        { "[", "]" },
        { "{", "}" },
        { "(", ")" },
        { "|", "|" },
      }

      local big_kinds = { "", "big", "bigg", "Big", "Bigg" }

      for _, kind in ipairs(big_kinds) do
        for _, pair in pairs(big_pairs) do
          npairs.add_rule(
            Rule(
              string.format("\\%s%s", kind, pair[1]),
              string.format("\\%s%s", kind, pair[2]),
              "tex"
            ):with_move(cond.done())
          )
        end
      end
    end,
  },
  {
    "folke/trouble.nvim",
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function()
      local trouble = kit.require_on_exported_call("trouble")

      local open = function(name)
        return kit.wrap(trouble.open, name)
      end

      map
        :prefix("<leader>x")
        :set("q", open("quickfix"))
        :set("x", open("workspace_diagnostics"), { desc = "Show code errors" })

      map
        :new({ prefix = "<leader>c" })
        :set("r", vim.lsp.buf.rename)
        :set("h", vim.diagnostic.open_float)
        :set("a", vim.lsp.buf.code_action)

      map
        :prefix("g", "+goto")
        :set("d", open("lsp_definitions"), { desc = "Go definitions" })
        :set("D", vim.lsp.buf.declaration, { desc = "Go declaration" })
        :set("r", open("lsp_references"), { desc = "Go references" })
        :set("i", open("lsp_implementations"), { desc = "Go implementations" })
        :set("n", vim.diagnostic.goto_next, { desc = "Go next error" })
        :set("p", vim.diagnostic.goto_prev, { desc = "Go prev error" })

      require("trouble").setup()
    end,
  },
  { "akinsho/git-conflict.nvim" },
  { "folke/neodev.nvim" },
  { "windwp/nvim-ts-autotag" },
  {
    "aklt/plantuml-syntax",
    ft = "plantuml",
  },
  {
    "folke/todo-comments.nvim",
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      signs = true,
      sign_priority = 1000,
      gui_style = {
        fg = "BOLD",
      },
      highlight = {
        comments_only = false,
        keyword = "fg",
        after = "",
        pattern = [[.*<(KEYWORDS)(\(.*\))?[: ]\s*]],
        exclude = { "log", "org" },
      },
      search = {
        pattern = [[\b(KEYWORDS)(\(.*\))?[: ]\s*]],
      },
    },
  },
  {
    "uga-rosa/ccc.nvim",
    opts = {
      highlighter = {
        auto_enable = true,
        lsp = true,
        excludes = { "git", "" },
      },
    },
  },
  {
    "AckslD/nvim-FeMaco.lua",
    ft = { "markdown" },
    config = function()
      local femaco = require("femaco")
      local edit_code_block = require("femaco.edit").edit_code_block

      femaco.setup({
        prepare_buffer = function()
          vim.cmd.split()
          local buf = vim.api.nvim_create_buf(false, false)
          vim.api.nvim_win_set_buf(0, buf)
          return buf
        end,
        ensure_newline = function(base_filetype)
          return base_filetype == "markdown"
        end,
        create_tmp_filepath = function(filetype)
          return "tmp." .. filetype
        end,
      })

      map:prefix("<leader>c", "+code"):set("e", edit_code_block, { desc = "Edit code block" })
    end,
  },
  {
    "akinsho/toggleterm.nvim",
    lazy = true,
    keymap = function(map)
      local toggleterm = kit.require_on_exported_call("toggleterm")

      map
        :prefix("<leader>t", "+toggle")
        :set("t", function()
          toggleterm.toggle(vim.v.count, nil, nil, "horizontal")
        end)
        :set("T", function()
          toggleterm.toggle(vim.v.count, nil, nil, "tab")
        end)

      map:ft("toggleterm"):mode("t"):set("<Esc><Esc>", "<C-\\><C-n>")
    end,
    config = function()
      local toggleterm = require("toggleterm")
      local Terminal = require("toggleterm.terminal").Terminal

      toggleterm.setup({
        shade_terminals = false,
        persist_mode = false,
        persist_size = false,
        auto_scroll = false,
        start_in_insert = false,
        on_open = function()
          vim.cmd.startinsert()
        end,
      })

      local origin_set_ft_options = Terminal.__set_ft_options
      function Terminal:__set_ft_options()
        origin_set_ft_options(self)
        vim.bo.buflisted = true
      end
    end,
  },
  {
    "kylechui/nvim-surround",
    config = function()
      local get_input = function(prompt, default)
        local ok, result =
          pcall(vim.fn.input, { prompt = prompt, default = default, cancelreturn = vim.NIL })
        if ok and result ~= vim.NIL then
          return result
        end
      end

      local utils = require("nvim-surround.config")

      local function get_env_selections()
        local poses = utils.get_selections({
          char = "e",
          pattern = "\\begin{(%w-)}().-\\end{(%w-)}()",
        })

        if poses then
          poses.left.first_pos[2] = poses.left.first_pos[2] - 1
          poses.left.last_pos[2] = poses.left.last_pos[2] - 1
          poses.right.first_pos[2] = poses.right.first_pos[2] - 1
          poses.right.last_pos[2] = poses.right.last_pos[2] - 1
          return poses
        end
      end

      require("nvim-surround").setup({
        surrounds = {
          ["e"] = {
            find = "\\begin{%w-}.-\\end{%w-}",
            change = {
              target = get_env_selections,
              replacement = function()
                local poses = get_env_selections()
                local old_env = vim.api.nvim_buf_get_text(
                  0,
                  poses.left.first_pos[1] - 1,
                  poses.left.first_pos[2] - 1,
                  poses.left.last_pos[1] - 1,
                  poses.left.last_pos[2],
                  {}
                )[1]

                local element = get_input("New environment: ", old_env)
                if element then
                  return { { element }, { element } }
                end
              end,
            },
          },
        },
      })
    end,
  },
  {
    "echasnovski/mini.nvim",
    config = function()
      require("mini.align").setup()

      require("mini.comment").setup({
        options = {
          ignore_blank_line = true,
        },
      })

      local bufremove = require("mini.bufremove")
      bufremove.setup()
      map:set("ZX", bufremove.delete)
    end,
  },
  {
    "folke/tokyonight.nvim",
    priority = 1000,
    config = function()
      local util = require("tokyonight.util")

      require("tokyonight").setup({
        styles = {
          comments = { italic = false },
          keywords = { italic = false },
        },
        on_colors = function(colors)
          colors.comment = util.lighten(colors.comment, 0.85)
        end,
        on_highlights = function(highlights, colors)
          highlights.WinSeparator = { fg = colors.yellow }
          highlights["@type.builtin"] = highlights.Type
          highlights["@lsp.typemod.type.defaultLibrary"] = highlights.Type
          highlights.doxygenComment = highlights.Comment
          highlights.doxygenBrief = highlights.Comment
          highlights.doxygenSpecialOnelineDesc = highlights.Comment
          highlights.doxygenParam = highlights.Identifier
          highlights.doxygenSpecial = highlights.Identifier
          highlights.SpecialChar = highlights.Identifier
          highlights.doxygenParamName = highlights["@parameter"]
          highlights.Todo = {}
          highlights.DiagnosticUnderlineError.fg = colors.error
          highlights.DiagnosticUnderlineWarn.fg = colors.warning
          highlights.DiagnosticUnderlineInfo.fg = colors.info
          highlights.diffAdded = highlights.DiffAdd
          highlights.diffChanged = highlights.DiffChange
          highlights.diffRemoved = highlights.DiffDelete
          highlights.Identifier = highlights["@field"]
          highlights.yamlPlainScalar = highlights.String

          highlights.OrgTODO = { fg = colors.green }
          highlights.OrgDONE = { fg = colors.dark3 }
          highlights["@OrgTSCheckbox.org"] = { fg = colors.green }
          highlights["@OrgTSCheckboxChecked.org"] = { fg = colors.green }
          highlights.OrgHeadlineLevel1 = { fg = colors.blue }
          for i = 2, 39 do
            highlights["OrgHeadlineLevel" .. tostring(i)] = highlights.OrgHeadlineLevel1
          end
        end,
      })
      vim.cmd.colorscheme("tokyonight")
    end,
  },
  {
    "L3MON4D3/LuaSnip",
    build = "make install_jsregexp",
    keymap = function(map)
      local ls = kit.require_on_exported_call("luasnip")

      -- map
      --     :mode("i")
      --     :amend("<Tab>", function(fallback)
      --       if vim.fn.pumvisible() == 1 then
      --         return vim.api.nvim_feedkeys(vim.keycode("<Down>"), "n", false)
      --       elseif ls.expand_or_jumpable() then
      --         return ls.expand_or_jump()
      --       end
      --       return fallback()
      --     end)
      --     :amend("<S-Tab>", function()
      --       if vim.fn.pumvisible() == 1 then
      --         return vim.api.nvim_feedkeys(vim.keycode("<Up>"), "n", false)
      --       elseif ls.jumpable(-1) then
      --         ls.jump(-1)
      --       end
      --       return vim.api.nvim_feedkeys(vim.keycode("<C-d>"), "n", false)
      --     end)

      map:mode("c"):amend("<CR>", function(fallback)
        if vim.fn.wildmenumode() == 1 then
          vim.api.nvim_feedkeys(vim.keycode("<Down>"), "n", false)
        else
          fallback()
        end
      end)
    end,
    config = function()
      local ls = require("luasnip")

      ls.setup({
        update_events = { "TextChanged", "TextChangedI", "CursorMoved", "CursorMovedI" },
        region_check_events = { "CursorMoved", "CursorMovedI" },
        delete_check_events = { "TextChanged", "TextChangedI" },
        ft_func = function()
          local ok, _ = pcall(vim.treesitter.get_parser)
          if ok and vim.bo.filetype ~= "tex" then
            return require("luasnip.extras.filetype_functions").from_cursor_pos()
          end
          return { vim.bo.filetype }
        end,
      })

      ls.filetype_extend("org", { "tex" })

      require("luasnip.loaders.from_lua").lazy_load()
    end,
  },
  {
    "ibhagwan/fzf-lua",
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function()
      local fzf = kit.require_on_exported_call("fzf-lua")
      fzf.oldfiles = require("plugins.fzf-lua.oldfiles").oldfiles

      map:mode("t"):ft("fzf"):set("<Esc>", vim.cmd.quit)

      map:prefix("<leader>ot", "Tasks"):set("i", function()
        fzf.live_grep({ cwd = "~/obsidian/итмо", query = "- [ ]" })
      end, { desc = "ITMO" })

      map:prefix("<leader>o", "Obsidian"):set("f", function()
        fzf.files({ cwd = "~/obsidian" })
      end, { desc = "Files" })

      map
        :prefix("<leader>f", "+find")
        :set("c", fzf.commands, { desc = "Commands" })
        :set("f", fzf.files, { desc = "Find files" })
        :set("g", fzf.live_grep, { desc = "Grep files" })
        :set("r", fzf.oldfiles, { desc = "Recent files" })
        :set("p", fzf.resume, { desc = "Resume last search" })

      map:mode("t"):set("<C-r>", [['<C-\><C-N>"'.nr2char(getchar()).'pi']], { expr = true })

      map:set("z=", fzf.spell_suggest)

      fzf.register_ui_select()

      local actions = require("fzf-lua.actions")

      local function arc_branch()
        local result = vim.system({ "arc", "branch", "--json" }, { text = true }):wait()
        if result.code ~= 0 then
          vim.notify("Arc error: " .. result.stderr)
          return
        end
        local data = vim.json.decode(result.stdout)

        local current_branch = vim
          .iter(data)
          :filter(function(value)
            return value["current"]
          end)
          :map(function(value)
            return value["name"]
          end)
          :totable()[1]

        local branch_names = vim
          .iter(data)
          :map(function(value)
            return value["name"]
          end)
          :totable()

        vim.ui.select(
          branch_names,
          { prompt = string.format("Select branch (current: %s): ", current_branch) },
          function(choice)
            if not choice then
              return
            end
            local result = vim.system({ "arc", "checkout", choice }, { text = true }):wait()
          end
        )
      end

      map:prefix("<leader>a"):set("c", arc_branch)

      local arc_commands = {
        branch = arc_branch,
      }

      vim.api.nvim_create_user_command("Arc", function(opts)
        local args = vim.split(opts.args, " ", { trimempty = true, plain = true })
        local command = arc_commands[args[1]]
        if not command then
          vim.notify("Unknown command")
          return
        end
        command()
      end, {
        nargs = "*",
        complete = function()
          return vim.tbl_keys(arc_commands)
        end,
      })

      fzf.setup({
        winopts = {
          preview = {
            default = false,
          },
        },
        winopts_fn = function()
          return {
            height = 0.3,
            row = vim.o.lines - 17,
            width = vim.o.columns,
            border = "none",
          }
        end,
        oldfiles = {
          fzf_opts = {
            ["--history"] = vim.fn.stdpath("data") .. "/fzf-lua-oldfiles-history",
          },
        },
        files = {
          fd_opts = "--color=never --type f --hidden --follow --exclude .git --exclude .obsidian --exclude build --exclude .DS_Store --exclude Вложения",
          fzf_opts = {
            ["--history"] = vim.fn.stdpath("data") .. "/fzf-lua-files-history",
          },
        },
        grep = {
          fzf_opts = {
            ["--history"] = vim.fn.stdpath("data") .. "/fzf-lua-grep-history",
          },
        },
        git = {
          status = {
            actions = {
              ["left"] = false,
              ["right"] = false,
              ["ctrl-l"] = { actions.git_unstage, actions.resume },
              ["ctrl-h"] = { actions.git_stage, actions.resume },
              ["ctrl-x"] = { actions.git_reset, actions.resume },
            },
          },
        },
        fzf_opts = {
          ["--ansi"] = "",
          ["--info"] = "inline",
          ["--height"] = "100%",
          ["--layout"] = "reverse",
          ["--border"] = "none",
          ["--cycle"] = "",
          ["--history"] = vim.fn.stdpath("data") .. "/fzf-lua-history",
        },
        fzf_colors = {
          ["fg"] = { "fg", "CursorLine" },
          ["bg"] = { "bg", "Normal" },
          ["hl"] = { "bg", "IncSearch" },
          ["fg+"] = { "fg", "Normal" },
          ["bg+"] = { "bg", "Visual" },
          ["hl+"] = { "bg", "IncSearch" },
          ["info"] = { "fg", "PreProc" },
          ["prompt"] = { "fg", "Keyword" },
          ["pointer"] = { "fg", "Exception" },
          ["marker"] = { "fg", "Keyword" },
          ["spinner"] = { "fg", "Conditional" },
          ["header"] = { "fg", "Comment" },
          ["gutter"] = { "bg", "Normal" },
        },
      })
    end,
  },
  {
    "derekwyatt/vim-fswitch",
    keymap = function(map)
      map:prefix("<leader>f"):set("o", vim.cmd.FSHere)
    end,
    config = function()
      kit.autocmd("BufEnter", {
        pattern = { "*.h", "*.hpp" },
        callback = function()
          vim.b.fswitchdst = "cpp"
          vim.b.fswitchlocs = "reg:|include.*|src/**|"
        end,
      })
    end,
  },
  {
    "jakewvincent/mkdnflow.nvim",
    ft = "markdown",
    config = function()
      require("mkdnflow").setup({
        to_do = {
          symbols = { " ", "-", "x" },
        },
        mappings = {
          MkdnTableNextCell = false,
          MkdnTablePrevCell = false,
          MkdnTablePrevRow = false,
          MkdnDestroyLink = false,
          MkdnEnter = false,
        },
      })
    end,
  },
  {
    "theHamsta/nvim_rocks",
    build = "pip3 install --user hererocks && python3 -mhererocks . -j2.1.0-beta3 -r3.0.0 && cp nvim_rocks.lua lua",
    config = function()
      local rocks = require("nvim_rocks")
      rocks.ensure_installed("luautf8")
    end,
  },
  {
    "starwing/luautf8",
    dependencies = "theHamsta/nvim_rocks",
    priority = 10000,
    config = function()
      _G.utf8 = require("lua-utf8")

      _G.string_sub = string.sub

      local function find_char_pos(s, init)
        if not init then
          return
        end

        if init < 0 then
          init = string.len(s) + init + 1
        end

        local clean, _ = utf8.clean(string_sub(s, 0, init))
        local new_init = utf8.len(clean)
        if utf8.offset(s, new_init) < init then
          new_init = new_init + 1
        end

        return new_init
      end

      local function is_ascii(s)
        for i = 1, #s do
          if s:byte(i) > 128 then
            return false
          end
        end
        return true
      end

      _G.string_find = string.find

      string.find = function(s, pattern, init, plain)
        if is_ascii(s) then
          return string_find(s, pattern, init, plain)
        end

        s = utf8.clean(s, "")

        init = find_char_pos(s, init)

        local start, finish = utf8.find(s, pattern, init, plain)
        if not start then
          return
        end

        local new_start = utf8.offset(s, start)
        local new_finish = utf8.offset(s, finish)
        if start ~= finish then
          new_finish = utf8.offset(s, finish + 1) - 1
        end

        return new_start, new_finish
      end

      _G.string_match = string.match

      string.match = function(s, pattern, init)
        if is_ascii(s) then
          return string_match(s, pattern, init)
        end

        s = utf8.clean(s, "")

        local result = { utf8.match(s, pattern, find_char_pos(s, init)) }

        for i, capture in ipairs(result) do
          if type(capture) == "number" then
            result[i] = utf8.offset(s, capture)
          end
        end

        return unpack(result)
      end

      _G.string_gmatch = string.gmatch

      string.gmatch = function(s, pattern)
        if is_ascii(s) then
          return string_gmatch(s, pattern)
        end

        s = utf8.clean(s, "")

        local iterator = utf8.gmatch(s, pattern)
        return function()
          local result = { iterator() }

          for i, capture in ipairs(result) do
            if type(capture) == "number" then
              result[i] = utf8.offset(s, capture)
            end
          end

          return unpack(result)
        end
      end

      -- string.sub = function(s, i, j)
      --   i = find_char_pos(s, i)
      --   j = find_char_pos(s, j)
      --   return utf8.sub(s, i, j)
      -- end

      _G.string_gsub = string.gsub
      string.gsub = function(s, pattern, repl, n)
        if is_ascii(s) or not utf8.isvalid(s) then
          return string_gsub(s, pattern, repl, n)
        else
          return utf8.gsub(s, pattern, repl, n)
        end
      end

      _G.string_lower = string.lower
      string.lower = function(s)
        if is_ascii(s) or not utf8.isvalid(s) then
          return string_lower(s)
        else
          return utf8.lower(s)
        end
      end

      _G.string_upper = string.upper
      string.upper = function(s)
        if is_ascii(s) or not utf8.isvalid(s) then
          return string_upper(s)
        else
          return utf8.upper(s)
        end
      end
    end,
  },
  {
    "chomosuke/term-edit.nvim",
    config = function()
      require("term-edit").setup({
        prompt_end = "> ",
      })
    end,
  },
  {
    "toppair/peek.nvim",
    build = "deno task --quiet build:fast",
    config = function()
      require("peek").setup({
        app = "browser",
        theme = "light",
      })
      vim.api.nvim_create_user_command("PeekOpen", require("peek").open, {})
      vim.api.nvim_create_user_command("PeekClose", require("peek").close, {})
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      local hooks = require("ibl.hooks")
      hooks.register(hooks.type.WHITESPACE, hooks.builtin.hide_first_space_indent_level)

      require("ibl").setup({
        exclude = {
          filetypes = { "git" },
        },
        scope = {
          enabled = false,
        },
      })
    end,
  },
  {
    "epwalsh/obsidian.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    enabled = is_local_session,
    keymap = function(map)
      map:ft("markdown"):prefix("<leader>o", "Obsidian"):set("o", vim.cmd.ObsidianOpen)
    end,
    config = function()
      require("obsidian.search").Patterns = {
        BlockID = "%^[%w%d][%w%d-]*$",
        FileUrl = "file:/[/{2}]?.*",
        Highlight = "==[^=]+==",
        Markdown = "%[[^][]+%]%([^%)]+%)",
        NakedUrl = "https?://[%a._-]+[%a._#/=&?:+%%-]+[%a/]",
        Tag = "#[%a_/-]+",
        TagCharsOptional = "[%a_/-]*",
        TagCharsRequired = "[%a]+[%a_/-]*[%a]+",
        Wiki = "%[%[[^][%|]+%]%]",
        WikiWithAlias = "%[%[[^][%|]+%|[^%]]+%]%]",
      }

      local obsidian = require("obsidian")

      obsidian.setup({
        disable_frontmatter = true,
        open_app_foreground = true,
        workspaces = {
          {
            name = "personal",
            path = "/Users/danilshvalov/Library/Mobile Documents/iCloud~md~obsidian/Documents/notes",
          },
        },
        notes_subdir = "Заметки/неотсортированное",
        attachments = {
          img_folder = "вложения",
        },
        daily_notes = {
          folder = "Ежедневник",
          date_format = "%Y/%m/%d",
          alias_format = "%Y-%m-%d",
          template = "Ежедневник.md",
        },
        templates = {
          subdir = "Шаблоны",
          date_format = "%Y-%m-%d",
          time_format = "%H:%M",
          substitutions = {},
        },
        note_id_func = function(title)
          local suffix = ""
          if title ~= nil then
            suffix = utf8.lower(utf8.gsub(title:gsub(" ", "-"), "[^%w%d-]", ""))
          else
            for _ = 1, 4 do
              suffix = suffix .. string.char(math.random(65, 90))
            end
          end
          return suffix
        end,
      })

      _G.ObsidianTagCompletion = function(findstart, base)
        local line = vim.api.nvim_get_current_line()
        if findstart == 1 then
          local startcol = utf8.len(line)
          while startcol > 1 and utf8.sub(line, startcol - 1, startcol - 1):match("[#%w_/-]") do
            startcol = startcol - 1
          end
          return utf8.offset(line, startcol)
        end

        if #base == 0 then
          return {}
        end

        local client = assert(obsidian.get_client())
        local seen = {}
        local matches = {}
        local entries = client:find_tags(base)

        for _, entry in ipairs(entries) do
          local tag = entry.tag
          if not seen[tag] then
            seen[tag] = true
            table.insert(matches, tag)
          end
        end

        return matches
      end

      kit.call_at_ft({ "markdown" }, function()
        vim.bo.completefunc = "v:lua.ObsidianTagCompletion"
      end)
    end,
  },
  {
    "lervag/vimtex",
    ft = "tex",
    config = function()
      vim.g.vimtex_syntax_enabled = 0
      vim.g.vimtex_syntax_conceal_disable = 1
      vim.g.vimtex_complete_enabled = 0
      vim.g.vimtex_fold_enabled = 0
      vim.g.vimtex_indent_enabled = 0
      vim.g.vimtex_include_search_enabled = 0
      vim.g.vimtex_doc_enabled = 0
      vim.g.vimtex_format_enabled = 1
      vim.g.vimtex_imaps_enabled = 0
      vim.g.vimtex_include_search_enabled = 0
    end,
  },
  {
    "williamboman/mason.nvim",
    dependencies = {
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
    },
    config = function()
      require("mason").setup()

      require("mason-lspconfig").setup()

      require("mason-tool-installer").setup({
        run_on_start = false,
        ensure_installed = {
          -- cpp
          "clang-format",
          "clangd",
          -- python
          "black",
          "pylsp",
          -- lua
          "stylua",
          "lua_ls",
          -- markdown
          "markdownlint",
        },
      })
    end,
  },
  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keymap = function(map)
      map:ft("oil"):set("q", vim.cmd.bdelete, { nowait = true })
      map
        :prefix("<leader>")
        :set("e", function()
          require("oil").open()
        end)
        :set("E", function()
          require("oil").open(".")
        end)
    end,
    config = function()
      require("oil").setup({
        columns = {},
        buf_options = {
          buflisted = true,
          bufhidden = "",
        },
        win_options = {
          signcolumn = "yes",
          cursorcolumn = false,
        },
      })
    end,
  },
  { "lambdalisue/suda.vim" },
  { "jamessan/vim-gnupg" },
  {
    "notomo/cmdbuf.nvim",
    config = function()
      -- vim.keymap.set("n", "q:", function()
      --   require("cmdbuf").split_open(vim.o.cmdwinheight)
      -- end)
      vim.keymap.set("c", "<C-f>", function()
        require("cmdbuf").split_open(
          vim.o.cmdwinheight,
          { line = vim.fn.getcmdline(), column = vim.fn.getcmdpos() }
        )
        vim.api.nvim_feedkeys(vim.keycode("<C-c>"), "n", true)
      end)

      -- Custom buffer mappings
      vim.api.nvim_create_autocmd({ "User" }, {
        group = vim.api.nvim_create_augroup("cmdbuf_setting", {}),
        pattern = { "CmdbufNew" },
        callback = function(args)
          vim.bo.bufhidden = "wipe" -- if you don't need previous opened buffer state
          vim.keymap.set("n", "q", [[<Cmd>quit<CR>]], { nowait = true, buffer = true })
          vim.keymap.set("n", "dd", [[<Cmd>lua require('cmdbuf').delete()<CR>]], { buffer = true })

          -- you can filter buffer lines
          local lines = vim.tbl_filter(function(line)
            return line ~= "q"
          end, vim.api.nvim_buf_get_lines(args.buf, 0, -1, false))
          vim.api.nvim_buf_set_lines(args.buf, 0, -1, false, lines)
        end,
      })

      -- open lua command-line window
      -- vim.keymap.set("n", "ql", function()
      --   require("cmdbuf").split_open(vim.o.cmdwinheight, { type = "lua/cmd" })
      -- end)

      -- -- q/, q? alternative
      -- vim.keymap.set("n", "q/", function()
      --   require("cmdbuf").split_open(vim.o.cmdwinheight, { type = "vim/search/forward" })
      -- end)
      -- vim.keymap.set("n", "q?", function()
      --   require("cmdbuf").split_open(vim.o.cmdwinheight, { type = "vim/search/backward" })
      -- end)
    end,
  },
  -- {
  --   "altermo/ultimate-autopair.nvim",
  --   branch='v0.6',
  --   config = function()
  --     local function is_cpp_class()
  --       local pattern = "^%%s*%s.+"
  --       local line = vim.api.nvim_get_current_line()
  --       return line:match(pattern:format("class")) ~= nil
  --         or line:match(pattern:format("struct")) ~= nil
  --     end

  --     local function not_fn(fn)
  --       return function(...)
  --         return not fn(...)
  --       end
  --     end

  --     local my_pairs = {
  --       {
  --         "{",
  --         "};",
  --         cond = is_cpp_class,
  --         newline = true,
  --         ft = { "cpp" },
  --       },
  --       {
  --         "(",
  --         ")",
  --         dosuround = true,
  --         newline = true,
  --         space = true,
  --         fly = true
  --       },
  --       {
  --         "{",
  --         "}",
  --         dosuround = true,
  --         newline = true,
  --         space = true,
  --         cond = not_fn(is_cpp_class),
  --       },
  --       {
  --         '"',
  --         '"',
  --         suround = true,
  --         multiline = false,
  --       },
  --       {
  --         "'",
  --         "'",
  --         suround = true,
  --         cond = function(fn)
  --           return not fn.in_lisp() or fn.in_string()
  --         end,
  --         alpha = true,
  --         nft = {
  --           "tex",
  --         },
  --         multiline = false,
  --       },
  --       {
  --         "`",
  --         "`",
  --         cond = function(fn)
  --           return not fn.in_lisp() or fn.in_string()
  --         end,
  --         nft = { "tex" },
  --         multiline = false,
  --       },
  --       { "``", "''", ft = { "tex" } },
  --       {
  --         "```",
  --         "```",
  --         newline = true,
  --         ft = { "markdown" },
  --       },
  --       {
  --         "<!--",
  --         "-->",
  --         ft = { "markdown", "html" },
  --         space = true,
  --       },
  --       {
  --         '"""',
  --         '"""',
  --         newline = true,
  --         ft = { "python" },
  --       },
  --       {
  --         "'''",
  --         "'''",
  --         newline = true,
  --         ft = { "python" },
  --       },
  --     }

  --     local big_pairs = {
  --       { "[", "]" },
  --       { "{", "}" },
  --       { "(", ")" },
  --       { "|", "|" },
  --     }

  --     local big_kinds = { "", "big", "bigg", "Big", "Bigg" }

  --     for _, kind in ipairs(big_kinds) do
  --       for _, pair in pairs(big_pairs) do
  --         table.insert(my_pairs, {
  --           string.format("\\%s%s", kind, pair[1]),
  --           string.format("\\%s%s", kind, pair[2]),
  --           newline = true,
  --           space = true,
  --           ft = { "tex" },
  --         })
  --       end
  --     end

  --     require("ultimate-autopair.utils").maxlines = math.huge

  --     require("ultimate-autopair").setup({
  --       cmap = false,
  --       multiline = false,
  --       close = {
  --         enable = false,
  --       },
  --       tabout = {
  --         enable = true,
  --         hopout = true,
  --       },
  --       internal_pairs = my_pairs,
  --     })
  --   end,
  -- },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "danilshvalov/cmp-path",
      "hrsh7th/cmp-cmdline",
      "saadparwaiz1/cmp_luasnip",
    },
    keymap = function(map)
      --- see https://vi.stackexchange.com/questions/5605/how-to-fix-cmap-breaking-cabbrev
      map:mode("c"):set("<CR>", "<C-]><CR>")
    end,
    config = function()
      local cmp = require("cmp")
      local ls = kit.require_on_exported_call("luasnip")

      local icons = {
        Text = "",
        Method = "󰆧",
        Function = "󰊕",
        Constructor = "",
        Field = "󰜢",
        Variable = "󰀫",
        Class = "󰠱",
        Interface = "",
        Module = "",
        Property = "󰜢",
        Unit = "󰑭",
        Value = "󰎠",
        Enum = "",
        Keyword = "󰌋",
        Snippet = "",
        Color = "󰏘",
        File = "󰈙",
        Reference = "",
        Folder = "󰉋",
        EnumMember = "",
        Constant = "󰏿",
        Struct = "󰙅",
        Event = "",
        Operator = "󰆕",
        TypeParameter = "󰅲",
      }

      local mappings = {
        ["<C-n>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
        ["<C-k>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c", "s" }),
        ["<C-j>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c", "s" }),
        ["<C-e>"] = cmp.mapping({
          i = cmp.mapping.abort(),
          c = cmp.mapping.close(),
        }),
        ["<CR>"] = cmp.mapping(cmp.mapping.confirm(), { "i", "c", "s" }),
        ["<Tab>"] = cmp.mapping({
          i = function(fallback)
            if cmp.get_selected_entry() then
              cmp.confirm()
            elseif ls.expand_or_jumpable() then
              ls.expand_or_jump()
            else
              fallback()
            end
          end,
          s = function(fallback)
            if ls.jumpable(1) then
              ls.jump(1)
            else
              fallback()
            end
          end,
        }),
        ["<S-Tab>"] = cmp.mapping({
          i = function(fallback)
            if ls.jumpable(-1) then
              ls.jump(-1)
            else
              fallback()
            end
          end,
          s = function(fallback)
            if ls.jumpable(-1) then
              ls.jump(-1)
            else
              fallback()
            end
          end,
        }),
      }

      local sources = {
        {
          name = "buffer",
          option = {
            keyword_pattern = [[\k\+]],
          },
        },
        {
          name = "path",
          option = {
            get_cwd = function()
              return vim.fn.getcwd()
            end,
          },
        },
        "nvim_lsp",
        "luasnip",
        "orgmode",
      }

      local priorities = {}

      for index, value in ipairs(sources) do
        if type(value) == "table" then
          value.priority = index
          priorities[index] = value
        else
          priorities[index] = { name = value, priority = index }
        end
      end

      cmp.setup({
        snippet = {
          expand = function(args)
            return ls.lsp_expand(args.body)
          end,
        },
        mapping = mappings,
        sources = priorities,
        formatting = {
          fields = { "kind", "abbr", "menu" },
          format = function(_, vim_item)
            vim_item.menu = vim_item.kind
            vim_item.kind = icons[vim_item.kind]
            return vim_item
          end,
        },
        completion = {
          autocomplete = false,
          keyword_pattern = [[\k\+]],
          completeopt = "menu,menuone",
        },
      })

      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" },
        },
        completion = {
          keyword_pattern = [[\k\+]],
        },
      })

      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "cmdline" },
        }),
      })
    end,
  },
  {
    "Darazaki/indent-o-matic",
    config = function()
      vim.g.editorconfig = false
      require("indent-o-matic").setup({})
    end,
  },
  {
    "otavioschwanck/arrow.nvim",
    config = function()
      require("arrow").setup({
        show_icons = true,
        leader_key = "<C-e>",
      })
    end,
  },
  {
    "folke/which-key.nvim",
    dependencies = { "Wansmer/langmapper.nvim" },
    config = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300

      local lmu = require("langmapper.utils")
      local view = require("which-key.view")
      local execute = view.execute

      view.execute = function(prefix_i, mode, buf)
        prefix_i = lmu.translate_keycode(prefix_i, "default", "ru")
        execute(prefix_i, mode, buf)
      end

      require("which-key").setup()
    end,
  },
  {
    "rest-nvim/rest.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    ft = "http",
    keymap = function(map)
      local rest = kit.require_on_exported_call("rest-nvim")
      map:prefix("<leader>r", "Rest"):set("r", rest.run, { desc = "Run" })
    end,
    config = function()
      require("rest-nvim").setup({
        skip_ssl_verification = true,
        result = {
          formatters = {
            json = function(body)
              return vim.fn.system({ "jq", "-n", body })
            end,
          },
        },
      })
    end,
  },
  -- {
  --   "nvim-orgmode/orgmode",
  --   dependencies = {
  --     "nvim-treesitter/nvim-treesitter",
  --   },
  --   event = "VeryLazy",
  --   config = function()
  --     require("orgmode").setup_ts_grammar()

  --     require("nvim-treesitter.configs").setup({
  --       highlight = {
  --         enable = true,
  --       },
  --       ensure_installed = { "org" },
  --     })

  --     require("orgmode").setup({
  --       org_agenda_files = "~/org/**/*",
  --       org_default_notes_file = "~/org/refile.org",
  --       org_adapt_indentation = false,
  --     })
  --   end,
  -- },
}

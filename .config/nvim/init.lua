vim.loader.enable()

table.unpack = unpack

require("kit.core")

-- disable any backup
vim.o.swapfile = false
vim.o.backup = false
vim.o.writebackup = false

vim.o.autoread = true

vim.o.shada = "!,'10000,<50,s10,h"

vim.o.undofile = true

vim.o.smoothscroll = true

vim.o.termguicolors = true

vim.o.colorcolumn = "80"

-- show tabs
vim.o.list = true
vim.opt.listchars = {
  tab = ">-",
}

vim.o.foldlevel = 99
vim.o.wrap = true
vim.o.breakindent = true
vim.o.lbr = true
vim.o.history = 10000
vim.o.mouse = "nv"

vim.o.pumheight = 5
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.clipboard = "unnamedplus"

-- enable cursor line highlight
vim.o.cursorline = true

vim.o.cinoptions = vim.o.cinoptions .. "L0"

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local plugins = require("plugins")

for _, plug in ipairs(plugins) do
  local init = plug.init
  plug.init = function(...)
    local args = { ... }

    if plug.keymap then
      -- try to load langmapper first
      pcall(require, "langmapper")
      local ok, keymap = pcall(require, "keymap")
      if ok then
        plug.keymap(keymap.map)
      end
    end

    if init then
      init(unpack(args))
    end
  end
end

require("lazy").setup(plugins, {
  colorscheme = { "tokyonight" },
  change_detection = {
    notify = false,
  },
})

vim.o.spell = true
vim.o.spelllang = "en,ru"
vim.o.spellcapcheck = ""
vim.o.spelloptions = "camel"
vim.o.spellsuggest = "best,7"
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.expandtab = true
vim.o.smartindent = true
vim.o.autoindent = true
vim.o.smarttab = true
vim.o.ignorecase = true
vim.o.incsearch = true
-- vim.o.smartcase = true
vim.o.hlsearch = true
vim.o.scrolloff = 10
vim.o.sidescrolloff = 20
vim.o.signcolumn = "yes"

vim.opt.completeopt = { "menu", "preview", "menuone", "noinsert" }

kit.autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({
      higroup = "IncSearch",
      timeout = 150,
    })
  end,
})

local function set_current_line(new_line)
  vim.fn.setline(".", new_line)
  local pos = vim.fn.getpos(".")
  -- the third value is the columns
  pos[3] = vim.fn.col("$")
  vim.fn.setpos(".", pos)
end

local function insert_item()
  local ts_utils = require("nvim-treesitter.ts_utils")

  if vim.bo.filetype == "markdown" then
    return vim.cmd.MkdnNewListItem()
  end

  if vim.bo.filetype ~= "tex" then
    return
  end

  local node = ts_utils.get_node_at_cursor()

  while node do
    local first_line = vim.split(vim.treesitter.get_node_text(node, 0), "\n")[1]
    if node:type() == "generic_environment" then
      local cur_line = vim.fn.getline(".")
      if #cur_line:gsub("^%s*", "", 1) ~= 0 then
        vim.fn.append(vim.fn.line("."), string.rep(" ", vim.fn.indent(".")))
        local pos = vim.api.nvim_win_get_cursor(0)
        vim.api.nvim_win_set_cursor(0, { pos[1] + 1, pos[2] })
        cur_line = vim.fn.getline(vim.fn.line("."))
      end
      if first_line:match("table") then
        set_current_line(cur_line .. "\\hline")
      elseif first_line:match("itemize") or first_line:match("enumerate") then
        set_current_line(cur_line .. "\\item ")
      end
      break
    end
    node = node:parent()
  end
end

kit.create_cmd("Open", function(opts)
  kit.open_file(string.format('"%s"', vim.fn.expand(opts.args)))
end, {
  nargs = "?",
  complete = "file",
})

kit.autocmd("BufWinEnter", {
  pattern = { "*.pdf", "*.png", "*.jpg", "*.heic" },
  callback = function()
    kit.open_file("%")
    MiniBufremove.delete()
  end,
})

kit.open_file = function(path)
  vim.fn.system("open " .. vim.fn.expand(path))
end

vim.o.diffopt = "internal,filler,closeoff,linematch:60,horizontal,foldcolumn:0"

vim.opt.fillchars:append("diff: ")

kit.autocmd("BufWinEnter", {
  desc = "return cursor to where it was last time closing the file",
  pattern = "*",
  command = 'silent! normal! g`"zv',
})

kit.autocmd("TermOpen", {
  callback = function()
    vim.wo.spell = false
  end,
})

function _G.make_comment(str)
  return vim.bo.commentstring:gsub("%%s", str)
end

map:ft({ "tex", "markdown" }):mode("i"):set("<A-CR>", insert_item)

kit.call_at_ft({ "markdown", "org", "tex" }, function()
  vim.bo.textwidth = 80
end)

vim.cmd.cabbrev("ц w")
vim.cmd.cabbrev("й q")
vim.cmd.cabbrev("цй wq")

if vim.env["SSH_TTY"] then
  vim.g.clipboard = {
    name = "OSC 52",
    copy = {
      ["+"] = require("vim.ui.clipboard.osc52").copy("+"),
      ["*"] = require("vim.ui.clipboard.osc52").copy("*"),
    },
    paste = {
      ["+"] = require("vim.ui.clipboard.osc52").paste("+"),
      ["*"] = require("vim.ui.clipboard.osc52").paste("*"),
    },
  }
end

block_on = function(async_fn_with_callback, timeout)
  local done = false
  local result
  timeout = timeout and timeout or 2000

  local function collect_result(res)
    result = res
    done = true
  end

  async_fn_with_callback(collect_result)

  vim.wait(timeout, function()
    return done
  end, 20, false)

  return result
end

local input = function(opts)
  return block_on(function(cb)
    vim.ui.input(opts, cb)
  end)
end

local echo = function(prompt)
  vim.api.nvim_echo({ { prompt } }, false, {})
end

local confirm = function(opts)
  echo(opts.prompt)
  local answer = vim.fn.nr2char(vim.fn.getchar()):lower()
  if answer ~= "y" and answer ~= "n" then
    answer = opts.default and "y" or "n"
  end

  if answer == "y" then
    return true
  elseif answer == "n" then
    return false
  end
end

local Arc = {}

function Arc.root(opts)
  opts = opts or {}

  local result = vim.system({ "arc", "root" }, { text = true, cwd = opts.cwd }):wait()
  if result.code ~= 0 then
    return
  end
  return vim.fs.normalize(vim.trim(result.stdout))
end

function Arc.make_link(opts)
  opts = opts or {}
  local path = vim.fs.normalize(vim.api.nvim_buf_get_name(0))
  local cwd = vim.fs.dirname(path)

  local root = Arc.root({ cwd = cwd })
  if not root then
    vim.notify("Not a mounted arc repository")
    return
  end

  path = path:sub(#root + 1)
  local link = vim.fs.joinpath("https://a.yandex-team.ru/arcadia", path)

  local line_number = confirm({ prompt = "Add line number [Y/n] ", default = true })

  if line_number then
    local line = vim.api.nvim_win_get_cursor(0)[1]
    link = link .. string.format("#L%s", line)
  end

  return link
end

function Arc.copy_link()
  local link = Arc.make_link()
  if not link then
    return
  end

  vim.fn.setreg("*", link)
  vim.notify(string.format("Copied link: %s", link))
end

map:prefix("<leader>a"):set("l", Arc.copy_link)

local function url_shortener()
  local url = input({ prompt = "Enter URL: " })
  local result = vim.system({ "clck", url }, { text = true }):wait()
  if result.code ~= 0 then
    vim.notify("Error: " .. result.stderr)
    return
  end

  local short_url = vim.trim(result.stdout)
  vim.fn.setreg("*", short_url)
  vim.notify("Short url: " .. short_url)
end

map:set("<leader>u", url_shortener)

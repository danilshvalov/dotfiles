local utils = require("orgmode.utils")
local config = require("orgmode.config")
local Files = require("orgmode.parser.files")
local File = require("orgmode.parser.file")
local Templates = require("orgmode.capture.templates")
local ClosingNote = require("orgmode.capture.closing_note")
local Date = require("orgmode.objects.date")
local Menu = require("orgmode.ui.menu")
local Calendar = require("orgmode.objects.calendar")

---@class Journal
---@field templates Templates
---@field closing_note ClosingNote
local Journal = {}

function Journal:new()
  local data = {}
  data.templates = Templates:new()
  data.closing_note = ClosingNote:new()
  setmetatable(data, self)
  self.__index = self
  return data
end

---@param date? Date
function Journal.open(opts)
  local cfg = opts.config
  local date = opts.date or Date:now()

  local file_path = cfg.directory .. opts.date:format(cfg.file_format)
  local file = vim.fn.resolve(vim.fn.fnamemodify(file_path, ":p"))

  if vim.fn.expand("%:p") ~= vim.fn.expand(file) then
    utils.open_window(file, 16, config.win_split_mode, config.win_border)
  end
end

function Journal.open_yesterday(opts)
  opts = vim.tbl_deep_extend("force", opts, { date = Date:today():adjust("-1d") })
  return Journal.open(opts)
end

function Journal.new_entry(opts)
  local cfg = opts.config
  local date = opts.date or Date:now()
  local file_path = cfg.directory .. date:format(cfg.file_format)
  local file = vim.fn.resolve(vim.fn.fnamemodify(file_path, ":p"))

  if vim.fn.expand("%:p") ~= vim.fn.expand(file) then
    utils.open_window(file, 16, config.win_split_mode, config.win_border)
  end

  local date_line = cfg.date.prefix .. date:format(cfg.date.format)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, true)
  local has_date = false
  for _, line in ipairs(lines) do
    if vim.trim(line) == vim.trim(date_line) then
      has_date = true
    end
  end

  if not has_date then
    local content = vim.split(date_line, "\n")
    local row = vim.fn.line("$")
    vim.api.nvim_buf_set_lines(0, row, -1, true, content)
  end

  if cfg.time then
    local content = cfg.time.prefix .. date:format(cfg.time.format)
    content = vim.split(content, "\n")
    local row = vim.fn.line("$")

    vim.api.nvim_buf_set_lines(0, row, -1, true, content)
    vim.api.nvim_win_set_cursor(0, { row + #content, 0 })
  end

  vim.cmd("startinsert!")
end

-- function Journal.new_entry()
--   local cfg = config.journal
--   local date = Date:now()
--   local file_path = cfg.directory .. date:format(cfg.file_format)
--   local file = vim.fn.resolve(vim.fn.fnamemodify(file_path, ":p"))

--   if vim.fn.expand("%:p") ~= vim.fn.expand(file) then
--     utils.open_window(file, 16, config.win_split_mode, config.win_border)
--   end

--   local is_empty = vim.fn.filereadable(file) == 0 or #vim.fn.readfile(file) == 0
--   if is_empty then
--     local content = cfg.date.prefix .. date:format(cfg.date.format)
--     vim.api.nvim_buf_set_lines(0, 0, -1, true, vim.split(content, "\n", true))
--   else
--     vim.api.nvim_buf_set_lines(0, vim.fn.line("$"), -1, true, { "" })
--   end

--   local row = vim.fn.line("$")
--   if is_empty then
--     row = row - 1
--   end

--   local content = cfg.time.prefix .. date:format(cfg.time.format)
--   content = vim.split(content, "\n", true)

--   vim.api.nvim_buf_set_lines(0, row, -1, true, content)
--   vim.api.nvim_win_set_cursor(0, { row + #content, 0 })
--   vim.cmd("startinsert!")
-- end

-- function Journal.menu()
--   local journals = vim.tbl_keys(config.journal.journals)
--   vim.ui.select(journals, { prompt = "Select journal" }, function(journal)
--     if journal then
--       Journal._menu(config.journal.journals[journal])
--     end
--   end)
-- end

function Journal.menu(opts)
  opts = opts or {}
  opts.config = config.journal.journals[config.journal.current_journal]
  opts.date = opts.date or Date:now()

  local datetime = opts.date:format("%d.%m.%Y")

  local items = {
    {
      label = "Today journal",
      key = "c",
      action = function()
        Journal.open(opts)
      end,
    },
    {
      label = "Yesterday journal",
      key = "p",
      action = function()
        Journal.open_yesterday(opts)
      end,
    },
    {
      label = "New entry",
      key = "C",
      action = function()
        Journal.new_entry(opts)
      end,
    },
    {
      label = "Open calendar",
      key = "o",
      action = function()
        Calendar:new({}):open():next(function(selected_date)
          if selected_date then
            opts = vim.tbl_extend("force", opts, { date = selected_date })
            Journal.menu(opts)
          end
        end)
      end,
    },
    {
      label = "Select journal",
      key = "s",
      action = function()
        local journals = vim.tbl_keys(config.journal.journals)
        vim.ui.select(journals, { prompt = "Select journal" }, function(journal)
          if journal then
            config.journal.current_journal = journal
            Journal.menu(opts)
          end
        end)
      end,
    },
  }

  Menu:new({
    title = string.format("Journal (%s, %s)", config.journal.current_journal, datetime),
    items = items,
    prompt = "Enter choice: ",
  }):open()
end

return Journal

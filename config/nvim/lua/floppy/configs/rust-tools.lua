-- :fennel:1651221107
local rusttools = require("rust-tools")
local executors = require("rust-tools/executors")
local opts = {tools = {autoSetHints = true, hover_with_actions = true, executor = executors.termopen, runnables = {use_telescope = true}, debuggables = {use_telescope = true}, inlay_hints = {only_current_line = false, only_current_line_autocmd = "CursorHold", show_parameter_hints = true, parameter_hints_prefix = "<- ", other_hints_prefix = "=> ", max_len_align = false, max_len_align_padding = 1, right_align = false, right_align_padding = 7, highlight = "Comment"}, hover_actions = {border = {[{["\226\149\173"] = "FloatBorder"}] = {["\226\148\128"] = "FloatBorder"}, [{["\226\149\174"] = "FloatBorder"}] = {["\226\148\130"] = "FloatBorder"}, [{["\226\149\175"] = "FloatBorder"}] = {["\226\148\128"] = "FloatBorder"}, [{["\226\149\176"] = "FloatBorder"}] = {["\226\148\130"] = "FloatBorder"}}, auto_focus = false}, crate_graph = {backend = "x11", output = nil, full = true}}, server = {}, dap = {adapter = {type = "executable", command = "lldb-vscode", name = "rt_lldb"}}}
local function setup()
  return rusttools.setup(opts)
end
return {setup = setup}
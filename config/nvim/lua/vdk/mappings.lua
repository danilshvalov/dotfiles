local utils = require("vdk.core.utils")
local map, nmap = utils.map, utils.nmap

map("в", "d")
map("н", "y")
map("з", "p")
map("Ж", ":")
map("ш", "i")
map("ф", "a")
map("Ш", "I")
map("Ф", "A")
map("е", "t")
map("а", "f")
map("Е", "T")
map("с", "c")
map("к", "r")
map("ы", "s")
map("Ы", "S")
map("щ", "o")
map("Щ", "O")
map("п", "g")
map("г", "u")

map("go", "o<Esc>")
map("gO", "O<Esc>")

-- fast scrolling
map("J", "9j")
map("K", "9k")

map("<S-Down>", "9<Down>")
map("<S-Up>", "9<Up>")

map("<Down>", "g<Down>")
map("<Up>", "g<Up>")
map("j", "gj")
map("k", "gk")
map("о", "gj")
map("л", "gk")

-- cd to file directory
map("<Leader>cd", "<Cmd>cd %:p:h<CR>")
map("<Leader>cp", "<Cmd>silent !pwd | pbcopy<CR>")
map("<Leader>od", "<Cmd>silent !open .<CR>")

-- jumping back and forth
map("<C-K>", "<C-O>")
map("<C-L>", "<C-I>")

-- % TODO: fix
nmap("<C-Right>", "<Cmd>vertical resize -1<CR>")
nmap("<C-Left>", "<Cmd>vertical resize +1<CR>")
nmap("<C-Up>", "<Cmd>resize +1<CR>")
nmap("<C-Down>", "<Cmd>resize -1<CR>")

nmap("<Leader>do", "<Cmd>DiffviewOpen<CR>")

nmap("<A-s>", "<Cmd>Neoformat | w<CR>")

map("gt", ":tabnext<CR>")
map("gT", ":tabprevious<CR>")
map("gq", ":tabclose<CR>")

map("<Leader>cs", "<Cmd>noh<CR>")

map("<Space>", "<Leader>", { remap = true, silent = true })

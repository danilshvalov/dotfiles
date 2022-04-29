local pack = "packer"

local function bootstrap(name, url, path)
    if vim.fn.isdirectory(path) == 0 then
        print(name .. ": installing in data dir...")

        vim.fn.system({ "git", "clone", "--depth", "1", url, path })

        vim.cmd([[redraw]])
        print(name .. ": finished installing")
    end
end

bootstrap(
    "tangerine.nvim",
    "https://github.com/udayvir-singh/tangerine.nvim",
    vim.fn.stdpath([[data]]) .. "/site/pack/" .. pack .. "/start/tangerine.nvim"
)

require("tangerine").setup({
    compiler = {
        hooks = { "oninit", "onsave" },
        verbose = false,
    },
})

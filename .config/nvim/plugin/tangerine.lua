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
    "hibiscus.nvim",
    "https://github.com/udayvir-singh/hibiscus.nvim",
    vim.fn.stdpath([[data]]) .. "/site/pack/" .. pack .. "/start/hibiscus.nvim"
)

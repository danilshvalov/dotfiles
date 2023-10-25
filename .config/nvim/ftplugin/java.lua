vim.bo.tabstop = 4
vim.bo.shiftwidth = 4

local config = {
  cmd = { "jdtls" },
  root_dir = vim.fs.dirname(
    vim.fs.find({ "gradlew", ".git", "mvnw", "pom.xml" }, { upward = true })[1]
  ),
}
require("jdtls").start_or_attach(config)

local leap = require("leap")

local hi = require("vdk.core.utils").hi

leap.setup({})
leap.set_default_keymaps()

hi("LeapMatch", { fg = "#ff768e" })
hi("LeapLabelPrimary", { bg = "#ff768e" })
hi("LeapLabelSecondary", { bg = "#ff768e" })
hi("LeapBackdrop", { fg = "gray" })

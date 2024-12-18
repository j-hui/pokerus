return {
  "gbprod/substitute.nvim",
  dependencies = { "gbprod/yanky.nvim" },
  keys = {
    { mode = "n", "s",  "<cmd>lua require('substitute').operator()<cr>", desc = "substitute" },
    { mode = "n", "ss", "<cmd>lua require('substitute').line()<cr>",     desc = "substitute-line" },
    { mode = "n", "S",  "<cmd>lua require('substitute').eol()<cr>",      desc = "substitute-eol" },
    { mode = "x", "s",  "<cmd>lua require('substitute').visual()<cr>",   desc = "substitute" },
  },
  config = function()
    require("substitute").setup({
      on_substitute = require("yanky.integration").substitute(),
    })
  end,
}

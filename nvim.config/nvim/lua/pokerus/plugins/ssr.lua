return {
  "cshuaimin/ssr.nvim",
  keys = {
    { "<leader>s", function() require("ssr").open() end, mode = { "n", "x" }, desc = "ssr-open" },
  },
}

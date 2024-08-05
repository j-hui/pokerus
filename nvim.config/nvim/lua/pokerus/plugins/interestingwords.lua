local function fn(f, ...)
  local args = { ... }
  return function()
    vim.fn[f](unpack(args))
  end
end

return {} or {
  "lfv89/vim-interestingwords",
  keys = {
    { "<leader>i", fn("InterestingWords", "n"), mode = "n", desc = "mark-interesting" },
    { "<leader>i", fn("InterestingWords", "n"), mode = "x", desc = "mark-interesting" },
    { "[i",        fn("WordNavigation", 0),     mode = "n", desc = "prev-interesting" },
    { "]i",        fn("WordNavigation", 1),     mode = "n", desc = "next-interesting" },
    {
      "<leader>I",
      function()
        vim.cmd [[noh]]
        fn("UncolorAllWords")()
      end,
      mode = "n",
      desc = "mark-interesting"
    },
  },
}

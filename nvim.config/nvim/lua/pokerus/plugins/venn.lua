local M = {}

local function noremap(mode, lhs, rhs)
  vim.api.nvim_buf_set_keymap(0, mode, lhs, rhs, { noremap = true })
end

local function unmap(mode, lhs)
  vim.api.nvim_buf_del_keymap(0, mode, lhs)
end

-- venn.nvim: enable or disable keymappings
M.toggle = function()
  local venn_enabled = vim.inspect(vim.b.venn_enabled)
  if venn_enabled == "nil" then
    vim.b.venn_enabled = true

    vim.b.venn_ve = vim.api.nvim_get_option "virtualedit"
    vim.api.nvim_set_option("virtualedit", "all")

    noremap("n", "<Down>", "<C-v>j:VBox<CR>")
    noremap("n", "<Up>", "<C-v>k:VBox<CR>")
    noremap("n", "<Left>", "<C-v>h:VBox<CR>")
    noremap("n", "<Right>", "<C-v>l:VBox<CR>")
    noremap("v", "<CR>", ":VBox<CR>")

    print "Enabled Venn mode. Press <CR> in visual mode to create box."
  else
    vim.api.nvim_set_option("virtualedit", vim.b.venn_ve)

    unmap("n", "<Down>")
    unmap("n", "<Up>")
    unmap("n", "<Left>")
    unmap("n", "<Right>")
    unmap("v", "<CR>")

    vim.b.venn_enabled = nil

    print "Disabled Venn mode."
  end
end

M.plug = function(use)
  use {
    "jbyuki/venn.nvim",
    config = function()
      vim.g.venn_ve = nil
      vim.g.venn_enabled = false
      vim.cmd [[command! Venn lua require"pokerus.plugins.venn".toggle()]]
    end,
  }
end

return M

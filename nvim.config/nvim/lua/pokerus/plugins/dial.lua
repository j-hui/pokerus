return {
  plug = function(use)
    use {
      "monaqa/dial.nvim",
      config = function()
        local dial = require("dial.map")

        require("pokerus").nmap({
          ["+"] = { dial.inc_normal(), "dial-increment" },
          ["_"] = { dial.dec_normal(), "dial-decrement" },
        }, { noremap = true })

        require("pokerus").vmap({
          ["+"] = { dial.inc_visual(), "dial-increment" },
          ["_"] = { dial.dec_visual(), "dial-decrement" },
          ["g+"] = { dial.inc_gvisual(), "dial-g-increment" },
          ["g_"] = { dial.dec_gvisual(), "dial-g-decrement" },
        }, { noremap = true })
      end,
    }
  end,
}

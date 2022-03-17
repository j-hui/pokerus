return {
  plug = function(use)
    use {
      "monaqa/dial.nvim",
      config = function()
        local dial = require("dial.map")

        require("pokerus").nmap({
          ["+"] = { dial.inc_normal(), "dial-increment" },
          ["-"] = { dial.dec_normal(), "dial-decrement" },
        }, { noremap = true })

        require("pokerus").xmap({
          ["+"] = { dial.inc_visual(), "dial-increment" },
          ["-"] = { dial.dec_visual(), "dial-decrement" },
          ["g+"] = { dial.inc_gvisual(), "dial-g-increment" },
          ["g-"] = { dial.dec_gvisual(), "dial-g-decrement" },
        }, { noremap = true })
      end,
    }
  end,
}

#!/usr/bin/env nix-shell
#!nix-shell --pure --keep LANG -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.xmobar])"

import           Control.Concurrent             ( forkOS )
import           System.Directory               ( doesPathExist )

import           Xmobar

baseConfig :: Config
baseConfig = defaultConfig
  { font             = "SauceCodePro Nerd Font Mono 12"
  , bgColor          = "black"
  , fgColor          = "#646464"
  , position         = Top
  , border           = BottomB
  , borderColor      = "#646464"
  , sepChar          = "%"    -- delineator between plugin names and straight text
  , alignSep         = "}{"   -- separator between left-right alignment
  , lowerOnStart     = True   -- send to bottom of window stack on start
  , hideOnStart      = False  -- start with window unmapped (hidden)
  , allDesktops      = True   -- show on all desktops
  , overrideRedirect = True   -- set the Override Redirect flag (Xlib)
  , pickBroadest     = False  -- choose widest display (multi-monitor)
  , persistent       = False  -- enable/disable hiding (True = disabled)
  , commands         = runnables
  }

runnables :: [Runnable]
runnables =
  [ Run XMonadLog
  , Run $ NamedXPropertyLog "_XMONAD_TITLE" "XMonadTitle"
  , Run $ Date "<fc=#ABABAB>%F (%a) %T</fc>" "date" 1
  , Run $ Kbd
    [("us(dvorak)", "<fc=#00008B>DV</fc>"), ("us", "<fc=#8B0000>US</fc>")]
  , Run $ Weather
    "RJTT"
    [ "--template"
    , "<skyCondition> | <fc=#4682B4><tempC></fc>°C | <fc=#4682B4><rh></fc>% | <fc=#4682B4><pressure></fc>hPa"
    ]
    36000
  , Run $ DynNetwork
    [ "--template"
    , "<dev>: <tx>kB/s|<rx>kB/s"
    , "--Low"
    , "1000"
    , "--High"
    , "5000"
    , "--low"
    , "darkgreen"
    , "--normal"
    , "darkorange"
    , "--high"
    , "darkred"
    ]
    10
  , Run $ MultiCpu
    [ "--template"
    , "Cpu: <autovbar>"
    , "--Low"
    , "50"
    , "--High"
    , "85"
    , "--low"
    , "darkgreen"
    , "--normal"
    , "darkorange"
    , "--high"
    , "darkred"
    ]
    10
  , Run $ CoreTemp
    [ "--template"
    , "Temp: <core0>°C|<core1>°C"
    , "--Low"
    , "70"
    , "--High"
    , "80"
    , "--low"
    , "darkgreen"
    , "--normal"
    , "darkorange"
    , "--high"
    , "darkred"
    ]
    50
  , Run $ Memory
    [ "--template"
    , "Mem: <used>GB/<total>GB"
    , "--Low"
    , "20"
    , "--High"
    , "90"
    , "--low"
    , "darkgreen"
    , "--normal"
    , "darkorange"
    , "--high"
    , "darkred"
    , "--"
    , "--scale"
    , "1024"
    ]
    10
  , Run $ BatteryP
    ["BAT1", "BAT0"]
    [ "--template"
    , "Bat: <left> <acstatus>"
    , "--Low"
    , "10"
    , "--High"
    , "80"
    , "--low"
    , "darkred"
    , "--normal"
    , "darkorange"
    , "--high"
    , "darkgreen"
    , "--"
    , "-P" -- Include percentage symbol in <left>
    , "-o" -- discharging status
    , "[Battery]"
    , "-O" -- AC "on" status
    , "<fc=#dAA520>[Charging]</fc>"
    , "-i" -- AC "idle" status (i.e., charged)
    , "<fc=#006000>[Charged]</fc>"
    ]
    50
  , Run $ Volume "default" "Master"
    [ "--template"
    , "Vol: <volumebar>"
    ]
    1
  ]

topBar :: Bool -> Config
topBar isLaptop = baseConfig { template = left ++ " }{ " ++ right }
 where
  left = "%XMonadLog%"
  right | isLaptop  = "%battery% | %default:Master% | %date% | %kbd% "
        | otherwise = "%default:Master% | %date% | %kbd% "

bottomBar :: Bool -> Config
bottomBar isLaptop = baseConfig { position = Bottom
                                , template = left ++ " }{ " ++ right
                                }
 where
  left  = "%XMonadTitle%"
  right = "%multicpu% | %memory% | %dynnetwork%"

main :: IO ()
main = do
  isLaptop <- doesPathExist "/sys/class/power_supply/BAT0" -- heuristic for detecting laptops
  forkOS $ xmobar $ topBar isLaptop
  xmobar $ bottomBar isLaptop

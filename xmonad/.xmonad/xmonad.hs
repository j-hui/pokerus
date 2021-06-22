{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import           Control.Monad                  ( forM_ )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhileEnd
                                                , elemIndex
                                                , find
                                                , isPrefixOf
                                                , nub
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , listToMaybe
                                                )
import           System.Exit                    ( exitSuccess )
import           System.IO.Unsafe               ( unsafePerformIO )

import XMonad
    ( mod4Mask,
      io,
      spawn,
      (|||),
      xmonad,
      (-->),
      (<&&>),
      (<+>),
      (=?),
      className,
      composeAll,
      doF,
      doFloat,
      doShift,
      liftX,
      title,
      sendMessage,
      windows,
      withFocused,
      KeyMask,
      Window,
      Dimension,
      MonadIO,
      Default(def),
      MonadState(get),
      ManageHook,
      X,
      XConfig(manageHook, handleEventHook, modMask, terminal,
              startupHook, layoutHook, workspaces, borderWidth,
              normalBorderColor, focusedBorderColor, focusFollowsMouse, logHook,
              keys),
      ChangeLayout(NextLayout, FirstLayout),
      Tall(Tall) )
import qualified XMonad.Core                   as Core
import qualified XMonad.StackSet               as SS

import           XMonad.Actions.CopyWindow      ( kill1 )
import           XMonad.Actions.CycleWS         ( Direction1D(..)
                                                , WSType(..)
                                                , moveTo
                                                , nextScreen
                                                , prevScreen
                                                , shiftTo
                                                )
import           XMonad.Actions.Promote         ( promote )
import           XMonad.Actions.RotSlaves       ( rotAllDown
                                                , rotAllUp
                                                , rotSlavesDown
                                                , rotSlavesUp
                                                )
import           XMonad.Actions.WithAll         ( killAll )


import           XMonad.Actions.DynamicProjects ( Project(..)
                                                , activateProject
                                                , dynamicProjects
                                                , lookupProject
                                                , shiftToProject
                                                , switchProject
                                                )
import           XMonad.Actions.DynamicWorkspaces
                                                ( addHiddenWorkspace )
import qualified XMonad.Hooks.DynamicLog       as Log
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.FadeInactive      ( fadeInactiveLogHook )
-- import qualified XMonad.Hooks.ManageDebug            as Debug
import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docksEventHook
                                                , manageDocks
                                                )
import           XMonad.Hooks.ManageHelpers     ( (-?>)
                                                , composeOne
                                                , doCenterFloat
                                                , doFullFloat
                                                , isFullscreen
                                                , transience'
                                                )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Hooks.UrgencyHook       ( NoUrgencyHook(..)
                                                , focusUrgent
                                                , readUrgents
                                                , withUrgencyHook
                                                )
import           XMonad.Hooks.WorkspaceHistory  ( workspaceHistory
                                                , workspaceHistoryHook
                                                )


import qualified XMonad.Layout.Tabbed          as Tabbed

-- Layouts modifiers
import           XMonad.Layout.Fullscreen       ( fullscreenFocus
                                                , fullscreenSupport
                                                )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout(..) )
import           XMonad.Layout.LimitWindows     ( decreaseLimit
                                                , increaseLimit
                                                , limitWindows
                                                )
import           XMonad.Layout.MultiToggle      ( Toggle(..)
                                                , mkToggle
                                                , single
                                                )
import           XMonad.Layout.MultiToggle.Instances
                                                ( StdTransformers(MIRROR) )
import           XMonad.Layout.NoBorders        ( noBorders )
import           XMonad.Layout.Renamed          ( Rename(..)
                                                , renamed
                                                )
import           XMonad.Layout.Spacing          ( Border(..)
                                                , Spacing
                                                , spacingRaw
                                                )
import           XMonad.Layout.WindowNavigation ( windowNavigation )

import qualified XMonad.Util.Dmenu             as Dmenu
import           XMonad.Util.EZConfig           ( mkKeymap )
import           XMonad.Util.Run                ( runProcessWithInput )
import           XMonad.Util.SpawnOnce          ( spawnOnce )

import qualified Codec.Binary.UTF8.String      as UTF8
import qualified DBus                          as D
import qualified DBus.Client                   as D
-------------------------------------------------------------------------------
-- Variables
--
myModMask :: KeyMask
myModMask = mod4Mask -- super/windows key

myTerminal :: String
myTerminal = "kitty"

myTerminalAt :: String -> String
myTerminalAt path = "kitty --directory " ++ path

myEditor :: String -> String
myEditor file = myTerminal ++ " nvim " ++ file

myBrowser :: String
myBrowser = "qutebrowser "

myBrowser' :: String
myBrowser' = "google-chrome-stable "

myWebapp :: String -> String
myWebapp url = "google-chrome-stable --new-window --app=" ++ url

myMailReader :: String
myMailReader = myTerminal ++ " neomutt"

myBorderWidth :: Dimension
myBorderWidth = 2

myScript :: String -> String
myScript s = "~/bin/" ++ s

mySpawn :: String -> X ()
mySpawn s = spawn $ myScript s

myDmenu :: MonadIO m => String -> [String] -> m String
myDmenu prompt = Dmenu.menuArgs "rofi" ["-dmenu", "-p", prompt]

-------------------------------------------------------------------------------
-- Colors
--

myFgColor :: String
myFgColor = fromXres "*color7"

myFgColor' :: String
myFgColor' = fromXres "*color3"

myUrgentColor :: String
myUrgentColor = fromXres "*color9"

myBgColor :: String
myBgColor = fromXres "*color18"

myBgColor' :: String
myBgColor' = fromXres "*color8"

myTabTheme :: Tabbed.Theme
myTabTheme = def { Tabbed.activeColor         = myBgColor'
                 , Tabbed.inactiveColor       = myBgColor
                 , Tabbed.urgentColor         = myUrgentColor
                 , Tabbed.activeTextColor     = myFgColor
                 , Tabbed.inactiveTextColor   = myFgColor
                 , Tabbed.urgentTextColor     = myFgColor
                 , Tabbed.activeBorderColor   = myBgColor'
                 , Tabbed.inactiveBorderColor = myBgColor
                 , Tabbed.urgentBorderColor   = myUrgentColor
                 }

getFromXres :: String -> IO String
getFromXres key = fromMaybe "" . findValue key <$> runProcessWithInput
  "xrdb"
  ["-query"]
  ""
 where
  findValue :: String -> String -> Maybe String
  findValue xresKey xres =
    snd <$> find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

  splitAtColon :: String -> Maybe (String, String)
  splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

  splitAtTrimming :: String -> Int -> (String, String)
  splitAtTrimming str idx = bimap trim (trim . tail) $ splitAt idx str

  trim :: String -> String
  trim = dropWhileEnd isSpace . dropWhile isSpace

fromXres :: String -> String
fromXres = unsafePerformIO . getFromXres

-------------------------------------------------------------------------------
-- Workspaces
--
myWorkspaces :: [String]
myWorkspaces = ["home", "hide", "misc"]

myProjects :: [Project]
myProjects =
  [ Project { projectName      = "acuity"
            , projectDirectory = "~/Documents/acuity"
            , projectStartHook = Just $ spawn $ myTerminalAt "~/Documents/acuity"
            }
  , Project { projectName      = "valor"
            , projectDirectory = "~/Documents/valor"
            , projectStartHook = Just $ spawn $ myEditor "~/Documents/valor/index.md"
            }
  , Project
    { projectName      = "www"
    , projectDirectory = "~/Downloads"
    , projectStartHook = Just $ do
                           sendMessage FirstLayout
                           spawn myBrowser
    }
  , Project
    { projectName      = "mail"
    , projectDirectory = "~/mail"
    , projectStartHook = Just $ do
                           sendMessage FirstLayout
                           spawn $ myWebapp "https://mail.google.com/mail/u/0/"
                           spawn $ myWebapp "https://mail.google.com/mail/u/1/"
                           spawn myMailReader
    }
  , Project
    { projectName      = "chat"
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
      sendMessage FirstLayout
      spawn "slack"
      spawn "Discord"
      spawn "Signal"
      spawn $ myWebapp "https://messages.google.com/web/conversations"
      spawn $ myWebapp "https://web.whatsapp.com"
      spawn $ myWebapp "https://messenger.com"
    }
  , Project
    { projectName      = "spt"
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           sendMessage FirstLayout
                           spawn $ myTerminal ++ " spt"
                                           -- spawn "spotify"
    }
  , Project { projectName      = "research"
            , projectDirectory = "~/research"
            , projectStartHook = Just $ spawn $ myTerminalAt "~/research"
            }
  , Project { projectName      = "tinker"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn myTerminal
            }
  , boringProject "gaming"
  , boringProject "video"

  -- screenshots
  -- zoom
  ]
  where boringProject name = Project name "~/" Nothing

promptDesktop :: String -> X String
promptDesktop prompt = do
  wins    <- readUrgents
  history <- workspaceHistory
  myDmenu prompt
    $  nub
    $  urgent wins
    ++ tail history
    ++ myWorkspaces
    ++ map projectName myProjects
  where urgent wins = []

switchDesktop :: String -> X ()
switchDesktop name
  | name `elem` myWorkspaces = windows $ SS.greedyView name
  | otherwise = do
    history <- workspaceHistory
    proj    <- lookupProject name
    if listToMaybe history == Just name
      then forM_ proj activateProject
      else forM_ proj switchProject

shiftToDesktop :: String -> X ()
shiftToDesktop name
  | name `elem` myWorkspaces = windows $ SS.shift name
  | otherwise = do
    proj <- lookupProject name
    forM_ proj shiftToProject

doShiftToDesktop :: Bool -> String -> XMonad.ManageHook
doShiftToDesktop chase name
  | name `elem` myWorkspaces = shift
  | otherwise                = liftX (addHiddenWorkspace name) >> shift
 where
  shift | chase     = doShift name
        | otherwise = doF $ SS.shift name

-------------------------------------------------------------------------------
-- Layouts
--
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True


floatFront :: SS.RationalRect
floatFront = SS.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

myLayoutHook :: ModifiedLayout _ _ Window
myLayoutHook =
  avoidStruts . mkToggle (single MIRROR) $ noBorders tabs ||| stack

 where
  stack =
    renamed [Replace "stack"]
      $ windowNavigation
      $ limitWindows 2
      $ mySpacing 2
      $ Tall 1 (3 / 100) (1 / 2)
  tabs = renamed [Replace "tabs"] $ Tabbed.tabbed Tabbed.shrinkText myTabTheme
-------------------------------------------------------------------------------
-- Keybinds
--
myKeys :: [(String, X ())]
myKeys =
  [ ("M-z"                    , xmonadRecompile)
  , ("M-S-z z"                , xmonadRestart)
  , ("M-S-z M-S-z"            , xmonadRestart)
  , ("M-S-z q"                , io exitSuccess)  -- Quit XMonad
  , ("M-l"                    , spawn "dunstctl close-all")
  , ("M-S-l"                  , spawn "xset s activate")

  -- Query
  , ("M-q d"                  , mySpawn "word-lookup")

  -- Application spawning
  , ("M-S-a"                  , spawn "rofi -show drun")
  , ("M-a d"                  , mySpawn "rofi-define")
  , ("M-a p"                  , spawn "rofi-pass")
  , ("M-a f"                  , spawn "thunar")

  -- Direct spawning
  , ("M-;"                    , spawn myTerminal)
  , ("M-S-;"                  , spawn "rofi -show run")
  , ("M-g"                    , spawn myBrowser')
  , ("M-S-g", spawn $ myBrowser' ++ " --incognito")

  -- Workspaces
  , ("M-/"                    , spawn "rofi -show windowcd")
  , ("M-S-/"                  , spawn "rofi -show window")
  , ("M-s", promptDesktop "Switch to" >>= switchDesktop)
  , ("M-S-s", promptDesktop "Shift to" >>= shiftToDesktop)

  -- , ("M-<Right>",      moveTo Next nonNSP)
  -- , ("M-<Left>",       moveTo Prev nonNSP)
  -- , ("M-S-<Right>",    shiftTo Next nonNSP >> moveTo Next nonNSP)
  -- , ("M-S-<Left>",     shiftTo Prev nonNSP >> moveTo Prev nonNSP)

  -- , ("M-S-.",          nextScreen)
  -- , ("M-S-,",          prevScreen)
  -- , ("M-S-h",          sendMessage Shrink)
  -- , ("M-S-l",          sendMessage Expand)

  -- Window management
  , ("M-w"                    , kill1)
  , ("M-S-w"                  , killAll)
  , ("M-t"                    , withFocused $ windows . SS.sink)
  , ("M-S-t", withFocused $ windows . (`SS.float` floatFront))

  -- Window navigation
  , ("M-j"                    , windows SS.focusDown)     -- Move focus to the next window
  , ("M-k"                    , windows SS.focusUp)       -- Move focus to the prev window
  , ("M-S-j"                  , windows SS.swapDown)      -- Swap focused window with next window
  , ("M-S-k"                  , windows SS.swapUp)        -- Swap focused window with prev window
  , ("M-r"                    , rotSlavesDown)            -- Rotate all windows except master
  , ("M-S-r"                  , rotSlavesUp)              -- Rotate all windows except master
  -- , ("M-S-n", rotAllUp)                 -- Rotate all windows
  -- , ("M-S-p", rotAllDown)               -- Rotate all windows
  , ("M-h"                    , windows SS.focusMaster)   -- Move focus to the master window
  , ("M-S-h"                  , windows SS.swapMaster)   -- Swap the focused window and the master window
  -- , ("M-S-h", promote)                  -- Move focused window to master pane, but maintain order
  , ("M-m"                    , sendMessage NextLayout)       -- Toggle layout
  , ("M-S-m"                  , sendMessage $ Toggle MIRROR)  -- Mirror layout
  , ("M-<Enter>", focusUrgent >> windows SS.swapMaster) -- Move focus to urgent window

  -- Layouts

  -- , ("M-.", increaseLimit)              -- Increase number of windows that can be shown
  -- , ("M-,", decreaseLimit)              -- Decrease number of windows that can be shown
  , ("M-'"                    , mySpawn "mediactl play-pause")
  , ("M-["                    , mySpawn "mediactl previous")
  , ("M-]"                    , mySpawn "mediactl next")
  , ("M-S-'"                  , mySpawn "mediactl here")
  , ("<XF86AudioPlay>"        , mySpawn "mediactl play-pause")
  , ("<XF86AudioPrev>"        , mySpawn "mediactl previous")
  , ("<XF86AudioNext>"        , mySpawn "mediactl next")
  , ("M--"                    , spawn "amixer sset Master 10%-")
  , ("M-="                    , spawn "amixer sset Master 10%+")
  , ("M-S-="                  , spawn "amixer sset Master 0%")
  , ("<XF86AudioLowerVolume>" , spawn "amixer sset Master 10%-")
  , ("<XF86AudioRaiseVolume>" , spawn "amixer sset Master 10%+")
  , ("<XF86AudioMute>"        , spawn "amixer sset Master 0%")
  , ("<XF86MonBrightnessUp>"  , spawn "brightnessctl set +10%")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set -10%")
  , ("<Print>"                , mySpawn "screenshot")
  , ("S-<Print>"              , mySpawn "screenshot --fullscreen")
  , ("M-x"                    , mySpawn "screenshot")
  , ("M-S-x"                  , mySpawn "screenshot --fullscreen")
  , ("M-c"                    , mySpawn "screenshot --no-delete")
  , ("M-S-c", mySpawn "screenshot --fullscreen --no-delete")
  -- TODO: make M-v fullscreen mode
  ]
 where
  nonNSP :: WSType
  nonNSP = WSIs (return (\ws -> SS.tag ws /= "nsp"))

  xmonadRecompile, xmonadRestart :: X ()
  xmonadRecompile =
    spawn "xmonad --recompile && notify-send XMonad 'recompiled successfully'"
  xmonadRestart =
    spawn
      $  "xmonad --recompile"
      ++ " && xmonad --restart"
      ++ " && sleep 1"
      ++ " && polybar-msg cmd restart"
      ++ " && notify-send XMonad 'restarted successfully'"

-------------------------------------------------------------------------------
-- Bar
--
mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  _    <- D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = D.emit dbus $ signal { D.signalBody = body }
 where
  opath  = D.objectPath_ "/org/xmonad/Log"
  iname  = D.interfaceName_ "org.xmonad.Log"
  mname  = D.memberName_ "Update"
  signal = D.signal opath iname mname
  body   = [D.toVariant $ UTF8.decodeString str]

polybarHook :: D.Client -> X ()
polybarHook dbus = do
  xs <- get
  Log.dynamicLogWithPP $ ppHook $ windowCount xs

 where
  ppHook c = def { Log.ppOutput  = dbusOutput dbus
                 , Log.ppCurrent = wrapper myFgColor'
                 , Log.ppVisible = wrapper myFgColor
                 , Log.ppUrgent  = wrapper myUrgentColor
                 , Log.ppHidden  = Log.wrap " " " "
                 , Log.ppWsSep   = ""
                 , Log.ppSep     = " | "
                 , Log.ppTitle   = const ""
                 , Log.ppLayout  = (++ " (" ++ show c ++ ")")
                 }
  wrapper c s | s /= "NSP" = Log.wrap (" %{F" ++ c ++ "} ") " %{F-} " s
              | otherwise  = mempty

  windowCount =
    maybe 0 countWins . SS.stack . SS.workspace . SS.current . Core.windowset
    where countWins s = 1 + length (SS.up s) + length (SS.down s)

-------------------------------------------------------------------------------
-- Startup
--
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"  -- Apparently useful for making Java GUI programs work
  spawnOnce "polybar top &"
  spawnOnce "polybar bottom &"
  spawnOnce "dunst &"
  spawn "xsetroot -cursor_name left_ptr &"
  spawn "xset r rate 250 69 &"

-------------------------------------------------------------------------------
-- Window management
--
myManageHook :: XMonad.ManageHook
myManageHook =
  composeAll
      [ isFullscreen --> doFullFloat
      , transience'
      , className =? "Gcr-prompter" --> doCenterFloat

  -- chasing doesn't seem to work
      , className =? "qutebrowser" --> doShiftToDesktop True "www"

  -- chasing doesn't seem to work
      , className =? "Steam" --> doShiftToDesktop False "gaming"
      , className =? "Slack" --> doShiftToDesktop True "chat"
      , className =? "discord" --> doShiftToDesktop True "chat"

  -- only works with spotifywm
      , className =? "spotify" --> doShiftToDesktop True "spt"
      ]
    <+> zoomShenanigans
 where
  zoomShenanigans = composeOne
    [ -- Zoom home; chasing doesn't seem to work, but I don't care about it
      (className =? "zoom" <&&> title =? "Zoom - Free Account")
      -?> doShiftToDesktop False "chat"

    -- Meeting window; chasing doesn't seem to work, but Zoom will pop a floating notification
    , (className =? "zoom" <&&> title =? "Zoom")
      -?> doShiftToDesktop True "video"

    -- Leave certain meeting windows tiled
    , (className =? "zoom" <&&> title =? "Chat") -?> mempty
    , (className =? "zoom" <&&> fmap ("Participants" `isPrefixOf`) title)
      -?> mempty

    -- Float all other Zoom windows
    , (className =? "zoom") -?> doFloat
    ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount where fadeAmount = 0.9

-------------------------------------------------------------------------------
-- Main
--
main :: IO ()
main = do
  dbus <- mkDbusClient
  xmonad $ cfg dbus

 where
  cfg d =
    dynamicProjects myProjects
      $ withUrgencyHook NoUrgencyHook
      $ fullscreenSupport
      $ ewmh
          --  $ Debug.debugManageHook
      $ def { manageHook         = manageDocks <+> myManageHook
            , handleEventHook    = docksEventHook
            , modMask            = myModMask
            , terminal           = myTerminal
            , startupHook        = myStartupHook
            , layoutHook         = fullscreenFocus myLayoutHook
            , workspaces         = myWorkspaces
            , borderWidth        = myBorderWidth
            , normalBorderColor  = myBgColor
            , focusedBorderColor = myBgColor'
            , focusFollowsMouse  = False
            , logHook = myLogHook <+> workspaceHistoryHook <+> polybarHook d
            , keys               = (`mkKeymap` myKeys)
            }

--  vim: set ts=2 sw=2 tw=120 et :

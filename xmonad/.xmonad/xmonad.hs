{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhileEnd
                                                , elemIndex
                                                , find
                                                , isPrefixOf
                                                , nub
                                                )
import qualified Data.Map                     as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           System.Exit                    ( exitSuccess )
import           System.IO.Unsafe               ( unsafePerformIO )

import           XMonad                         ( (-->)
                                                , (<&&>)
                                                , (<+>)
                                                , (=?)
                                                , ChangeLayout (..)
                                                , Default(def)
                                                , Dimension
                                                , KeyMask
                                                , ManageHook
                                                , MonadState(get)
                                                , Resize(..)
                                                , Tall(..)
                                                , Full(..)
                                                , Window
                                                , X
                                                , XConfig(..)
                                                , className
                                                , composeAll
                                                , doFloat
                                                , gets
                                                , io
                                                , mod4Mask
                                                , sendMessage
                                                , spawn
                                                , title
                                                , windows
                                                , withFocused
                                                , xmonad
                                                , (|||)
                                                )
import qualified XMonad.Core                   as Core
import qualified XMonad.StackSet               as SS

import           XMonad.Actions.CopyWindow      ( kill1 )
import           XMonad.Actions.CycleWS         ( WSType(..)
                                                , nextScreen
                                                , prevScreen
                                                , shiftNextScreen
                                                , shiftPrevScreen
                                                , swapNextScreen
                                                , swapPrevScreen
                                                )
import           XMonad.Actions.RotSlaves       ( rotSlavesDown
                                                , rotSlavesUp
                                                )
import           XMonad.Actions.WindowBringer   ( bringMenuArgs' )
import           XMonad.Actions.WithAll         ( killAll )

import           XMonad.Hooks.DebugStack        ( debugStack )
import qualified XMonad.Hooks.DynamicLog       as Log
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.FadeInactive      ( fadeInactiveLogHook )
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


import qualified XMonad.Layout.BoringWindows   as Boring

-- Layouts modifiers
import           XMonad.Layout.Fullscreen       (fullscreenEventHook
                                                , fullscreenManageHook
                                                , fullscreenFocus
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
                                                ( StdTransformers(..) )
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
import           XMonad.Util.NamedWindows       ( getName, unName )
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

myBrowser :: String
myBrowser = "qutebrowser "

myBrowser' :: String
myBrowser' = "google-chrome-stable "

myWebapp :: String -> String
myWebapp url = "google-chrome-stable --new-window --app=" ++ url

myBorderWidth :: Dimension
myBorderWidth = 2

myScript :: String -> String
myScript s = "~/.local/tms/" ++ s

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

myAccentColor :: String
myAccentColor = fromXres "*color6"

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

fromXres :: String -> String
fromXres = unsafePerformIO . getFromXres

-------------------------------------------------------------------------------
-- Combinators and Helpers
--

nonEmptyStr :: String -> Maybe String
nonEmptyStr s
  | null $ trim s = Nothing
  | otherwise     = Just s

onJust :: b -> (a -> X b) -> Maybe a -> X b
onJust b f (Just a) = f a
onJust b _ Nothing  = return b

-- | X combinator for piping around strings. Fails if empty.
(>|=) :: X String -> (String -> X ()) -> X ()
xs >|= f = xs >>= return . nonEmptyStr >?= f

(>?=) :: X (Maybe a) -> (a -> X ()) -> X ()
xa >?= f = xa >>= onJust () f

infixl 1 >|=
infixl 1 >?=

dmenuName :: String
dmenuName = "rofi"

dmenuArgs :: String -> [String]
dmenuArgs p = ["-monitor", "-4", "-dmenu", "-p", p]

myDmenu_ :: String -> [String] -> X String
myDmenu_ prompt = Dmenu.menuArgs dmenuName (dmenuArgs prompt)

myDmenu :: String -> M.Map String a -> X (Maybe a)
myDmenu prompt = Dmenu.menuMapArgs dmenuName (dmenuArgs prompt)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-------------------------------------------------------------------------------
-- Workspaces
--
myWorkspaces :: [String]
myWorkspaces = zipWith (\i n -> show i ++ ":" ++ n) [1..]
  [ "dev"     -- ^ Work-related software development
  , "doc"     -- ^ Work-related reading and writing
  , "cfg"     -- ^ Tinkering and config file editing
  , "fun"     -- ^ Work-unrelated software development, reading, etc.
  , "life"    -- ^ Life management
  , "chat"    -- ^ Ongoing conversations
  , "media"   -- ^ Multimedia applications, e.g., Spotify, video player, etc.
  , "idle"    -- ^ Applications that run in the background, e.g., monitoring
  ]

promptDesktop :: String -> X String
promptDesktop prompt = do
  wins    <- readUrgents
  history <- workspaceHistory
  myDmenu_ prompt
    $  nub
    $  urgent wins
    ++ tail history
    ++ myWorkspaces
    -- ++ map projectName myProjects
  where urgent wins = []

switchDesktop :: String -> X ()
switchDesktop = windows . SS.greedyView

shiftToDesktop :: String -> X ()
shiftToDesktop = windows . SS.shift

promptWindow :: String -> X (Maybe Window)
promptWindow p = do
  winIds <- SS.index <$> gets Core.windowset
  wins <- M.fromList . map mkPair <$> mapM getName winIds
  fmap unName <$> myDmenu p wins
    where mkPair w = ("(" ++ show (unName w) ++ ") " ++ show w, w)

bringWin :: (Eq a, Show a) => a -> SS.Stack a -> SS.Stack a
bringWin a s@SS.Stack { SS.focus = f, SS.up = u, SS.down = d }
  | a == f      = s
  | a `elem` u  = s { SS.focus = a, SS.up = u' ++  f : tail d' }
  | a `elem` d  = s { SS.focus = a, SS.down = f : filter (/= a) d }
  | otherwise   = s
   where (u', d') = span (/= a) u

-------------------------------------------------------------------------------
-- Layouts
--
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True


floatFront :: SS.RationalRect
floatFront = SS.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

myLayoutHook :: ModifiedLayout _ _ Window
myLayoutHook = avoidStruts
             $ mkToggle (single FULL)
             $ mkToggle (single MIRROR)
             $ Boring.boringWindows (noBorders full) ||| Boring.boringAuto stack
 where
  stack =
    renamed [Replace "stack"]
      $ windowNavigation
      $ limitWindows 2
      $ mySpacing 2
      $ Tall 1 (3 / 100) (1 / 2)
  full = renamed [Replace "full"]
          $ windowNavigation
          $ Full

-------------------------------------------------------------------------------
-- Keybinds
--
myKeys :: [(String, X ())]
myKeys =
  -- The following keys are unbound:
  -- - b/f
  -- - e
  -- - d
  -- - v
  -- - n/p
  -- - o
  [ ("M-z z"                  , xmonadRecompile)
  , ("M-z r"                  , xmonadRestart)
  , ("M-z q"                  , io exitSuccess)  -- Quit XMonad
  , ("M-z d"                  , debugStack)

  , ("M-q"                    , spawn "dunstctl close-all")
  , ("M-S-q"                  , spawn "xset s activate")

  -- Applications
  , ("M-;"                    , spawn "rofi -monitor -4 -show drun")
  , ("M-S-;"                  , spawn "rofi -monitor -4 -show run")

  , ("M-<Return>"             , spawn myTerminal)
  , ("M-g"                    , spawn myBrowser')
  , ("M-S-g"                  , spawn $ myBrowser' ++ " --incognito")
  , ("M-i"                    , spawn $ myScript "passman")
  , ("M-S-i"                  , spawn "thunar")
  , ("M-S-a"                  , spawn $ myScript "cmd-launcher")

  -- Workspaces
  , ("M-/"                    , spawn "rofi -monitor -4 -show windowcd")
  , ("M-S-/"                  , spawn "rofi -monitor -4 -show window")
  , ("M-s"                    , promptDesktop "Switch to" >>= switchDesktop)
  , ("M-S-s"                  , promptDesktop "Shift to" >>= shiftToDesktop)
  , ("M-o"                    , promptWindow "Get window" >?= windows . SS.modify' . bringWin)
  , ("M-S-o"                  , bringMenuArgs' dmenuName (dmenuArgs "bring"))

  , ("M-.",                     nextScreen)
  , ("M-,",                     prevScreen)
  , ("M-S-.",                   shiftNextScreen)
  , ("M-S-,",                   shiftPrevScreen)
  , ("M-`",                     swapNextScreen)
  , ("M-S-`",                   swapPrevScreen)

  -- Window management
  , ("M-w"                    , kill1)
  , ("M-S-w"                  , killAll)
  , ("M-t"                    , withFocused $ windows . SS.sink)
  , ("M-S-t"                  , withFocused $ windows . (`SS.float` floatFront))

  -- Window navigation
  , ("M-j"                    , Boring.focusDown)       -- Move focus to the next window
  , ("M-k"                    , Boring.focusUp)         -- Move focus to the prev window
  , ("M-S-j"                  , rotSlavesDown)          -- Rotate all windows except master
  , ("M-S-k"                  , rotSlavesUp)            -- Rotate all windows except master
  , ("M-m"                    , Boring.focusMaster)     -- Move focus to the master window
  , ("M-S-m"                  , windows SS.swapMaster)  -- Swap the focused window and the master window

  , ("M-y"                    , sendMessage NextLayout)       -- Toggle layout
  , ("M-S-y"                  , sendMessage $ Toggle MIRROR)  -- Mirror layout
  , ("M-S-u"                  , sendMessage $ Toggle FULL)    -- Toggle on full layout
  , ("M-u"                    , focusUrgent >> windows SS.swapMaster) -- Move focus to urgent window

  , ("M-h"                    , sendMessage Shrink)
  , ("M-l"                    , sendMessage Expand)
  , ("M-S-h"                  , decreaseLimit)              -- Decrease number of windows that can be shown
  , ("M-S-l"                  , increaseLimit)              -- Increase number of windows that can be shown
  , ("M-'"                    , spawn $ myScript "mediactl play-pause")
  , ("M-["                    , spawn $ myScript "mediactl previous")
  , ("M-]"                    , spawn $ myScript "mediactl next")
  , ("M-S-'"                  , spawn $ myScript "mediactl here")
  , ("<XF86AudioPlay>"        , spawn $ myScript "mediactl play-pause")
  , ("<XF86AudioPrev>"        , spawn $ myScript "mediactl previous")
  , ("<XF86AudioNext>"        , spawn $ myScript "mediactl next")
  , ("M--"                    , spawn "amixer sset Master 10%-")
  , ("M-="                    , spawn "amixer sset Master 10%+")
  , ("M-S-="                  , spawn "amixer sset Master 0%")
  , ("<XF86AudioLowerVolume>" , spawn "amixer sset Master 10%-")
  , ("<XF86AudioRaiseVolume>" , spawn "amixer sset Master 10%+")
  , ("<XF86AudioMute>"        , spawn "amixer sset Master 0%")
  , ("<XF86MonBrightnessUp>"  , spawn "brightnessctl set 10%+")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
  , ("<Print>"                , spawn $ myScript "screenshot")
  , ("S-<Print>"              , spawn $ myScript "screenshot --fullscreen")
  , ("M-x"                    , spawn $ myScript "screenshot")
  , ("M-S-x"                  , spawn $ myScript "screenshot --fullscreen")
  , ("M-c"                    , spawn $ myScript "screenshot --no-delete")
  , ("M-S-c"                  , spawn $ myScript "screenshot --fullscreen --no-delete")
  ] ++ map viewWorkspace workspaceKW ++ map shiftWorkspace workspaceKW
 where
  nonNSP :: WSType
  nonNSP = WSIs $ return $ (/= "nsp") . SS.tag

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

  viewWorkspace (k, w) = ("M-" ++ show k, switchDesktop w)
  shiftWorkspace (k, w) = ("M-S-" ++ show k, shiftToDesktop w)
  workspaceKW = take 9 $ zip [1..] myWorkspaces

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
polybarHook dbus = get >>= Log.dynamicLogWithPP . ppHook
 where
  ppHook xs = def { Log.ppOutput          = dbusOutput dbus
                  , Log.ppCurrent         = wrapper myFgColor' " {" "} "
                  , Log.ppVisible         = wrapper myFgColor " [" "] "
                  , Log.ppUrgent          = wrapper myUrgentColor " <" "> "
                  , Log.ppHidden          = Log.wrap "  " "  "
                  , Log.ppHiddenNoWindows = wrapper myBgColor' "  " "  "
                  , Log.ppWsSep           = ""
                  , Log.ppSep             = " | "
                  , Log.ppTitle           = const ""
                  , Log.ppLayout          = (++ " (" ++ windowCount xs ++ ")")
                  }
  wrapper c b a s
    | s == "nsp" = mempty
    | otherwise  = Log.wrap (b ++ "%{F" ++ c ++ "}") ("%{F-}" ++ a) s
  windowCount = show . length . SS.index . Core.windowset

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
      , className =? "float-term"   --> doCenterFloat

      -- , className =? "qutebrowser" --> doShiftToDesktop "www"

      -- , className =? "Slack" --> doShiftToDesktop "chat"
      -- , className =? "discord" --> doShiftToDesktop "chat"

      -- , className =? "spotify" --> doShiftToDesktop "spt"
      ]
    <+> zoomShenanigans
 where
  zoomShenanigans = composeOne
    [ -- Zoom home; chasing doesn't seem to work, but I don't care about it
      -- (className =? "zoom" <&&> title =? "Zoom - Free Account") -?> doShiftToDesktop "chat"

    -- Meeting window; chasing doesn't seem to work, but Zoom will pop a floating notification
    -- , (className =? "zoom" <&&> title =? "Zoom") -?> doShiftToDesktop "video"

    -- Leave certain meeting windows tiled
      (className =? "zoom" <&&> title =? "Chat") -?> mempty
    , (className =? "zoom" <&&> fmap ("Participants" `isPrefixOf`) title) -?> mempty

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
    -- dynamicProjects myProjects
      withUrgencyHook NoUrgencyHook
      $ fullscreenSupport
      $ ewmh
          --  $ Debug.debugManageHook
      $ def { manageHook         = manageDocks <+> fullscreenManageHook <+> myManageHook
            , handleEventHook    = docksEventHook <+> fullscreenEventHook
            , modMask            = myModMask
            , terminal           = myTerminal
            , startupHook        = myStartupHook
            , layoutHook         = fullscreenFocus myLayoutHook
            , workspaces         = myWorkspaces
            , borderWidth        = myBorderWidth
            , normalBorderColor  = myBgColor
            , focusedBorderColor = myFgColor
            , focusFollowsMouse  = False
            , logHook = myLogHook <+> workspaceHistoryHook <+> polybarHook d
            , keys               = (`mkKeymap` myKeys)
            }

--  vim: set ts=2 sw=2 tw=120 et :

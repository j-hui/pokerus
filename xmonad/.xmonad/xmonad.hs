{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wunused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Char                      ( isSpace )
import           Data.Function                  ( (&) )
import           Data.List                      ( dropWhileEnd
                                                , elemIndex
                                                , find
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Monoid                    ( All )
import           System.Exit                    ( exitSuccess )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Text.Read                      ( readMaybe )

import qualified XMonad.Core                   as Core
import qualified XMonad.Hooks.DynamicLog       as Log
import qualified XMonad.Layout.BoringWindows   as Boring
import qualified XMonad.StackSet               as SS
import qualified XMonad.Util.Dmenu             as Dmenu
import qualified XMonad.Util.Loggers           as Log

import           XMonad
import           XMonad.Actions.CycleWS         ( nextScreen
                                                , prevScreen
                                                , shiftNextScreen
                                                , shiftPrevScreen
                                                , swapNextScreen
                                                , swapPrevScreen
                                                )
import           XMonad.Actions.RotSlaves       ( rotSlavesDown
                                                , rotSlavesUp
                                                )
import           XMonad.Actions.WindowBringer   ( bringWindow )
import           XMonad.Hooks.DebugStack        ( debugStack )
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.FadeWindows       ( FadeHook
                                                , fadeWindowsEventHook
                                                , fadeWindowsLogHook
                                                , isUnfocused
                                                , opaque
                                                , transparency
                                                )
import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                )
import           XMonad.Hooks.ManageHelpers     ( (-?>)
                                                , composeOne
                                                , doCenterFloat
                                                , doFullFloat
                                                , doRectFloat
                                                , isDialog
                                                , isFullscreen
                                                , isInProperty
                                                , transience
                                                )
import           XMonad.Hooks.ServerMode        ( serverModeEventHookF )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Hooks.UrgencyHook       ( NoUrgencyHook(..)
                                                , focusUrgent
                                                , withUrgencyHook
                                                )
import           XMonad.Hooks.WorkspaceHistory  ( workspaceHistory
                                                , workspaceHistoryHook
                                                )
import           XMonad.Layout.Fullscreen       ( fullscreenEventHook
                                                , fullscreenFocus
                                                , fullscreenManageHook
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

myBorderWidth :: Dimension
myBorderWidth = 2

myTerminal :: String
myTerminal = "alacritty"

notifySend :: String -> X ()
notifySend msg = spawn $ "notify-send XMonad '" ++ msg ++ "'"

notifyErr :: String -> X ()
notifyErr msg =
  spawn $ "notify-send -u critical -t 3000 XMonad '" ++ msg ++ "'"

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

-- | Hack to obtain theme colors from .Xresources
fromXres :: String -> String
fromXres = unsafePerformIO . getFromXres
 where
  getFromXres :: String -> IO String
  getFromXres key = fromMaybe "" . findValue key <$> runQuery

  runQuery :: IO String
  runQuery = runProcessWithInput "xrdb" ["-query"] ""

  findValue :: String -> String -> Maybe String
  findValue xresKey xres =
    snd <$> find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

  splitAtColon :: String -> Maybe (String, String)
  splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

  splitAtTrimming :: String -> Int -> (String, String)
  splitAtTrimming str idx = bimap trim (trim . tail) $ splitAt idx str

-------------------------------------------------------------------------------
-- Combinators and Helpers
--

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
myWorkspaces = zipWith
  (\i n -> show i ++ ":" ++ n)
  ([1 ..] :: [Int])
  [ "dev"     -- Work-related software development
  , "doc"     -- Work-related reading and writing
  , "cfg"     -- Tinkering and config file editing
  , "fun"     -- Work-unrelated software development, reading, etc.
  , "life"    -- Life management
  , "chat"    -- Ongoing conversations
  , "media"   -- Multimedia applications, e.g., Spotify, video player, etc.
  , "idle"    -- Applications that run in the background, e.g., monitoring
  ]

switchDesktop :: String -> X ()
switchDesktop = windows . SS.greedyView

shiftToDesktop :: String -> X ()
shiftToDesktop = windows . SS.shift

readWindow :: String -> Maybe Window
readWindow = readMaybe

lastWorkspace :: X (Maybe String)
lastWorkspace = do
  history <- workspaceHistory
  case history of
    (_ : w : _) -> return $ Just w
    _           -> return Nothing

viewLastWorkspace :: X ()
viewLastWorkspace = lastWorkspace >>= maybe (return ()) switchDesktop

shiftLastWorkspace :: Window -> X ()
shiftLastWorkspace f = lastWorkspace >>= maybe (return ()) shiftToDesktop

-------------------------------------------------------------------------------
-- Layouts
--
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

floatFront :: SS.RationalRect
floatFront = SS.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

floatBottom :: SS.RationalRect
floatBottom = SS.RationalRect 0 (1 / 2) 1 (1 / 2)

myLayoutHook :: ModifiedLayout _ _ Window
myLayoutHook =
  avoidStruts
    $   mkToggle (single MIRROR)
    $   Boring.boringAuto stack
    ||| Boring.boringWindows (noBorders full)
 where
  stack =
    renamed [Replace "S"]
      $ windowNavigation
      $ limitWindows 3
      $ mySpacing 2
      $ Tall 1 (3 / 100) (1 / 2)
  full = renamed [Replace "F"] $ windowNavigation Full

-------------------------------------------------------------------------------
-- Keybinds
--
myKeys :: [(String, X ())]
myKeys =
  [ ("M-z z", xmonadRecompile)
    , ("M-z r", xmonadRestart)
    , ("M-z q", io exitSuccess)  -- Quit XMonad
    , ("M-z d", debugStack)

  -- Screens and workspaces
    , ("M-."  , nextScreen)
    , ("M-,"  , prevScreen)
    , ("M-S-.", shiftNextScreen)
    , ("M-S-,", shiftPrevScreen)
    , ("M-`"  , swapNextScreen)
    , ("M-S-`", swapPrevScreen)
    , ("M-l"  , viewLastWorkspace)
    , ("M-S-l", withFocused shiftLastWorkspace)

  -- Window management
    , ("M-t"  , withFocused $ windows . SS.sink)
    , ("M-S-t", withFocused $ windows . (`SS.float` floatFront))

  -- Window navigation
    , ("M-j"  , Boring.focusDown)       -- Move focus to the next window
    , ("M-k"  , Boring.focusUp)         -- Move focus to the prev window
    , ("M-h"  , Boring.focusMaster)     -- Move focus to the master window
    , ("M-S-h", windows SS.swapMaster)  -- Swap the focused window and the master window
    , ("M-S-j", rotSlavesDown)          -- Rotate all windows except master
    , ("M-S-k", rotSlavesUp)            -- Rotate all windows except master
    , ("M-u"  , focusUrgent >> windows SS.swapMaster) -- Focus urgent window

  -- Layout management
    , ("M-i"  , decreaseLimit)
    , ("M-o"  , increaseLimit)
    , ("M-S-i", sendMessage Shrink)
    , ("M-S-o", sendMessage Expand)
    , ("M-m"  , sendMessage NextLayout)
    , ("M-S-m", sendMessage $ Toggle MIRROR)  -- Mirror layout
    ]
    ++ map viewWorkspace  workspaceKW
    ++ map shiftWorkspace workspaceKW
 where
  xmonadRecompile, xmonadRestart :: X ()
  xmonadRecompile = spawn "~/.xmonad/scripts/xmoctl recompile"
  xmonadRestart   = spawn "~/.xmonad/scripts/xmoctl restart"

  viewWorkspace (k, w) = ("M-" ++ show k, switchDesktop w)
  shiftWorkspace (k, w) = ("M-S-" ++ show k, shiftToDesktop w)
  workspaceKW = take 9 $ zip ([1 ..] :: [Int]) myWorkspaces

myServerEventHook :: Event -> X All
myServerEventHook = serverModeEventHookF "XMONAD_COMMAND" (handle . words)
 where
  handle (   "bring" : a : _) = whenJust (readWindow a) (windows . bringWindow)
  handle (   "send"  : a : _) = windows $ SS.shift a
  handle as@(_           : _) = notifyErr $ "Unknown command: " ++ unwords as
  handle []                   = notifyErr "Empty command"

-------------------------------------------------------------------------------
-- Status bar
--
mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  _    <- D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

dbusOutput :: D.Client -> String -> X ()
dbusOutput dbus s = io $ D.emit dbus $ signal { D.signalBody = body s }
 where
  opath  = D.objectPath_ "/org/xmonad/Log"
  iname  = D.interfaceName_ "org.xmonad.Log"
  mname  = D.memberName_ "Update"
  signal = D.signal opath iname mname
  body s = [D.toVariant $ UTF8.decodeString s]

type ColorFn
  =  String -- ^ Foreground color
  -> String -- ^ Background color
  -> String -- ^ Output string
  -> String

-- | Format string with color for polybar; background color not supported.
polybarColor :: ColorFn
polybarColor fc _ = Log.wrap ("%{F" ++ fc ++ "}") "%{F-}"

barPP :: ColorFn -> Int -> Log.PP
barPP color windowCount = def
  { Log.ppCurrent         = color myFgColor' "" . Log.wrap "{" "}"
  , Log.ppVisible         = color myFgColor "" . Log.wrap "[" "]"
  , Log.ppUrgent          = color myUrgentColor "" . Log.wrap "<" ">"
  , Log.ppHidden          = Log.wrap "" ""
  , Log.ppHiddenNoWindows = color myBgColor' "" . Log.wrap "" ""
  , Log.ppWsSep           = " "
  , Log.ppSep             = " | "
  , Log.ppTitle           = color myFgColor ""
  , Log.ppLayout          = (++ "/" ++ show windowCount)
  }

getWindowCount :: X Int
getWindowCount = length . SS.index <$> gets Core.windowset

getTitle :: X String
getTitle = fromMaybe "" <$> Log.logTitle

polybarHook :: D.Client -> X ()
polybarHook dbus = do
  getWindowCount
    >>= Log.dynamicLogString
    .   barPP polybarColor
    >>= dbusOutput dbus

xmobarLogHook :: X ()
xmobarLogHook = do
  wc <- getWindowCount
  let pp = barPP Log.xmobarColor wc
  Log.dynamicLogString pp { Log.ppTitle = const "" } >>= Log.xmonadPropLog
  getTitle >>= Log.xmonadPropLog' "_XMONAD_TITLE" . Log.ppTitle pp


-------------------------------------------------------------------------------
-- Startup
--
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"  -- Apparently useful for making Java GUI programs work
  spawnOnce "~/.local/tms/startup-launch"

-------------------------------------------------------------------------------
-- Window management
--
myManageHook :: XMonad.ManageHook
myManageHook =
  composeOne
    $  [ isFullscreen -?> doFullFloat
       , transience
       , isDialog -?> doFloat
       , className =? "Gcr-prompter" -?> doCenterFloat
       , className =? "float-term" -?> doRectFloat floatBottom
       ]
    ++ zoomShenanigans
 where
  zoomShenanigans =
    [ className
      =?   "zoom"
      <&&> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE"
      -?>  doFloat
    , className =? "zoom" <&&> title =? "Zoom - Licensed Account" -?> doFloat
    ]

myFadeHook :: FadeHook
myFadeHook = composeAll
  [ isUnfocused <&&> fmap not (className =? "float-term") --> transparency 0.1
  , className =? "float-term" --> transparency 0.1
  , fmap not isUnfocused <&&> fmap not (className =? "float-term") --> opaque
  ]

-------------------------------------------------------------------------------
-- Main
--platform/zephyr/src/ssm-timer.c
main :: IO ()
main = do
  -- d <- mkDbusClient
  config & docks & xmonad
  -- xmonad $ docks $ config d
  -- config
  -- config & docks & xmobar >>= xmonad
 where
  -- xmobar =
  --   Log.statusBar "~/.config/xmobar/xmobar.hs" Log.xmobarPP toggleStrutsKey
  -- toggleStrutsKey XConfig { modMask = modm } = (modm, xK_b)
  config = withUrgencyHook NoUrgencyHook $ fullscreenSupport $ ewmh $ def
    { manageHook         = fullscreenManageHook <+> myManageHook
    , handleEventHook    = fullscreenEventHook
                           <+> myServerEventHook
                           <+> fadeWindowsEventHook
    , logHook            = xmobarLogHook
                           <+> workspaceHistoryHook
                           <+> fadeWindowsLogHook myFadeHook
    , layoutHook         = fullscreenFocus myLayoutHook
    , startupHook        = myStartupHook
    , modMask            = myModMask
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , focusedBorderColor = myFgColor
    , normalBorderColor  = myBgColor
    , focusFollowsMouse  = False
    , keys               = (`mkKeymap` myKeys)
    }

--  vim: set ts=2 sw=2 tw=120 et :

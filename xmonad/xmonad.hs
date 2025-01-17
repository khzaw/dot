import System.IO

import XMonad
import qualified XMonad.StackSet as W

import Data.Char (isSpace)
import Data.List
import Data.Monoid
import qualified Data.Map as M

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies, copyToAll, copy)
import qualified XMonad.Actions.CycleWS as WS
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.Minimize
import qualified XMonad.Actions.Search as S

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Minimize

-- Utils
import XMonad.Util.Run
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce
import qualified XMonad.Util.Hacks as Hacks

-- Layouts and Layout Modifiers
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Minimize

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
        [ className =? "1Password"                                 --> doCenterFloat
        , className =? "Nitrogen"                                  --> doCenterFloat
        , className =? "feh"                                       --> doCenterFloat
        -- , className =? "mpv"                                       --> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
        , className =? "mpv"                                       --> doShift (myWorkspaces !! 9)  -- watch
        , className =? "slack"                                     --> doShift (myWorkspaces !! 5)  -- chat
        , className =? "telegram-desktop"                          --> doShift (myWorkspaces !! 5)
        , className =? "Plugins"                                   --> doCenterFloat
        , className =? "file_progress"                             --> doFloat
        , className =? "pinentry-gtk-2"                            --> doFloat
        , className =? "toolbar"                                   --> doFloat
        , className =? "notification"                              --> doFloat
        , className =? "error"                                     --> doFloat
        , className =? "stalonetray"                               --> doIgnore
        , className =? "trayer"                                    --> doIgnore
        , className =? "calculator"                                --> doFloat
        , className =? "hl_linux"                                  --> doFloat
        , className =? "crx_nkbihfbeogaeaoehlefnkodbefgpgknn"      --> doFloat
        , (className =? "firefox" <&&> resource =? "Dialog")       --> doFloat
        , (className =? "brave-browser" <&&> resource =? "Dialog") --> doFloat
        , (className =? "zen-browser" <&&> resource =? "Dialog") --> doFloat
        , title     =? "Volume Control"                       --> doCenterFloat
        , title     =? "Bluetooth Devices"                    --> doCenterFloat
        , title     =? "Save As"                              --> doCenterFloat
        , title     =? "Save Image"                           --> doCenterFloat
        , title     =? "Save Folder to Upload"                --> doCenterFloat
        , title     =? "Enter name of file to save to"        --> doCenterFloat
        , title     =? "Picture-in-Picture"                   --> doFloat
        , title     =? "Media viewer"                         --> doFloat
        ]

underLine col = xmobarBorder "Bottom" col 3

-- StartupHook
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore"
  spawnOnce "picom -b"
  spawnOnce "udiskie -an"
  spawnOnce "setxkbmap -option caps:ctrl_modifier"
  setWMName "xmonad"

-- Variables
myTerminal :: String
myTerminal = "ghostty"

myBrowser :: String
myBrowser = "zen-browser"

myLauncher :: String
myLauncher = "rofi -show run -dpi 196 -show-icons"

myDiscord :: String
myDiscord = "discord"

myTelegram :: String
myTelegram = "telegram-desktop"

my1Password :: String
my1Password = "1password"

mySpotify :: String
mySpotify = "spotify"

myEditor :: String
myEditor = "emacs"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Key Bindings
-- M1 - Left Alt
-- M  - Mod Key (Cmd)
myKeys :: [(String, X ())]
myKeys =
  [ ("M1-<Space>", spawn myLauncher)
  , ("M1-S-3", unGrab *> spawn "scrot ~/Pictures/screenshots/")
  , ("M1-S-4", unGrab *> spawn "scrot -s ~/Pictures/screenshots/")
  , ("M-x 1", spawn my1Password)
  , ("M-x a", spawn mySpotify)
  , ("M-x b", spawn myBrowser)
  , ("M-x t", spawn myTelegram)
  , ("M-x d", spawn myDiscord)
  , ("M-x e", spawn myEditor)
  , ("M-m",   withFocused minimizeWindow) -- Minimize current window
  , ("M-S-m", withLastMinimized maximizeWindowAndFocus) -- Restore current window
  ]

-- workspaces
-- myWorkspaces are clickable by mouse. Requires `xdotool`.
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]


myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
  $ ["www", "side", "dev1", "dev2", "chat", "files", "write", "edit", "watch"]
  where
    clickable l = [ "<action=`xdotool key super+" ++ show n ++ "`>" ++ ws ++ "</action>" |
                    (i, ws) <- zip [1..9] l,
                    let n = i ]


-- Layouts
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True


main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -d"
  tray <- spawnPipe "killall -q stalonetray; sleep 1; stalonetray"
  spawn "pkill dunst; dunst"
  xmonad $ docks $ ewmh $ def {
      modMask = mod4Mask
    , workspaces = myWorkspaces
    , manageHook = myManageHook <+> manageHook def
    , layoutHook = avoidStruts $ mySpacing 10 $ minimize (layoutHook def)
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput          = hPutStrLn xmproc
      , ppCurrent         = xmobarColor "#ddbd94" "" . wrap "[" "]" -- Current workspace in xmobar
      , ppVisible         = xmobarColor "#ddbd94" ""                -- visible but not current workspace
      , ppHidden          = xmobarColor "#c15c2e" ""                -- Hidden workspaces in xmobar
      , ppHiddenNoWindows = xmobarColor "#5a8c93" ""                -- Hidden workspaces (no windows)
      , ppTitle           = xmobarColor "#8bc34a" "" . shorten 50   -- Title of active window in xmobar
      , ppSep             = "<fc=#666666> | </fc>"                  -- Separators in xmobar
      , ppExtras          = [windowCount]                           -- # of windows current workspace
      -- , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      }
    , borderWidth = 1
    , terminal = myTerminal
    , startupHook = myStartupHook
    , normalBorderColor = "#9ece6a"
    , focusedBorderColor = "#e0af68"
    , handleEventHook = handleEventHook def
      <+> minimizeEventHook
      <+> Hacks.windowedFullscreenFixEventHook
      <+> Hacks.trayPaddingXmobarEventHook (className =? "stalonetray") "_XMONAD_TRAYPAD"
  } `additionalKeysP` myKeys

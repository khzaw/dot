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
import qualified XMonad.Actions.Search as S

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- Utils
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Cursor

-- Layouts and Layout Modifiers
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier (ModifiedLayout)

myManageHook = composeAll
        [ className =? "1Password"                            --> doCenterFloat
        , className =? "Nitrogen"                             --> doCenterFloat
        , className =? "feh"                                  --> doCenterFloat
        , className =? "mpv"                                  --> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
        , className =? "Plugins"                              --> doCenterFloat
        , className =? "file_progress"                        --> doFloat
        , className =? "pinentry-gtk-2"                       --> doFloat
        , className =? "toolbar"                              --> doFloat
        , className =? "notification"                         --> doFloat
        , className =? "error"                                --> doFloat
        , className =? "stalonetray"                          --> doIgnore
        , className =? "trayer"                               --> doIgnore
        , className =? "calculator"                           --> doFloat
        , className =? "hl_linux"                             --> doFloat
        , className =? "crx_nkbihfbeogaeaoehlefnkodbefgpgknn" --> doFloat
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
myStartupHook = setWMName "xmonad"

-- Variables
myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "brave"

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
  , ("M1-\\", spawn my1Password)
  , ("M-x a", spawn mySpotify)
  , ("M-x b", spawn myBrowser)
  , ("M-x t", spawn myTelegram)
  , ("M-x d", spawn myDiscord)
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
  $ ["main", "side", "dev1", "dev2", "chat", "files", "write", "edit", "watch"]
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
  xmonad $ docks $ ewmhFullscreen . ewmh $ def {
    modMask = mod4Mask,
    workspaces = myWorkspaces,
    manageHook = myManageHook <+> manageHook def,
    layoutHook = avoidStruts $ mySpacing 10 $ layoutHook def,
    logHook = dynamicLogWithPP xmobarPP
      { ppOutput  = hPutStrLn xmproc
      , ppCurrent = xmobarColor "#ddbd94" "" . wrap "[" "]" -- Current workspace in xmobar
      , ppTitle   = xmobarColor "#8bc34a" "" . shorten 50   -- Title of active window in xmobar
      , ppSep     = "<fc=#666666> | </fc>"                  -- Separators in xmobar
        },
    borderWidth = 1,
    terminal = myTerminal,
    startupHook = myStartupHook,
    normalBorderColor = "#9ece6a",
    focusedBorderColor = "#e0af68"
  } `additionalKeysP` myKeys

-- import XMonad
-- import XMonad.Config.Desktop
--
-- import XMonad.Util.EZConfig
-- import XMonad.Util.Ungrab
--
-- import XMonad.Hooks.EwmhDesktops
-- import XMonad.Hooks.DynamicLog
--
-- myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
--
-- myConfig = def
--   { terminal   = "alacritty"
--   , modMask    = mod4Mask
--   , borderWidth = 2
--   , workspaces = myWorkspaces
--   }
--   `additionalKeysP`
--   [ ("M-S-4"    , unGrab *> spawn "scrot -s")
--   , ("M-<Space>", spawn "rofi -show run -dpi 164")
--   , ("M-]"      , spawn "google-chrome-beta" )
--   , ("M-S-]"    , spawn "firefox")
--   , ("M-M1-\\"   , spawn "1password")
--   ]
--
-- main :: IO ()
-- main = xmonad . ewmh =<< myConfig

import System.IO

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier (ModifiedLayout)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Cursor

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
        , className =? "crx_nkbihfbeogaeaoehlefnkodbefgpgknn"	--> doFloat
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
myStartupHook = setWMName "LG3D"

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

-- Key Bindings
-- M1 - Left Alt
-- M  - Mod Key (Cmd)
myKeys :: [(String, X ())]
myKeys =
	[ ("M1-<Space>" , spawn myLauncher)
	, ("M1-S-3"	, unGrab *> spawn "scrot ~/Pictures/screenshots/")
	, ("M1-S-4"	, unGrab *> spawn "scrot -s ~/Pictures/screenshots/")
	, ("M1-\\"	, spawn my1Password)
	, ("M-x a"	, spawn mySpotify)
	, ("M-x b"	, spawn myBrowser)
	, ("M-x t"	, spawn myTelegram)
	, ("M-x d"	, spawn myDiscord)
	]

-- workspaces
myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6"]

-- clickableWorkspaces :: [String] -> [String]
-- clickableWorkspaces = zipWith switchWorkspace [0..]
--   where
--     switchWorkspace i = xmobarAction ("wmctrl -s " ++ show i) "1"

-- Layouts
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True


main :: IO ()
main = do
	xmproc <- spawnPipe "xmobar -d"
        tray <- spawnPipe "killall -q stalonetray; sleep 1; stalonetray"
	spawn "pkill dunst ; dunst"
	xmonad $ docks def {
    modMask = mod4Mask,
    workspaces = myWorkspaces,
    manageHook = myManageHook <+> manageHook def,
    layoutHook = avoidStruts $ mySpacing 10 $ layoutHook def,
    logHook = dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmproc,
        ppTitle = xmobarColor "#8bc34a" "" . shorten 50
		},
    handleEventHook = fullscreenEventHook,
    borderWidth = 1,
    terminal = myTerminal,
    startupHook = myStartupHook,
    normalBorderColor = "#9ece6a",
    focusedBorderColor = "#e0af68"
  } `additionalKeysP` myKeys

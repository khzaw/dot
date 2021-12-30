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

myManageHook = composeAll
	[ className =? "1Password"				--> doCenterFloat
        , className =? "Nitrogen"				--> doCenterFloat
        , className =? "feh"					--> doCenterFloat
        , className =? "Plugins"				--> doCenterFloat
        , className =? "file_progress"				--> doFloat
        , className =? "pinentry-gtk-2"				--> doFloat
        , className =? "toolbar"				--> doFloat
        , className =? "notification"				--> doFloat
        , className =? "error"					--> doFloat
	, className =? "stalonetray"				--> doIgnore
	, className =? "trayer"					--> doIgnore
        , className =? "calculator"	  			--> doFloat
        , className =? "hl_linux"	  			--> doFloat
	, className =? "crx_nkbihfbeogaeaoehlefnkodbefgpgknn"	--> doFloat
        , title     =? "Volume Control"				--> doCenterFloat
        , title     =? "Bluetooth Devices"			--> doCenterFloat
        , title     =? "Save As"				--> doCenterFloat
        , title     =? "Save Image"				--> doCenterFloat
        , title     =? "Enter name of file to save to"  	--> doCenterFloat
        , title     =? "Picture in picture"  			--> doFloat
        , title     =? "Media viewer"	 			--> doFloat
        ]

-- StartupHook
myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

-- Variables
myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

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
	, ("M-x s"	, spawn mySpotify)
	, ("M-x b"	, spawn myBrowser)
	, ("M-x t"	, spawn myTelegram)
	, ("M-x d"	, spawn myDiscord)
	]

-- Layouts
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True


main = do 
	xmproc <- spawnPipe "xmobar -d"
        tray <- spawnPipe "killall -q stalonetray; sleep 1; stalonetray"
	xmonad $ docks defaultConfig 
		{ manageHook = myManageHook <+> manageHook defaultConfig
		, layoutHook = avoidStruts $ mySpacing 10 $ layoutHook defaultConfig
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "#8bc34a" "" . shorten 50
			}
		, modMask = mod4Mask
		, handleEventHook = fullscreenEventHook
		, borderWidth = 1
		, terminal = myTerminal
                , startupHook = myStartupHook
		} `additionalKeysP` myKeys
	


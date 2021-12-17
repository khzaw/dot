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

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import System.IO

myManageHook = composeAll
	[ className =? "1password"	--> doFloat
	]

main = do 
	xmproc <- spawnPipe "xmobar"
	xmonad $ docks defaultConfig 
		{ manageHook = myManageHook <+> manageHook defaultConfig
		, layoutHook = avoidStruts $ layoutHook defaultConfig
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "green" "" . shorten 50
			}
		, modMask = mod4Mask
		, borderWidth = 1
		, terminal = "alacritty"
		} `additionalKeysP`
		[ ("M-<Space>" , spawn "rofi -show run -dpi 164") 
		-- , ("M-S-4"     , unGrab *> spawn "scrot -s")
		]
	


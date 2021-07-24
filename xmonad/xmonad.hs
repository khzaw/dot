import XMonad
import XMonad.Config.Desktop

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio 
    tiled    = Tall nmaster delta ratio
    nmaster  = 1     -- Default number of windows in the master pane
    ratio    = 1/2   -- Default proportion of screen occupied by master pane
    delta    = 3/100 -- Percent of screen to increment by when resizing panes

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myConfig = def
  { terminal   = "alacritty"
  , modMask    = mod4Mask
  , borderWidth = 2
  , layoutHook = spacingRaw True (Border 0 50 50 50) True (Border 50 50 50 50) True $ layoutHook def
  , workspaces = myWorkspaces
  }
  `additionalKeysP`
  [ ("M-S-4"    , unGrab *> spawn "scrot -s")
  , ("M-<Space>", spawn "rofi -show run -dpi 164")
  , ("M-]"      , spawn "brave-beta" )
  , ("M-S-]"    , spawn "firefox")
  , ("M-M1-\\"   , spawn "1password")
  ]

main :: IO ()
main = xmonad . ewmh =<< xmobar myConfig


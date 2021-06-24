import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Xfce
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W
import XMonad.Layout.NoBorders
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ThreeColumns
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86

myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#93c91d"

myManageHook :: [ManageHook]
myManageHook =
    [ resource  =? "Do"   --> doIgnore
    , className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "desktop_window"  --> doFloat
    , appName =? "Enpass-Desktop"  --> doFloat
    , appName =? "Enpass"  --> doFloat
    , appName =? "wrapper-2.0" --> doFloat
    , isFullscreen --> doFullFloat
    , manageDocks
    ]

curLayout :: X String
curLayout = gets windowset >>= return . description . W.layout . W.workspace . W.current

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

main = xmonad $ xfceConfig
  { terminal             = "xfce4-terminal"
  , modMask              = mod4Mask
  , focusFollowsMouse    = True
  , normalBorderColor    = myNormalBorderColor
  , focusedBorderColor   = myFocusedBorderColor
  , borderWidth          = 2
  , manageHook           = manageHook defaultConfig <+> composeAll myManageHook
  }
  `additionalKeysP` myKeysP
  `additionalKeys` myKeys

myKeys = []

myKeysP =
  [ ("M-S-z", spawn "xflock4")
  , ("M-C-h", sendMessage $ pullGroup L)
  , ("M-C-l", sendMessage $ pullGroup R)
  , ("M-C-k", sendMessage $ pullGroup U)
  , ("M-C-j", sendMessage $ pullGroup D)
  , ("M-C-m", withFocused (sendMessage . MergeAll))
  , ("M-C-u", withFocused (sendMessage . UnMerge))
    -- Cycle WS
  , ("M-<Right>",     nextWS)
  , ("M-<Left>",      prevWS)
  , ("M-S-<Right>",   shiftToNext >> nextWS)
  , ("M-S-<Left>",    shiftToPrev >> prevWS)
  , ("M-z",           toggleWS)
  , ("M-y",           spawn "ytfzf -D")
  , ("M-g",           moveTo Next HiddenNonEmptyWS)
  , ("M-n",           moveTo Next EmptyWS)
  , ("M-S-n",         tagToEmptyWorkspace)
  , ("M-c",           kill) -- Close the focused window
  , ("M-<Down>",      windows W.focusDown)
  , ("M-<Up>",        windows W.focusUp)
  , ("M-S-<Down>",    windows W.swapDown)
  , ("M-S-<Up>",      windows W.swapUp)
  , ("M-d",           spawn "dmenu_run -l 30 -sb '#494949' -p run")
  , ("M-p",           spawn "~/.bin/passmenu")
  , ("M-x",           spawn "~/.bin/dswitcher")
  ]
  ++
  [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
       | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
       , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
  ]

-- imports
import XMonad

import System.IO (hPutStrLn)

import Data.Monoid
import Data.List

import XMonad.Util.Cursor
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.FloatSnap
import XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens

import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.SimpleDecoration (shrinkText)

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- shortkeys

myTerm    = "urxvtc"
myTmux = "~/bin/tmuxsess"
myBrowser = "firefox"
myAltBrowser = "chromium"
myMail = "thunderbird"
myLock = "xautolock -locknow"
myScreenFull = "scrot -q100 /tmp/screenshot_%Y%m%d_%H%M%S.png"
myScreenshot = "sleep 0.2; scrot -q100 -s -b /tmp/screen%H%M%S.png"
-- because chromium doesn't support shift+insert as primary selection switch from primary to clipboard
myVolUp="ponymix increase 5"
myVolDown="ponymix decrease 5"
myVolMute="ponymix toggle"
myDmenu="~/bin/dm"
myDmenuPass="~/bin/dmpass"
myRecomp="xmonad --recompile; killall xmobar; xmonad --restart; notify-send 'xmonad recompiled'"
myRest="killall xmobar; xmonad --restart; notify-send 'xmonad restarted'"

-- mouse move relative and click with xdotool
myMouseMoveLeft="xdotool mousemove_relative -- -20 0"
myMouseMoveRight="xdotool mousemove_relative -- 20 0"
myMouseMoveUp="xdotool mousemove_relative -- 0 -20" 
myMouseMoveDown="xdotool mousemove_relative -- 0 20"
myMouseClickLeft="xdotool click 1"
myMouseClickMiddle="xdotool click 2"
myMouseClickRight="xdotool click 3"

-- get focus on mouse 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- border
myBorderWidth   = 1

-- my metas
myModMask   = mod4Mask
altMask     = mod1Mask

-- my workspaces - clickable http://github.com/windelicato/dotfiles

myWorkspaces =  ["1","2","3","4","5","6","7","8","9"]

-- border colors
myNormalBorderColor  = "#2a1f1d"
myFocusedBorderColor = "#9b6c4a"

--key bindings
myKeys = \c -> mkKeymap c $ 
  -- launch a terminal
  [ ("M-S-<Return>", spawn $ XMonad.terminal c)
  -- launch dmenu
  , ("M-p", spawn myDmenu)
  -- launch dmpass
  , ("M-S-p", spawn myDmenuPass)
  -- close focused window
  , ("M-c", kill)
   -- rotate through layouts
  , ("M-<Space>", sendMessage NextLayout)
  -- reset to default layout
  , ("M-S-<Space>", setLayout $ XMonad.layoutHook c)
  -- go to full layout
  , ("M-f", sendMessage $ ToggleLayout)
  -- refresh
  , ("M-n", refresh)
  -- focus next window
  , ("M-j", windows W.focusDown)
  -- focus prev window
  , ("M-k", windows W.focusUp)
  -- focus master window
  , ("M-m", windows W.focusMaster)
  -- swap master)
  , ("M-<Return>", windows W.swapMaster)
  -- swap current window with next
  , ("M-S-j", windows W.swapDown)
  -- swap current window with prev
  , ("M-S-k", windows W.swapUp)
  -- shrink master area
  , ("M-h", sendMessage Shrink)
  -- expand master area
  , ("M-l", sendMessage Expand)
  -- expand master area
  , ("M-S-h", sendMessage MirrorShrink)
  -- expand master area
  , ("M-S-l", sendMessage MirrorExpand)
  -- number of windows in master area +1
  , ("M-,", sendMessage (IncMasterN 1))
  -- number of windows in master area -1
  , ("M-.", sendMessage (IncMasterN (-1)))
  -- dual monitor setup
  , ("M-o", swapNextScreen)
  , ("M-S-o", shiftNextScreen)
  -- scratchPad term
  , ("M-S-/", namedScratchpadAction scratchpads "term")
  -- scratchPad joplin-desktop
  , ("M-/", namedScratchpadAction scratchpads "joplin")
  -- scratchPad pavucontrol
  , ("M-v", namedScratchpadAction scratchpads "pavucontrol")
  -- togglestruts
  , ("M-b", sendMessage  ToggleStruts )
  -- quit xmonad
  , ("M-S-q", io (exitWith ExitSuccess))
  -- restart xmonad
  , ("M1-r", spawn myRest)
  -- restart w/o recompile
  , ("M1-S-r", spawn myRecomp)
  -- show date
--  , ("M-d", spawn myShowDate)
  -- lock
  , ("M1-C-l", spawn myLock)
  -- start browser
  , ("M-<F1>", spawn myBrowser)
  -- start second browser
  , ("M-S-<F1>", spawn myAltBrowser)
  -- start mail app
  , ("M-<F2>", spawn myMail)
  -- start tmux
  , ("M-<F3>", spawn myTmux)
  -- VolDown
  , ("<XF86AudioLowerVolume>", spawn myVolDown)
  -- VolUp
  , ("<XF86AudioRaiseVolume>", spawn myVolUp)
  -- Mute 
  , ("<XF86AudioMute>", spawn myVolMute)
  -- Screenshot full display
  , ("<Print>", spawn myScreenFull)
  -- Screenshot with selection
  , ("S-<Print>", spawn myScreenshot)
  -- mouse move relative 
  , ("M-M1-k", spawn myMouseMoveUp)
  , ("M-M1-j", spawn myMouseMoveDown)
  , ("M-M1-h", spawn myMouseMoveLeft)
  , ("M-M1-l", spawn myMouseMoveRight)
  , ("M-M1-u", spawn myMouseClickLeft)
  , ("M-M1-i", spawn myMouseClickMiddle)
  , ("M-M1-o", spawn myMouseClickRight)

  -- push window into tiling if not floating - float if tiling
  , ("M-t",
        withFocused (\windowId -> do { floats <- gets (W.floating . windowset);
        if windowId `M.member`floats
        then withFocused $ windows. W.sink
        else float windowId }))
  ] ++
  -- mod-[1..9], go to workspace n
  -- mod-shift-[1..9], send window to workspace n
  [(m ++ k, windows $ f w)
    | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
    , (m, f) <- [("M-",W.view), ("M-S-",W.shift)]] -- was W.greedyView
  ++
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [(m ++ "M-" ++ [k], screenWorkspace sc >>= flip whenJust (windows . f))
     | (k, sc) <- zip "wer" [0..]
     , (f, m) <- [(W.view, ""), (W.shift, "S-")]]

-- Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
-- mod-button1, Set the window to floating mode and move by dragging
  [ ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.position w
                   >> windows W.shiftMaster))
-- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
-- mod-button3, Set the window to floating mode and resize by dragging
  , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w
                  >> windows W.shiftMaster))
  ]

-- TabConfig

myTabConfig = def { activeColor = "#2a1f1d"
                  , inactiveColor = "#2a1f1d"
                  , urgentColor = "#9b6c4a"
                  , activeBorderColor = "#2a1f1d"
                  , inactiveBorderColor = "#2a1f1d"
                  , urgentBorderColor = "#9b6c4a"
                  , activeTextColor = "#9b6c4a"
                  , inactiveTextColor = "#e0dbb7"
                  , urgentTextColor = "#2a1f1d"
                  , fontName = "xft:Liberation Mono:size=8"
                  }
--layouts
myLayout = avoidStruts $ toggleLayouts full $ rt ||| mt ||| tab ||| tp ||| full
  where
  -- tiling profiles
  def = ResizableTall nmaster delta ratio []
  rt =  renamed [Replace "r" ] $ def
  mt =  renamed [Replace "m" ] $ Mirror rt
  tab = renamed [Replace "t" ] $ tabbed shrinkText myTabConfig
  tp = renamed [Replace "2" ] $ TwoPane delta ratio
  full =  renamed [Replace "f"] $ noBorders $ fullscreenFull Full
  -- default #windows in master
  nmaster = 1
  -- proportion size of master
  ratio   = 7/10
  -- incrementation on resizing
  delta   = 2/100

--managehook
myManageHook = composeAll . concat $
  [ [isDialog --> doFloat]
  , [className =? c --> doFloat | c <- myCFloats]
  , [title =? t --> doFloat | t <- myTFloats]
  , [resource =? r --> doFloat | r <- myRFloats]
  , [(className =? i <||> title =? i <||> resource =? i) --> doIgnore | i <- myIgnores]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 0) | x <- my1Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 1) | x <- my2Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 2) | x <- my3Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 3) | x <- my4Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 4) | x <- my5Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 5) | x <- my6Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 6) | x <- my7Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 7) | x <- my8Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 8) | x <- my9Shifts]
  ]
  where
--classes / titles / resources
  myCFloats = []
  myTFloats = []
  myRFloats = []
  myIgnores = []
  my1Shifts = ["Firefox","Chromium"]
  my2Shifts = []
  my3Shifts = ["Atom"]
  my4Shifts = ["Gimp","MPlayer","Thunderbird"]
  my5Shifts = ["remmina","xfreerdp"]
  my6Shifts = ["VirtualBox Manager","VirtualBox"]
  my7Shifts = []
  my8Shifts = []
  my9Shifts = []

-- event handling

myEventHook = mconcat [ docksEventHook ]

-- startup

myStartupHook = do 
  setWMName "LG3D" 
  setDefaultCursor xC_left_ptr
  docksStartupHook

-- xmobar

myXmobar = "xmobar /home/eye/.xmobarrc"

-- statusbar / logging
myLogHook h = dynamicLogWithPP $ def {
        ppCurrent = xmobarColor "#9b6c4a" ""
        , ppHidden = xmobarColor "#e0dbb7" "" 
        , ppUrgent = xmobarColor "#9b6c4a" "#573d26" 
        , ppSep = xmobarColor "#9b6c4a" "" " · "
        , ppWsSep = xmobarColor "#999999" "" " "
        , ppTitle = xmobarColor "#e0dbb7" "" . shorten 50
        -- do not show NSP at end of workspace list
        , ppSort = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort defaultPP
        , ppOutput = hPutStrLn h
}

-- scratchPads
scratchpads :: [NamedScratchpad]
scratchpads = [
-- run htop in xterm, find it by title, use default floating window placement
    NS "joplin" "joplin-desktop" (className =? "Joplin") 
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,

    NS "term" "urxvtc -name scratchpad" (resource =? "scratchpad")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),

    NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol")
        (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4))
  ] 

-- main function
main = do 
--  n <- countScreens
--  xmprocs <- mapM (\i -> spawnPipe $ "xmobar /home/eye/.xmobarrc-" ++ show i ++ " -x " ++ show i) [0..n-1]
  myBar <- spawnPipe myXmobar 
-- my config
  xmonad $ def {
  --simple stuff
    terminal       = myTerm
    ,focusFollowsMouse  = myFocusFollowsMouse
    ,borderWidth    = myBorderWidth
    ,modMask      = myModMask
    ,workspaces     = myWorkspaces
    ,normalBorderColor  = myNormalBorderColor
    ,focusedBorderColor = myFocusedBorderColor
    --key bindings
    ,keys         = myKeys
    ,mouseBindings    = myMouseBindings
    --hooks, layouts
    ,layoutHook     = myLayout
    ,manageHook     = (myManageHook <+> manageDocks ) <+> namedScratchpadManageHook scratchpads
    ,handleEventHook  = myEventHook
    ,logHook      = myLogHook myBar
    ,startupHook    = myStartupHook
}

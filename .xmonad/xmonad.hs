-- gnomeye's xmonad.hs

-- imports
import XMonad

import System.IO (hPutStrLn)

import Data.Monoid
import Data.List

import XMonad.Util.Cursor
import XMonad.Util.Run (spawnPipe)

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Actions.FloatSnap
import XMonad.Actions.FlexibleManipulate as Flex

import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders 
import XMonad.Layout.ToggleLayouts

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map    as M

-- shortkeys

myTerm    = "urxvtc"
myTmux = "~/bin/term"
myBrowser = "firefox"
myAltBrowser = "chromium"
myMail = "mimeo http://mail.google.com"
myWpChgr = "~/bin/wpchgr.pl"
mySkype = "/usr/bin/skype"
myPentaFlashOut = "~/bin/pentadactyt fpo"
myPentaFlashIn = "~/bin/pentadactyt fpi"
myLock = "xautolock -locknow"
myScreenFull = "scrot -q100 /tmp/screenshot_%Y%m%d_%H%M%S.png"
myScrShot = "sleep 0.2; scrot -q100 -s -b /tmp/screen%H%M%S.png"
myMPDPlay="mpc toggle"
myMPDNext="mpc next"
myMPDPrev="mpc prev"
myVolUp="ponymix increase 5"
myVolDown="ponymix decrease 5"
myDmenu="~/bin/dm"
myRecomp="killall conky dzen2 && xmonad --recompile; xmonad --restart; notify-send 'xmonad recompiled'"
myRest="killall conky dzen2 && /usr/bin/xmonad --restart; notify-send 'xmonad restarted'"
myStream="mplayer -nocache http://roach:8000" 
myXDisplay="~/bin/xdisplay.sh"
myXBackLightUp="/usr/bin/xbacklight -inc 10 -time 0 -steps 1"
myXBackLightDwn="/usr/bin/xbacklight -dec 10 -time 0 -steps 1"
--myDate="date '+%a, %b %d | %H:%M' | dzen2 -p 2 -fn 'Zekton:size=50' -fg '#A3583B' -ta c -w 160 -h 40 -x 880 -y 520"
myDate="date '+%a, %b %d | %H:%M' | dzen2 -p 1 -fn 'Zekton:size=50' -fg '#A3583B' -ta c -w 1600 -h 900"

-- get focus on mouse 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- border
myBorderWidth   = 3

-- my metas
myModMask   = mod4Mask
altMask     = mod1Mask

-- my workspaces - clickable http://github.com/windelicato/dotfiles
myWorkspaces  = click $ ["web","code","im","media","vm"]
              where click w = [ "^ca(1,xdotool key super+"++show(n)++ ")"++getIcon ws++ws++"^ca()" |
                      (i,ws) <- zip [1..] w,
                      let n = i ]

-- get icon function

getIcon i = "^i("++icondir++i++".xbm"++") "
  where icondir = "/home/eye/.dzen/xbm/"

-- border colors
myNormalBorderColor  = "#1f1f1b"
myFocusedBorderColor = "#A3583B"

--key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

  -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  -- launch dmenu
  , ((modm, xK_p), spawn myDmenu)
  -- close focused window
  , ((modm, xK_c), kill)
   -- rotate through layouts
  , ((modm, xK_space), sendMessage NextLayout)
  -- reset to default layout
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  -- go to full layout
  , ((modm, xK_f), sendMessage $ Toggle "Full")
  -- refresh
  , ((modm, xK_n), refresh)
  -- focus next window
  , ((modm, xK_j), windows W.focusDown)
  -- focus prev window
  , ((modm, xK_k), windows W.focusUp)
  -- focus master window
  , ((modm, xK_m), windows W.focusMaster  )
  -- swap master)
  , ((modm, xK_Return), windows W.swapMaster)
  -- swap current window with next
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  -- swap current window with prev
  , ((modm .|. shiftMask, xK_k), windows W.swapUp  )
  -- shrink master area
  , ((modm, xK_h), sendMessage Shrink)
  -- expand master area
  , ((modm, xK_l), sendMessage Expand)
  -- expand master area
  , ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
  -- expand master area
  , ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
  -- number of windows in master area +1
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
  -- number of windows in master area -1
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
  -- togglestruts
  , ((modm, xK_b), sendMessage ToggleStruts)
  -- quit xmonad
  , ((modm .|. shiftMask, xK_q   ), io (exitWith ExitSuccess))
  -- restart xmonad
  , ((modm .|. shiftMask, xK_r  ), spawn myRecomp)
  -- restart w/o recompile
  , ((modm, xK_r ), spawn myRest)
  -- lock
  , ((altMask .|. controlMask, xK_l ), spawn myLock)
  -- date
  , ((modm, xK_d), spawn myDate)
  -- start browser
  , ((modm, xK_F1), spawn myBrowser)
  -- start alternative browser
  , ((modm .|. shiftMask, xK_F1), spawn myAltBrowser)
  -- start mail app
  , ((modm, xK_F2), spawn myMail)
  -- start tmux
  , ((modm, xK_F3), spawn myTmux)
  -- xdotool to move mouse away from flash
  , ((modm, xK_F4), spawn myPentaFlashOut)
  -- xdotool to move mouse inside flash
  , ((modm .|. shiftMask, xK_F4), spawn myPentaFlashIn)
  -- change wallpaper over net
  , ((modm, xK_F5), spawn myWpChgr)
  -- start skype 
  , ((modm, xK_F7), spawn mySkype)
  -- start mplayer stream
  , ((modm, xK_F8), spawn myStream)
  -- MPD controls -- XF86 Keys
  -- MPDPrev
  , ((0, 0x1008ff16), spawn myMPDPrev)
  -- MPDPlay
  , ((0, 0x1008ff14), spawn myMPDPlay)
  -- MPDNext
  , ((0, 0x1008ff17), spawn myMPDNext)
  -- VolDown
  , ((0, 0x1008ff11), spawn myVolDown)
  -- VolUp
  , ((0, 0x1008ff13), spawn myVolUp)
  -- Change xrandr setup
  , ((0, 0x1008ff59), spawn myXDisplay)
  -- Screenshot full display
  , ((0, xK_Print), spawn myScreenFull)
  -- Screenshot with selection
  , ((shiftMask, xK_Print), spawn myScrShot)
  -- push window into tiling if not floating - float if tiling
  , ((modm, xK_t),
	withFocused (\windowId -> do { floats <- gets (W.floating . windowset);
	if windowId `M.member`floats
	then withFocused $ windows. W.sink
	else float windowId }))
  -- moving / shrinking Floating Windows (thanks to FloatSnap Module)
  , ((modm,         xK_Left),  withFocused $ snapMove L Nothing)
  , ((modm,         xK_Right), withFocused $ snapMove R Nothing)
  , ((modm,         xK_Up),  withFocused $ snapMove U Nothing)
  , ((modm,         xK_Down),  withFocused $ snapMove D Nothing)
  , ((modm .|. shiftMask, xK_Left),  withFocused $ snapShrink R Nothing)
  , ((modm .|. shiftMask, xK_Right), withFocused $ snapGrow R Nothing)
  , ((modm .|. shiftMask, xK_Up),  withFocused $ snapShrink D Nothing)
  , ((modm .|. shiftMask, xK_Down),  withFocused $ snapGrow D Nothing)
  ] ++
  -- mod-[1..9], go to workspace n
  -- mod-shift-[1..9], send window to workspace n
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

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

--layouts
myLayout = avoidStruts $ smartBorders $ toggleLayouts Full $ tile ||| mtile ||| full 
  where
  -- tiling profiles
  rt = spacing 3 $ ResizableTall nmaster delta ratio []
  tile   = renamed [Replace "[]="] $ smartBorders rt
  mtile   = renamed [Replace "M[]="] $ smartBorders $ Mirror rt
  full   = renamed [Replace "[]"] $ noBorders Full
  -- default #windows in master
  nmaster = 1
  -- proportion size of master
  ratio   = 6/10
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
  ]
  where
--classes / titles / resources
  myCFloats = ["MPlayer","Gimp","VirtualBox","hl2_linux","Steam"]
  myTFloats = ["Downloads", "Save As"]
  myRFloats = []
  myIgnores = ["desktop_window", "kdesktop","stalonetray","Xfce4-notifyd"]
  my1Shifts = []
  my2Shifts = []
  my3Shifts = ["Skype","ts3client_linux_amd64"]
  my4Shifts = ["Gimp","MPlayer","hl2_linux","Steam"]
  my5Shifts = ["VirtualBox"]

-- event handling

myEventHook = mempty

-- startup

myStartupHook = do 
  setWMName "LG3D" 
  setDefaultCursor xC_left_ptr

-- dzen2

myDzenLeftBar = "/usr/bin/dzen2 -ta l -w 500" ++ myDzenPost
myDzenRightBar = myConky ++ " | /usr/bin/dzen2 -ta r -x 500" ++ myDzenPost
myConky="conky -qc /home/eye/.dzen/conkyrc-dzen"
--myDzenRightBar = "/home/eye/.dzen/dzenbar.sh | /usr/bin/dzen2 -ta r -x 500" ++ myDzenPost
myDzenPost=" -bg '#1f1f1b' -fn 'Zekton:size=7' -h 16 -e 'onstart=lower'"

-- statusbar / logging
myLogHook h = dynamicLogWithPP $ defaultPP {
	ppCurrent = dzenColor "#A3583B" "" 
	, ppHidden = dzenColor "#C2BFB8" "" 
 	, ppVisible = dzenColor "#C2BFB8" "" 
 	, ppUrgent = dzenColor "#1f1f1b" "#A3583B" 
  , ppLayout = dzenColor "#707070" "" 
 	, ppSep = " "
 	, ppWsSep = dzenColor "#505050" "" "  "
 	, ppTitle = dzenColor "#A3583B" "" . shorten 50
  , ppOutput = hPutStrLn h
}

-- main function
main = do 
  barLeft <- spawnPipe myDzenLeftBar
  barRight <- spawnPipe myDzenRightBar
--my config
  xmonad $  defaultConfig {
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
    ,manageHook     = myManageHook <+> manageDocks
    ,handleEventHook  = myEventHook
    ,logHook      = myLogHook barLeft
    ,startupHook    = myStartupHook
}

-- gnomeye's xmonad.hs

-- imports
import XMonad

import System.IO (hPutStrLn)

import Data.Monoid
import Data.List

import XMonad.Util.Cursor
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers 
import XMonad.Actions.FloatSnap
import XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.CycleWS

import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders 
import XMonad.Layout.Fullscreen
import XMonad.Layout.ToggleLayouts

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map    as M

-- shortkeys

myTerm    = "urxvtc"
myTmux = "~/bin/tmuxsess"
myBrowser = "chromium"
mySecBrowser = "firefox"
myMail = "mimeo http://inbox.google.com"
myWpChgr = "perl -e 'use LWP::Simple;use JSON;print get(from_json(get(\"https://api.desktoppr.co/1/wallpapers/random\"),{utf8 => 1})->{\"response\"}->{\"image\"}->{\"url\"})' | feh --bg-scale --no-fehbg -"
myWpPattern = "~/bin/randompatternsvg.rb > /tmp/.w.svg && feh --magick-timeout 1 --bg-tile --no-fehbg /tmp/.w.svg"
mySkype = "/usr/bin/skype"
myXdotoolFlashOut = "xdotool mousemove 10 90;xdotool keyup Super_L;xdotool click 1"
myXdotoolFlashIn = "xdotool mousemove 940 370;xdotool keyup Super_L;xdotool click 1"
myLock = "xautolock -locknow"
myScreenFull = "scrot -q100 /tmp/screenshot_%Y%m%d_%H%M%S.png"
myScrShot = "sleep 0.2; scrot -q100 -s -b /tmp/screen%H%M%S.png"
-- because chromium doesn't support shift+insert as primary selection switch from primary to clipboard
myXclipSwitch="xclip -o -se p | xclip -i -se c" 
myVolUp="ponymix increase 5"
myVolDown="ponymix decrease 5"
myVolMute="ponymix toggle"
myDmenu="~/bin/dm"
myRecomp="killall conky dzen2 && xmonad --recompile; xmonad --restart; notify-send 'xmonad recompiled'"
myRest="killall conky dzen2 trayer && ~/bin/mytrayer.sh && xmonad --restart; notify-send 'xmonad restarted'"
myStream="mplayer -nocache http://roach:8000" 
myXBackLightUp="/usr/bin/xbacklight -inc 10 -time 0 -steps 1"
myXBackLightDwn="/usr/bin/xbacklight -dec 10 -time 0 -steps 1"
-- myShowDate="echo \"^fg(#81bac8)$(date '+%a, %b %d | %H:%M')\" | dzen2 -ta r -x 500 -bg '#303030' -fn 'Liberation Mono:size=8' -h 16 -p 2"

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
myBorderWidth   = 4

-- my metas
myModMask   = mod4Mask
altMask     = mod1Mask

-- my workspaces - clickable http://github.com/windelicato/dotfiles
myWorkspaces  = click $ ["web","shell","work","media","dev" ]
              where click w = [ "^ca(1,xdotool key super+"++show(n)++")"++getIcon ws++"^ca()" |
                      (i,ws) <- zip [1..] w,
                      let n = i ]

-- get icon function

getIcon i = "^i("++icondir++i++".xbm"++")"
  where icondir = "/home/eye/.dzen/xbm/"

-- border colors
myNormalBorderColor  = "#303030"
myFocusedBorderColor = "#81bac8"

--key bindings
myKeys = \c -> mkKeymap c $ 
  -- launch a terminal
  [ ("M-S-<Return>", spawn $ XMonad.terminal c)
  -- launch dmenu
  , ("M-p", spawn myDmenu)
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
  -- togglestruts
  , ("M-b", sendMessage ToggleStruts)
  -- quit xmonad
  , ("M-S-q", io (exitWith ExitSuccess))
  -- restart xmonad
  , ("M-S-r", spawn myRecomp)
  -- restart w/o recompile
  , ("M-r", spawn myRest)
  -- lock
  , ("M1-C-l", spawn myLock)
  -- xclip switch
  , ("M-y", spawn myXclipSwitch)
  -- start browser
  , ("M-<F1>", spawn myBrowser)
  -- start second browser
  , ("M-S-<F1>", spawn mySecBrowser)
  -- start mail app
  , ("M-<F2>", spawn myMail)
  -- start tmux
  , ("M-<F3>", spawn myTmux)
  -- xdotool to move mouse away from flash
  , ("M-<F4>", spawn myXdotoolFlashOut)
  -- xdotool to move mouse inside flash
  , ("M-S-<F4>", spawn myXdotoolFlashIn)
  -- change wallpaper over net
  , ("M-<F5>", spawn myWpChgr)
  -- change to random pattern wallpaper
  , ("M-<F6>", spawn myWpPattern)
  -- start skype 
  , ("M-<F7>", spawn mySkype)
  -- start mplayer stream
  , ("M-<F8>", spawn myStream)
  -- VolDown
  , ("<XF86AudioLowerVolume>", spawn myVolDown)
  -- VolUp
  , ("<XF86AudioRaiseVolume>", spawn myVolUp)
  -- Mute 
  , ("<XF86AudioMute>", spawn myVolMute)
  -- Screenshot full display
  , ("<Print>", spawn myScreenFull)
  -- Screenshot with selection
  , ("S-<Print>", spawn myScrShot)
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
  -- moving / shrinking Floating Windows (thanks to FloatSnap Module)
  , ("M-<L>",  withFocused $ snapMove L Nothing)
  , ("M-<R>", withFocused $ snapMove R Nothing)
  , ("M-<U>",  withFocused $ snapMove U Nothing)
  , ("M-<D>",  withFocused $ snapMove D Nothing)
  , ("M-S-<L>",  withFocused $ snapShrink R Nothing)
  , ("M-S-<R>", withFocused $ snapGrow R Nothing)
  , ("M-S-<U>",  withFocused $ snapShrink D Nothing)
  , ("M-S-<D>",  withFocused $ snapGrow D Nothing)
  ] ++
  -- mod-[1..9], go to workspace n
  -- mod-shift-[1..9], send window to workspace n
  [(m ++ k, windows $ f w)
    | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
    , (m, f) <- [("M-",W.view), ("M-S-",W.shift)]] -- was W.greedyView

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
myLayout = avoidStruts $ smartBorders $ toggleLayouts full $ tile ||| mtile ||| btile ||| full 
  where
  -- tiling profiles
  lay = ResizableTall nmaster delta ratio []
  rt = spacing 2 $ lay
  tile = renamed [Replace "tile"] $ smartBorders rt
  mtile = renamed [Replace "mtile"] $ smartBorders $ Mirror rt
  btile = renamed [Replace "btile" ] $ noBorders $ lay
  full =  renamed [Replace "full"] $ noBorders $ fullscreenFull Full
  -- default #windows in master
  nmaster = 1
  -- proportion size of master
  ratio   = 5/10
  -- incrementation on resizing
  delta   = 2/100

--managehook
myManageHook = composeAll . concat $
  [ [isDialog --> doFloat]
  , [className =? c --> doFloat | c <- myCFloats]
  , [title =? t --> doFloat | t <- myTFloats]
  , [resource =? r --> doFloat | r <- myRFloats]
  , [(className =? i <||> title =? i <||> resource =? i) --> doIgnore | i <- myIgnores]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 1) | x <- my2Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 0) | x <- my1Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 2) | x <- my3Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 3) | x <- my4Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 4) | x <- my5Shifts]
  ]
  where
--classes / titles / resources
  myCFloats = []
  myTFloats = []
  myRFloats = []
  myIgnores = ["desktop_window", "kdesktop","stalonetray","Xfce4-notifyd"]
  my1Shifts = ["Firefox","Chromium"]
  my2Shifts = ["ts3client_linux_amd64","Pidgin","crx_nckgahadagoaajjgafhacjanaoiihapd"]
  my3Shifts = ["Wfica"]
  my4Shifts = ["Gimp","MPlayer"]
  my5Shifts = ["VirtualBox"]

-- event handling

myEventHook = mempty

-- startup

myStartupHook = do 
  setWMName "LG3D" 
  setDefaultCursor xC_left_ptr

-- dzen2

myDzenLeftBar = "dzen2 -ta l -w 500" ++ myDzenPost
myDzenRightBar = myConky ++ " | dzen2 -ta r -x 500" ++ myDzenPost
myConky="conky -qc /home/eye/.dzen/conkyrc-dzen"
myDzenPost=" -bg '#303030' -fn 'Liberation Mono:size=8' -h 16 -e 'onstart=lower'"

-- statusbar / logging
myLogHook h = dynamicLogWithPP $ defaultPP {
	ppCurrent = dzenColor "#81bac8" "" 
	, ppHidden = dzenColor "#e3e3e3" "" 
 	, ppUrgent = dzenColor "#303030" "#81bac8" 
  , ppLayout = wrap "^ca(1,xdotool key super+space)" "^ca()" . dzenColor "#707070" "" .
              (\x -> case x of
                  "tile" -> "^i(/home/eye/.dzen/xbm/tile.xbm)"
                  "mtile" -> "^i(/home/eye/.dzen/xbm/tile.xbm) M"
                  "btile" -> "^i(/home/eye/.dzen/xbm/tile.xbm) B"
                  "full" -> "^i(/home/eye/.dzen/xbm/full.xbm)"
                  )
 	, ppSep = " · "
 	, ppWsSep = dzenColor "#505050" "" " "
 	, ppTitle = wrap "^ca(2,xdotool key super+c)" "^ca()" . dzenColor "#81bac8" "" . shorten 50
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

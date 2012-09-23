-- gnomeye's xmonad.hs
--
-- imports
import XMonad
import XMonad.Hooks.DynamicLog
import Data.Monoid
import XMonad.Util.EZConfig
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders 
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- preferred applications
-- shortcuts

myTerm      = "urxvt"
myTmux = "~/bin/term"
myBrowser = "firefox"
myMail = "mimeo http://mail.google.com"
myWpChgr = "~/bin/wpchgr.pl"
myRandWp = "wpfl=$(find ~/img/wallpapers/wallbase -iname 'wallbase*jpg' -type f|sort -R|head -1);feh --bg-scale --no-fehbg $wpfl;echo $wpfl > /tmp/.randwp"
myPentaMouse = "~/bin/pentadactyt yt"
myLock = "xautolock -locknow"
myScreenFull = "scrot /tmp/screenshot_%H%M%S_%Y%m%d.png"
myScrShot = "sleep 0.2; scrot -s -b /tmp/screen%H%M%S.png"
myMPDPlay="mpc toggle"
myMPDNext="mpc next"
myMPDPrev="mpc prev"
myVolMute="~/bin/volctrl mute"
myVolUp="~/bin/volctrl up"
myVolDown="~/bin/volctrl down"
myVolChange="~/bin/volctrl change"
myDmenu="~/bin/dm"
myRecomp="xmonad --recompile; xmonad --restart; notify-send 'xmonad recompiled'"
myRest="/usr/bin/xmonad --restart; notify-send 'xmonad restarted'"

-- get focus on mouse 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- border
myBorderWidth   = 1

-- my metas
myModMask       = mod1Mask

-- my workspaces
myWorkspaces    = ["web","media","vm","code" ] 

-- border colors
myNormalBorderColor  = "#707070"
myFocusedBorderColor = "#1793d0"

--key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask,xK_Return),
	spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,xK_p),
	spawn myDmenu)

    -- close focused window
    , ((modm,xK_c),
	kill)

     -- rotate through layouts
    , ((modm,xK_space),
	sendMessage NextLayout)

    -- reset to default layout
    , ((modm .|. shiftMask,xK_space),
	setLayout $ XMonad.layoutHook conf)

    -- resize windows to predefined size
    , ((modm,xK_n),
	refresh)

    -- focus next window
    , ((modm,xK_Tab),
	windows W.focusDown)

    -- focus next window
    , ((modm,xK_j),
	windows W.focusDown)

    -- focus prev window
    , ((modm,xK_k),
	windows W.focusUp  )

    -- focus master window
    , ((modm,xK_m),
	windows W.focusMaster  )

    -- swap master
    , ((modm,xK_Return),
	windows W.swapMaster)

    -- swap current window with next
    , ((modm .|. shiftMask, xK_j),
	windows W.swapDown)

    -- swap current window with prev
    , ((modm .|. shiftMask,xK_k),
	windows W.swapUp    )

    -- shrink master area
    , ((modm,xK_h),
	sendMessage Shrink)

    -- expand master area
    , ((modm,xK_l),
	sendMessage Expand)

    -- push window into tiling
    , ((modm,xK_t),
	withFocused $ windows . W.sink)
	
    -- push window out of tiling (float)
--    , ((modm .|. shiftMask, xK_t),
----	withFocused $ windows . W.float)

    -- number of windows in master area +1
    , ((modm,xK_comma),
	sendMessage (IncMasterN 1))

    -- number of windows in master area -1
    , ((modm,xK_period),
	sendMessage (IncMasterN (-1)))

    -- quit xmonad
    , ((modm .|. shiftMask, xK_q     ),
	io (exitWith ExitSuccess))

    -- restart xmonad
    , ((modm .|. shiftMask, xK_r    ),
	spawn myRecomp)

    -- restart w/o recompile
    , ((modm, xK_r ), 
	spawn myRest)
    ]
    ++

    -- mod-[1..9], go to workspace n
    -- mod-shift-[1..9], send window to workspace n
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-- Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

--layouts
myLayout = tile ||| mtile ||| full
  where
     -- tiling profiles
     rt = ResizableTall nmaster delta ratio []
     tile   = renamed [Replace "[]="] $ smartBorders rt
     mtile   = renamed [Replace "M[]="] $ smartBorders $ Mirror rt
     full   = renamed [Replace "[]"] $ noBorders Full
     -- default #windows in master
     nmaster = 1
     -- proportion size of master
     ratio   = 6/10
     -- incrementation on resizing
     delta   = 2/100
     

--rules
myManageHook = composeAll
    [ className =? "MPlayer"        --> doShift "media"
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className  =? "VirtualBox"     --> doShift "vm"
    , className  =? "VirtualBox"     --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

-- event handling
myEventHook = mempty

-- statusbar / logging
myLogHook = return ()

-- startup
myStartupHook = return ()

-- launch xmobar
myBar = "/usr/bin/xmobar"
--custom PP
myPP = xmobarPP { 
	ppCurrent = xmobarColor "#1793d0" "" 
	, ppHidden = xmobarColor "#b0b0b0" ""
	, ppHiddenNoWindows = xmobarColor "#707070" ""
 	, ppVisible = xmobarColor "#b0b0b0" ""
 	, ppUrgent = xmobarColor "#1c1d1f" "#1793d0"
	, ppLayout = xmobarColor "#707070" "" 
 	, ppSep = "\t\t\t"
 	, ppWsSep = xmobarColor "#505050" "" " / "
 	, ppTitle = xmobarColor "#1793d0" "" . shorten 50
}
-- key bind
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- main function
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConf
-- my config
myConf = defaultConfig {
      -- simple stuff
        terminal           = myTerm,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
-- additional keys ...
	`additionalKeysP`
	[ ("M4-l", spawn myLock)
	, ("M4-<Esc>", spawn myVolChange)
	, ("M-<F1>", spawn myBrowser)
	, ("M-<F2>", spawn myMail)
	, ("M-<F3>", spawn myTmux)
	, ("M-<F4>", spawn myPentaMouse)
	, ("M-<F5>", spawn myWpChgr)
	, ("M-<F6>", spawn myRandWp)
	, ("S-<Print>", spawn myScrShot)
	, ("<Print>", spawn myScreenFull)
	, ("<XF86AudioPlay>", spawn myMPDPlay)
	, ("<XF86AudioNext>", spawn myMPDNext)
	, ("<XF86AudioPrev>", spawn myMPDPrev)
	, ("<XF86AudioMute>", spawn myVolMute)
	, ("<XF86AudioLowerVolume>", spawn myVolDown)
	, ("<XF86AudioRaiseVolume>", spawn myVolUp)]

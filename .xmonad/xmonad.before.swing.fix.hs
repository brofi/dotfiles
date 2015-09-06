import XMonad
-- LAYOUTS
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import System.IO
import XMonad.Layout.Minimize
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops as ED -- devilspie brauch ewmh
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor -- für cursorfix

import XMonad.Layout.Drawer --vlt für gimp/ nich hinbekommen nochmal probiern
import XMonad.Layout.Fullscreen as FS
import XMonad.Layout.Grid
import XMonad.Layout.IM
import Data.Ratio ((%))
import XMonad.Layout.LimitWindows
import XMonad.Layout.MagicFocus
import XMonad.Layout.Maximize
import XMonad.Layout.MessageControl -- unEscape(...)
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.OneBig
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts as TL
import XMonad.Layout.WindowArranger

myTerminal = "urxvt"

--myLayout = spacing 5 $ ThreeCol 1 (3/100) (1/2) ||| ThreeColMid 1 (3/100) (1/2) ||| minimize (tiled) ||| Mirror tiled ||| nobordersLayout
	--where
		--tiled = spacing 5 $ Tall nmaster delta ratio
		--nmaster = 1
		--delta = 3/100
		--ratio = 2/3 -- default: 1/2
		--nobordersLayout = smartBorders $ Full


myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9 /dev/null"]

myFull = noBorders Full
myBase = smartBorders $ spacing 5 $ minimize $ maximize $ myWide ||| myTall ||| myBig ||| myCol
    where
        myTall = multiCol [1] 4 0.01 0.5
        myWide = Mirror (multiCol [1] 2 0.01 (-0.25))
        myBig = OneBig (3/4) (3/4)
        myCol = ThreeColMid 1 (3/100) (1/2)

myMainLayout = myBase ||| myFull
myChatLayout = noBorders $ withIM ratio matchSteam $  withIM ratio matchSkype $ reflectHoriz $ withIM ratio matchPidgin (Grid ||| Full)
    where
        ratio = 1%5
        matchSteam = And (ClassName "Steam")(Title "Friends")
        matchSkype = And (ClassName "Skype")(Or (Title "schnugge. - Skype™")(Title "Skype™ 4.2 for Linux"))
        matchPidgin = And (ClassName "Pidgin")(Role "buddy_list")

myLayout = avoidStruts $ toggleLayouts (myFull) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ onWorkspaces ["1", "2"] myMainLayout $ onWorkspace "3" myChatLayout $ myMainLayout

myManageHook = composeAll
	[ className =? "Skype" --> doShift "3"
	, className =? "Pidgin" --> doShift "3"
	, className =? "Steam" --> doShift "3"
	--, className =? "Skype" --> doFloat
	--, className =? "Pidgin" --> doFloat
	--, className =? "Steam" --> doFloat
	, isFullscreen --> doFullFloat ] --aus jedem Layout heraus

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    -- ewmh nullifies the effect of setWMName
    -- solution: https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Using_SetWMName_with_EwmhDesktops
	-- xmonad $ defaultConfig
	xmonad $ ewmh defaultConfig
		{ terminal = myTerminal
		, layoutHook = myLayout
		, manageHook = fullscreenManageHook <+> manageDocks <+> myManageHook <+> manageHook defaultConfig
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "#ff9f00" "" . shorten 50
			, ppLayout = const "" } >> fadeInactiveLogHook 0.85
		, startupHook = setDefaultCursor xC_left_ptr -- normal cursor fix
		, workspaces = myWorkspaces
		, borderWidth = 1
		, focusedBorderColor = "#ffffff" --"#68e862"
		, normalBorderColor = "#ffffff" --"#729fcf"
		, modMask = mod1Mask 
		, handleEventHook = FS.fullscreenEventHook -- chrome fullscreen fix
		} `additionalKeys`
		[ ((0, xK_Print), spawn "scrot")
		, ((mod1Mask, xK_Print), spawn "scrot -s")
		, ((mod1Mask, xK_p), spawn "dmenu_run -b -nb '#383838' -nf '#ccff00' -sf '#383838' -sb '#ccff00' -p 'launch' -fn 'Inconsolata-11'")
		, ((0, xF86XK_MyComputer), spawn "export TERM='xterm-256color'; urxvt -e 'mc -c'")
		, ((0, xF86XK_HomePage), spawn "google-chrome")
		, ((0, xF86XK_Calculator), spawn "urxvt -e bc")
		, ((0, xF86XK_AudioMedia), spawn "urxvt -e ncmpcpp")
		, ((0, xF86XK_AudioPrev), spawn "ncmpcpp prev")
		, ((0, xF86XK_AudioNext), spawn "ncmpcpp next")
		, ((0, xF86XK_AudioPlay), spawn "ncmpcpp play")
		, ((0, xF86XK_AudioStop), spawn "ncmpcpp stop")
		, ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
		, ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2dB-")
		, ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2dB+")
		, ((mod1Mask, xF86XK_AudioLowerVolume), spawn "ncmpcpp volume -3")
		, ((mod1Mask, xF86XK_AudioRaiseVolume), spawn "ncmpcpp volume +3")
		, ((mod1Mask, xK_backslash), withFocused $ sendMessage . maximizeRestore)
		, ((mod1Mask, xK_m), withFocused minimizeWindow)
		, ((mod1Mask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
		, ((mod1Mask .|. controlMask, xK_x), sendMessage $ MT.Toggle REFLECTX)
		, ((mod1Mask .|. controlMask, xK_y), sendMessage $ MT.Toggle REFLECTY)
		, ((mod1Mask .|. controlMask, xK_space), sendMessage ToggleLayout)
        --- comment ArrangeAll bindings gehen ned...
		]

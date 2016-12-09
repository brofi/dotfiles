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

-- Default terminal
myTerminal :: String
myTerminal = "urxvt"

-- Workspace names
myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9 /dev/null"]

-- Set no borders on Full layout
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
        matchSkype = And (ClassName "skypeforlinux")(Title "Skype for Linux Alpha")
        matchPidgin = And (ClassName "Pidgin")(Or (Role "buddy_list")(Role "accounts"))

myLayout = avoidStruts $ toggleLayouts (myFull) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ onWorkspaces ["1", "2"] myMainLayout $ onWorkspace "5" myChatLayout $ myMainLayout

myManageHook = composeAll
    [ className =? "skypeforlinux" --> doShift "5"
    , className =? "Pidgin" --> doShift "5"
    , className =? "Steam" --> doShift "5"
    --, className =? "Skype" --> doFloat
    --, className =? "Pidgin" --> doFloat
    --, className =? "Steam" --> doFloat
    , isFullscreen --> doFullFloat ] --aus jedem Layout heraus


myKeys = [ ((0, xK_Print), spawn "scrot")
        , ((mod4Mask, xK_Print), spawn "scrot -s")
        , ((mod4Mask, xK_p), spawn "dmenu_run -b -nb '#2f343b' -nf '#d3dae3' -sf '#ccff00' -sb '#2f343b' -p '>' -fn 'Inconsolata-11'")
        , ((0, xF86XK_MyComputer), spawn "urxvt -e mc")
        , ((0, xF86XK_Explorer), spawn "urxvt -e mc")
        , ((0, xF86XK_HomePage), spawn "google-chrome-stable")
        , ((0, xF86XK_Calculator), spawn "urxvt -e bc")
        , ((0, xF86XK_Mail), spawn "google-chrome-stable gmail.com")
        , ((0, xF86XK_AudioMedia), spawn "urxvt -e ncmpcpp")
        , ((0, xF86XK_AudioPrev), spawn "ncmpcpp prev")
        , ((0, xF86XK_AudioNext), spawn "ncmpcpp next")
        , ((0, xF86XK_AudioPlay), spawn "ncmpcpp play")
        , ((0, xF86XK_AudioStop), spawn "ncmpcpp stop")
        , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
        , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2dB-")
        , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2dB+")
        , ((mod4Mask, xF86XK_AudioLowerVolume), spawn "ncmpcpp volume -3")
        , ((mod4Mask, xF86XK_AudioRaiseVolume), spawn "ncmpcpp volume +3")
        , ((mod4Mask, xK_backslash), withFocused $ sendMessage . maximizeRestore)
        , ((mod4Mask, xK_m), withFocused minimizeWindow)
        , ((mod4Mask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
        , ((mod4Mask .|. controlMask, xK_x), sendMessage $ MT.Toggle REFLECTX)
        , ((mod4Mask .|. controlMask, xK_y), sendMessage $ MT.Toggle REFLECTY)
        , ((mod4Mask .|. controlMask, xK_space), sendMessage ToggleLayout)
        --- comment ArrangeAll bindings gehen ned...
        ]
-- ewmh nullifies the effect of setWMName
-- solution: https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Using_SetWMName_with_EwmhDesktops

myLogHook xmproc = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "#ff9f00" "" . shorten 50
                    , ppLayout = const "" } >> fadeInactiveLogHook 0.85

myStartupHook = setDefaultCursor xC_left_ptr >> setWMName "LG3D" -- normal cursor fix & java swing fix

myConf = ewmh defaultConfig
                { terminal = myTerminal
                , layoutHook = myLayout
                , manageHook = fullscreenManageHook <+> manageDocks <+> myManageHook <+> manageHook defaultConfig
                -- , logHook = myLogHook xmproc
                , startupHook = myStartupHook
                , workspaces = myWorkspaces
                , borderWidth = 2
                , focusedBorderColor = "#2d3036"
                , normalBorderColor = "#2d3036"
                -- , modMask = mod1Mask 
                , modMask = mod4Mask -- Use Super instead of Alt
                , handleEventHook = FS.fullscreenEventHook -- chrome fullscreen fix
                } `additionalKeys` myKeys

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad myConf
            -- set startuphook with java swing fix again, because ewmh nullifies it
            { startupHook = startupHook myConf >> setWMName "LG3D"
            -- set logHook here, because no clue on how to do it with parameter for myConf
            , logHook = myLogHook xmproc
            } --`additionalKeys` myKeys

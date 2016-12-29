-- For type constraints like 'LayoutClass l Window'
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : XMonad configuration
--
-- Custom XMonad configuration.
--
-----------------------------------------------------------------------------

module Main where

import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Monoid (All) -- for type only
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86 -- to use xF86XK_* in key bindings
import System.Directory (getHomeDirectory,createDirectoryIfMissing)

import XMonad
import XMonad.Hooks.DynamicLog (statusBar
    ,PP,ppCurrent,ppVisible,ppHidden,ppHiddenNoWindows,ppUrgent
    ,ppSep,ppWsSep,ppTitle,ppLayout,ppOrder,ppSort,ppExtras
    ,xmobarColor,dzenColor,shorten,pad,wrap)
import XMonad.Hooks.EwmhDesktops (ewmh,fullscreenEventHook)
import XMonad.Hooks.ManageDocks (AvoidStruts) -- for type only
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.LayoutModifier (ModifiedLayout) -- for type only
import XMonad.Layout.Maximize (maximize,maximizeRestore)
import XMonad.Layout.Minimize (minimize,minimizeWindow
    ,MinimizeMsg(RestoreNextMinimizedWin))
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.MultiToggle (Toggle(Toggle),mkToggle,single)
import XMonad.Layout.NoBorders (Ambiguity(Screen),lessBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (REFLECTX(REFLECTX),REFLECTY(REFLECTY))
import XMonad.Layout.Renamed (renamed,Rename(CutWordsLeft,Replace))
import XMonad.Layout.Spacing (smartSpacing)
import qualified XMonad.StackSet as W -- window key bindings (e.g. additional workspace)
    (greedyView,shift,focusUp,focusDown)
import XMonad.Util.WorkspaceCompare (getSortByIndex) -- ppSort

-- Experimental
-- import XMonad.Layout.IM
-- import XMonad.Layout.Magnifier
import XMonad.Layout.Tabbed

-- TODO read colors from .Xresources
-- <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/terminfo-0.4.0.2/System-Console-Terminfo-Color.html terminfo colors>
-- | Default colors.
bg, fg, blue, green, red :: String
bg     = "#2f343b"
fg     = "#d3dae3"
blue   = "#48c6ff"
green  = "#ccff00"
red    = "#ff00a0"
orange = "#ff9f00"

-- | List of workspace names.
workspaces' :: [WorkspaceId]
workspaces' = map show [1..9] ++ ["/dev/null"]

-- | Default terminal.
terminal' :: String
terminal' = "urxvt"

-- | Xmonad border width.
borderWidth' :: Dimension
borderWidth' = 2

-- | Use @Super_L@ as Mod key.
modMask' :: KeyMask
modMask' = mod4Mask

-- | Xmonad border colors.
normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = bg
focusedBorderColor' = blue

-- | Space around windows.
spacing' :: Int
spacing' = 5

-- Minimized windows aren't skipped when changing focus.
-- "XMonad.Layout.BoringWindows" could help with that, but there's a
-- <https://mail.haskell.org/pipermail/xmonad/2009-December/009403.html bug>
-- with boringAuto.

{- | Custom 'layoutHook'.
See "XMonad.Config" for default layout.


Added:

* 'maximize' to be able to \"temporarily yank the focused window out of the
layout to mostly fill the screen\" with @xK_backslash@ (see 'keys'').

* 'minimize' to \"make it possible to minimize windows and temporarily
removing them from the layout until they are restored\" with @xK_m@ and
shift + @xK_m@, respectively (see 'keys'').

* Ability to \"reflect a layout horizontally or vertically\" and
to toggle those reflections ("XMonad.Layout.Reflect" and "XMonad.Layout.MultiToggle").
Key bindings @xK_x@ and @xK_y@ in 'keys''.

* Added 'lessBorders'. Since @'smartBorders'@ (@= 'lessBorders' Never@) only removes
borders if \"there is only one screen and only one window\" or "a floating window
covers the entire screen\", we use @'lessBorders' Screen@ to never draw borders
on singleton screens. To differentiate between screen focus, we could use the
status bar in the future. 'noBorders' can be used for specific layouts.

* Added 'smartSpacing' to \"add a configurable amount of space around windows\",
\"except when the window is the only visible window on the current workspace\".
-}
layout' =
    -- Cutting the layout modifiers \"SmartSpacing spacing' Minimize Maximize\"
    -- from the layout name. TODO There's probably a better solution.
    renamed [CutWordsLeft 4]

    -- Never draw borders on singleton screens
    . lessBorders Screen

    -- Set experimental layout on last workspace
    . onWorkspace (last workspaces') layoutExperimental

    -- Adds custom space between windows except on singleton screens
    . smartSpacing spacing'

    -- Adds possibility to minimize and restore windows
    . minimize

    -- Allow windows to be yanked out of layout
    . maximize

    -- Adds possibility to reflect layout horizontally and vertically
    . mkToggle (single REFLECTX)
    . mkToggle (single REFLECTY)

    -- Layouts to toggle
    $ tall ||| wide ||| Grid ||| Full
      where
        -- One tall master pane to the left, columns to the right
        tall = renamed [Replace "Tall"] $ multiCol n defnT delta ratioT
        -- One wide master pane at the top, columns below
        wide = renamed [Replace "Wide"] $ Mirror (multiCol n defnW delta ratioW)
        -- Windows in each column, starting with master.
        -- Set to 0 to catch the rest.
        n      = [1]
        -- Default value for all following columns.
        defnT  = 4
        defnW  = 2
        -- Default proportion of screen occupied by master pane,
        -- or column area if the size is negative.
        ratioT = 1/2
        ratioW = 3/4
        -- How much to change size each time.
        delta  = 1/100

-- | 'Theme' used for tabs with 'tabbed'.
tabTheme :: Theme
tabTheme = def
    { activeColor         = bg
    , inactiveColor       = bg
    , urgentColor         = bg
    , activeBorderColor   = bg
    , inactiveBorderColor = bg
    , urgentBorderColor   = bg
    , activeTextColor     = blue
    , inactiveTextColor   = fg
    , urgentTextColor     = red
    , fontName            = "xft:" ++ xftFont
    , decoWidth           = 200
    , decoHeight          = 20
    }

-- | Experimental layout used on last workspace.
layoutExperimental = tabbed shrinkText tabTheme

{- | Custom 'manageHook' for managing new windows.
E.g. float a window or shift it to a specific workspace.-}
manageHook' :: ManageHook
manageHook' = composeAll
                [ className =? "skypeforlinux" --> doShift "5"
                , className =? "Pidgin"        --> doShift "5"
                , className =? "Steam"         --> doShift "5"
                , className =? "csgo_linux64"  --> doShift "8" ]

{- | Custom 'handleEventHook'.

Compose event hooks with '<+>'.

Added 'fullscreenEventHook' to \"handle applications that wish to fullscreen using
the @_NET_WM_STATE@ protocol\".
-}
eventHook' :: Event -> X All
eventHook' = fullscreenEventHook

-- | Directory of xmonad.hs.
configDir :: IO FilePath
configDir = liftM (++ "/.dotfiles/.xmonad") getHomeDirectory

-- | Directory used for haddock output.
haddockDir :: IO FilePath
haddockDir = liftM (++ "/doc") configDir

-- | Icon root directory used for status bars.
iconRoot :: String
iconRoot = ".xmobar/icons"

-- | dmenu with command line options.
dmenu :: String
dmenu = "dmenu_run -b -nb '" ++ bg ++ "' -nf '" ++ fg
        ++ "' -sf '" ++ blue ++ "' -sb '" ++ bg
        ++ "' -p '>' -fn '" ++ xftFont ++ "'"

-- | trayer with command line options.
-- TODO calc margin
-- TODO height = statusBarHeight - distance
trayer :: String
trayer = "trayer"
         ++ " --SetDockType true"
         ++ " --edge top"
         ++ " --height 19"
         ++ " --distance 1"
         ++ " --widthtype request"
         -- Using --align left with --margin, otherwise
         -- (with --align center) trayer would show up at
         -- the center of all connected screens.
         ++ " --align left"
         ++ " --margin 970"
         -- --tint specifies transparency color, so we use
         -- --transparent true, but --alpha 0 (opaque).
         -- Full transparency would show desktop background,
         -- instead of xmobar's background color.
         ++ " --transparent true"
         ++ " --alpha 0"
         ++ " --tint 0x" ++ tail bg

{- | The X Free Type /Xft/ font name.

An Xft <https://keithp.com/~keithp/render/Xft.tutorial Tutorial>.
-}
xftFont :: String
xftFont = "Inconsolata:size=11:antialias=true"

-- | Bold version of 'xftFont'.
xftFontBold :: String
xftFontBold = xftFont ++ ":weight=bold"

-- | Default pretty printing options.
defaultPP' :: (String -> String -> String -> String) -- ^ color format function
           -> (String -> String) -- ^ icon format function
           -> PP
defaultPP' c i = def
    { ppCurrent         = c blue "" . wrap "[" "]"
    , ppVisible         = wrap "(" ")"
    , ppHidden          = id
    , ppHiddenNoWindows = const ""
    , ppUrgent          = c red ""
    , ppSep             = " "
    , ppWsSep           = " "
    , ppTitle           = (c orange "" ">>= " ++) . c blue "" . shorten 50
    , ppLayout          = ppLayout' i
    , ppOrder           = \(ws:l:t:e) -> [l,ws,t] ++ e
    , ppSort            = getSortByIndex
    , ppExtras          = []
    }

-- | Icon string depending on layout name.
ppLayout' :: (String -> String) -- ^ icon format function
          -> String -- ^ layout name
          -> String
ppLayout' i "Tall" = i "layout_tall.xpm"
ppLayout' i "Wide" = i "layout_wide.xpm"
ppLayout' i "Grid" = i "layout_grid.xpm"
ppLayout' i "Full" = i "layout_full.xpm"
ppLayout' _ _      = ""

{- | Weather StationID used with Xmobar's System Monitor Plugin
<http://projects.haskell.org/xmobar/#weather-stationid-args-refreshrate Weather>.
-}
xmobarWeatherStationId :: String
xmobarWeatherStationId = "EDSB"

-- | Configuration of Xmobar plugins.
xmobarCommands :: [String]
xmobarCommands =
    [ "Run Weather " ++ quote xmobarWeatherStationId ++ " " ++ xmobarArgs wthrArgs ++ " 36000"
    , "Run DynNetwork " ++ xmobarArgs netArgs ++ " 3"
    , "Run Cpu " ++ xmobarArgs cpuArgs ++ " 5"
    , "Run Memory " ++ xmobarArgs memArgs ++ " 5"
    , "Run Date " ++ (quote . xmobarColor blue "") "%a %b %_d %l:%M" ++ " " ++ quote "date" ++ " 10"
    , "Run Uptime " ++ xmobarArgs upArgs ++ " 300"
    , "Run Volume " ++ quote "default" ++ " " ++ quote "Master" ++ " " ++ xmobarArgs volArgs ++ " 5" ]
  where
    wthrArgs = [ "-t", "<tempC>C"
               , "-L", "18", "-H", "25"
               , "-l", blue, "-n", green, "-h", red ]
    netArgs  = [ "-t", "<rxipat> <rx>"
               , "-L", "500000", "-H", "1500000"
               , "-n", green, "-h", red
               , "-S", "True"
               , "--"
               , "--rx-icon-pattern", xmobarIcon "net_%%.xpm" ]
    cpuArgs  = [ "-t", "<ipat> <total>%"
               , "-L", "3", "-H", "50"
               , "-n", green, "-h", red
               , "--"
               , "--load-icon-pattern", xmobarIcon "cpu_%%.xpm" ]
    memArgs  = [ "-t", "<usedipat> <usedratio>%"
               , "--"
               , "--used-icon-pattern", xmobarIcon "mem_%%.xpm" ]
    upArgs   = [ "-t", "up <hours> <minutes>"
               , "-S", "True" ]
    volArgs  = [ "-t", "<volumeipat> <volume>% <status>"
               , "--"
               , "-O", " ", "-c", red
               , "--volume-icon-pattern", xmobarIcon "vol_%%.xpm" ]

-- | Ordered list of Xmobar plugins.
xmobarTemplate :: [String]
xmobarTemplate = ["dynnetwork", "cpu", "memory", xmobarWeatherStationId, "date", "uptime", "default:Master"]

-- | Seperator used between Xmobar plugin templates.
xmobarTemplateSep :: String
xmobarTemplateSep = "  "

-- TODO xmobar on multiple screens
-- solution1: write own statusBar function which can spawn multiple status bars
-- solution2: figure out how Hooks.DynamicBars works
--
-- Keep in mind:
-- Somehow if xmobar -x1 is positioned ( with -o, -b or --p positon) it is moved to screen 0 to that position.
-- So if xmobar -x0 and xmobar -x1 have the same position, they are drawn on top of each other.

-- | Xmobar with command line options.
xmobar' :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar' conf = statusBar ("xmobar ~/.xmobar/xmobarrc " ++ flags) xmobarFocusedPP toggleStrutsKey conf
  where
    pos    = "Top"
    asep   = "}{"
    sep    = "%"
    templL = pad' sep "StdinReader"
    templR = intercalate xmobarTemplateSep $ map (pad' sep) xmobarTemplate
    flags  = "-f 'xft:" ++ xftFont
             ++ "' -B '" ++ bg
             ++ "' -F '" ++ fg
             ++ "' -p '" ++ pos
             ++ "' -a '" ++ asep
             ++ "' -s '" ++ sep
             ++ "' -i '" ++ iconRoot
             ++ "' -t '" ++ pad templL ++ asep ++ templR
             ++ "' -c '" ++ listString ("Run StdinReader" : xmobarCommands)
             ++ "' -d" -- same as overrideRedirect = False

-- | Custom pretty printing options for xmobar on focused screen.
xmobarFocusedPP :: PP
xmobarFocusedPP = defaultPP' xmobarColor xmobarIcon

-- TODO
-- | Custom pretty printing options for xmobar on unfocused screen.
xmobarUnfocusedPP = xmobarFocusedPP { ppTitle = const "" }

-- | Dzen with command line options.
dzen' :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzen' conf = statusBar ("dzen2 " ++ flags) dzenFocusedPP toggleStrutsKey conf
  where
    height = 20
    align  = "left"
    events = [ ("onstart", [("lower", [])]) ]
    flags  = "-fn '" ++ xftFont
            ++ "' -bg '" ++ bg
            ++ "' -fg '" ++ fg
            ++ "' -h '"  ++ show height
            ++ "' -ta '" ++ align
            ++ "' -e '"  ++ dzenEvents events
            ++ "'"

-- | Custom pretty printing options for dzen on focused screen.
dzenFocusedPP :: PP
dzenFocusedPP = defaultPP' dzenColor dzenIcon

-- TODO
-- | Custom pretty printing options for dzen on unfocused screen.
dzenUnfocusedPP = dzenFocusedPP { ppTitle = const "" }

{- | Key binding to hide status bar.

Same as in "XMonad.Hooks.DynamicLog", since it's not exported.
-}
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {modMask = modm} = (modm, xK_b)

-- | List of additional key bindings.
keys' :: [((KeyMask, KeySym), X ())]
keys' = [-- launch dmenu
            ((modMask', xK_p), spawn dmenu)
          -- print screen
          , ((0, xK_Print), spawn "scrot")
          -- additional key binding 0 -> /dev/null
          , ((modMask', xK_0), windows $ W.greedyView "/dev/null")
          , ((modMask' .|. shiftMask, xK_0), windows $ W.shift "/dev/null")
          -- re-enable Alt+Tab
          , ((mod1Mask, xK_Tab), windows W.focusDown)
          , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp)
          -- media keys
          , ((0, xF86XK_MyComputer), spawn terminal')
          , ((0, xF86XK_Explorer), spawn terminal')
          , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2dB-")
          , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2dB+")
          , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
          , ((0, xF86XK_Calculator), spawn $ terminal' ++ " -e bc -q")
          , ((0, xF86XK_HomePage), spawn "xdg-open http://google.com")
          , ((0, xF86XK_Mail), spawn "xdg-open http://mailbox.org")
          -- TODO forward to spotify via xbindkeys
          -- , ((0, xF86XK_AudioMedia), )
          -- , ((0, xF86XK_AudioPrev), )
          -- , ((0, xF86XK_AudioNext), )
          -- , ((0, xF86XK_AudioPlay), )
          -- , ((0, xF86XK_AudioStop), )
          -- maximize
          , ((modMask', xK_backslash), withFocused $ sendMessage . maximizeRestore)
          -- minimize and restore
          , ((modMask', xK_m), withFocused minimizeWindow)
          , ((modMask' .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
          -- toggle horizontal and vertical reflection
          , ((modMask' .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
          , ((modMask' .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)
          -- TODO merge?
          , ((modMask', xK_q), spawn "if command -v xmonad; then xmonad --recompile && xmonad --restart; fi")
          , ((modMask', xK_g), runHaddock)
        ]

-- | Override default configuration.

-- Additional keys are added as per XMonad.Doc.Extending
-- (http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html#g:10)
-- or rather XMonad.Util.EZConfig (additionalKeys)
config' = ewmh def
    { terminal           = terminal'
    , borderWidth        = borderWidth'
    , modMask            = modMask'
    , workspaces         = workspaces'
    , normalBorderColor  = normalBorderColor'
    , focusedBorderColor = focusedBorderColor'
    , keys               = \x -> M.union (M.fromList keys') (keys def x)
    , layoutHook         = layout'
    , manageHook         = manageHook'
    , handleEventHook    = eventHook'
    }

{- | Main entry point.

Possible status bars: 'xmobar'', 'dzen''.

When using 'statusBar', the following is already
taken care of:

* Status bar spawned via 'XMonad.Util.Run.spawnPipe'
* 'dynamicLogWithPP' added to 'logHook'
* 'ppOutput' added to 'PP'
* 'XMonad.Hooks.ManageDocks.avoidStruts' added to 'layoutHook'
* 'XMonad.Hooks.ManageDocks.manageDocks' added to 'manageHook',
'XMonad.Hooks.ManageDocks.docksEventHook' to 'handleEventHook' and
'XMonad.Hooks.ManageDocks.docksStartupHook' to 'startupHook'
via 'XMonad.Hooks.ManageDocks.docks' config modifier.
-}
main :: IO ()
main = do
    -- Create doc directory for haddock if missing
    createDirectoryIfMissing False =<< haddockDir
    spawn(trayer)
    xmonad =<< xmobar' config'

{- | Xmobar icon string with given icon name.

The 'iconRoot' is passed in 'xmobar''.

Example:

> xmobarIcon "myIcon" -> "<icon=myIcon/>"

-}
xmobarIcon :: String -> String
xmobarIcon = wrap "<icon=" "/>"

{- | Dzen icon string with given icon name.

Example:

@dzenIcon "myIcon" -> "^i('iconRoot'/myIcon)"@

-}
dzenIcon :: String -> String
dzenIcon = wrap "^i(" ")" . (++) (iconRoot ++ "/")

{- | String in quotes for use with command line.

Example:

> quote "hello" -> "\"hello\""

-}
quote :: String -> String
quote = pad' "\""

{- | Formatted Xmobar arguments for use with command line.

Example:

> xmobarArgs ["-t", "Mem: <usedratio>%"] -> "[\"-t\", \"Mem: <usedratio>%\"]"

-}
xmobarArgs :: [String] -> String
xmobarArgs = listString . map quote

{- | Formatted Dzen events, actions and options for use with command line.

Example:

> events = [ ("button1", [("exec", ["xterm", "firefox"])])
>          , ("entertitle", [("uncollapse", []), ("unhide", [])])
>          , ("button3", [("exit", [])])
>          ]
>
> dzenEvents events -> "button1=exec:xterm:firefox;entertitle=uncollapse,unhide;button3=exit"

-}
dzenEvents :: [(String, [(String, [String])])] {- ^ dzen events:
@[ (event1, [ (action1, [option1, option2, ...]), (action2, [...]), ... ])
 , (event2, [...])
 , ...]@-}
           -> String -- ^ formatted events
dzenEvents = intercalate ";" . map (\(e, as) -> e ++ "=" ++ actions as)
             -- no event without action
             . filter (\(_, as) -> (not . null) as)
  where actions = intercalate "," . map (\(a, os) ->
                    if null os then a else (a ++ ":" ++ intercalate ":" os))

{- | List of 'String' as 'String'.

Example:

> listString ["an", "example"] -> "[an, example]"

-}
listString :: [String] -> String
listString = wrap "[" "]" . intercalate ", "

{- | String padded with the same string on both sides.

Example:

> pad' "%" "hello" -> "%hello%"

-}
pad' :: String -> String -> String
pad' w = wrap w w

{- | Run haddock on xmonad.hs and the output in a \"doc\" folder next to it.

The used haddock interfaces are defined in 'iHaddock'.
-}
runHaddock :: MonadIO m => m ()
runHaddock = do
    d <- io haddockDir
    c <- io configDir
    spawn ("haddock" ++ o ++ i ++ " -o " ++ d ++ " " ++ c ++ "/xmonad.hs")
      where
        o = " -h --pretty-html --hyperlinked-source --no-print-missing-docs"
        i = concatMap (\(u,i) -> " -i " ++ u ++ "," ++ i) iHaddock

-- TODO get base version
-- | List of haddock interfaces.
iHaddock :: [(String, String)] -- ^ Documentation base URL paired with path to haddock file.
iHaddock = [("https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.9.0.0/",
             "/usr/share/doc/ghc/html/libraries/base-4.9.0.0/base.haddock"),
            ("http://xmonad.org/xmonad-docs/xmonad/",
             "/usr/share/doc/xmonad/html/xmonad.haddock"),
            ("http://xmonad.org/xmonad-docs/xmonad-contrib/",
             "/usr/share/doc/xmonad-contrib/html/xmonad-contrib.haddock")]

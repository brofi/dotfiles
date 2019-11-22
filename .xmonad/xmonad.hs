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

import Colors as C

import Control.Monad (liftM,(>=>))
import Data.List (intercalate)
import Data.Monoid (All) -- for type only
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86 -- to use xF86XK_* in key bindings
import System.Directory (getHomeDirectory,createDirectoryIfMissing)
import System.Process (readProcess)

import XMonad
import XMonad.Hooks.DynamicBars(DynamicStatusBar,DynamicStatusBarPartialCleanup
    ,dynStatusBarStartup',dynStatusBarEventHook',multiPP)
import XMonad.Hooks.DynamicLog (PP,ppCurrent,ppVisible,ppHidden
    ,ppHiddenNoWindows,ppUrgent,ppSep,ppWsSep,ppTitle,ppLayout,ppOrder,ppSort
    ,ppExtras,xmobarColor,dzenColor,shorten,pad,wrap)
import XMonad.Hooks.EwmhDesktops (ewmh,fullscreenEventHook)
import XMonad.Hooks.ManageDocks (AvoidStruts,avoidStruts,docks,ToggleStruts(ToggleStruts))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.LayoutModifier (ModifiedLayout) -- for type only
import XMonad.Layout.Maximize (maximize,maximizeRestore)
-- import XMonad.Layout.Minimize (minimize,minimizeWindow,MinimizeMsg(RestoreNextMinimizedWin))
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.MultiToggle (Toggle(Toggle),mkToggle,single)
import XMonad.Layout.NoBorders (Ambiguity(Screen),lessBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (REFLECTX(REFLECTX),REFLECTY(REFLECTY))
import XMonad.Layout.Renamed (renamed,Rename(CutWordsLeft,Replace))
import XMonad.Layout.Spacing (spacingRaw,Border(Border))
import qualified XMonad.StackSet as W -- window key bindings (e.g. additional workspace)
    (greedyView,shift,focusUp,focusDown)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByIndex) -- ppSort

-- Experimental
-- import XMonad.Layout.IM
-- import XMonad.Layout.Magnifier
import XMonad.Layout.Tabbed


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
normalBorderColor', focusedBorderColor' :: C.Color
normalBorderColor'  = bg
focusedBorderColor' = yellow

-- | When @True@ spacing is not applied if there fewer than two windows.
useSmartSpacing' :: Bool
useSmartSpacing' = True

-- | Space around screen.
screenSpacing' :: Border
screenSpacing' = Border 0 0 0 0

-- | Whether to enable screen spacing.
useScreenSpacing' :: Bool
useScreenSpacing' = False

-- | Space around windows.
windowSpacing' :: Border
windowSpacing' = Border 5 5 5 5

-- | Whether to enable window spacing.
useWindowSpacing' :: Bool
useWindowSpacing' = True

-- | Background color.
bg :: C.Color
bg = bg0 Hard

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

    -- Adds custom space between windows and screen
    . spacingRaw useSmartSpacing'
        screenSpacing' useScreenSpacing'
        windowSpacing' useWindowSpacing'

    -- Adds possibility to minimize and restore windows
    -- . minimize

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
    , inactiveTextColor   = fg0
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
                [ className =? "Skype"        --> doShift "5"
                , className =? "Pidgin"       --> doShift "5"
                , className =? "Steam"        --> doShift "8"
                , className =? "Steam"        --> doFloat
                , className =? "csgo_linux64" --> doShift "9" ]

{- | Custom 'handleEventHook'.

Compose event hooks with '<+>'.

Added 'fullscreenEventHook' to \"handle applications that wish to fullscreen using
the @_NET_WM_STATE@ protocol\".
-}
eventHook' :: Event -> X All
eventHook' = fullscreenEventHook

{- | Custom 'startupHook'.

Combine actions with '<+>'.
-}
startupHook' :: X ()
startupHook' = (haddockDir >>= io . createDirectoryIfMissing False)
               <+> io writeTrayerCmd
               <+> io writeDzenCmd
               <+> io writeXmobarCmd
               <+> spawn "pkill trayer" >> spawn ("sleep 1 && " ++ trayerCmd)

-- | Write trayer command line string to file for debugging purposes.
writeTrayerCmd :: IO()
writeTrayerCmd = getXMonadDir >>= (\d -> writeFile (d ++ "/.trayerCmd") trayerCmd)

-- | Write dzen command line string to file for debugging purposes.
writeDzenCmd :: IO()
writeDzenCmd = getXMonadDir >>= (\d -> writeFile (d ++ "/.dzenCmd") (dzenCmd (S 0)))

-- | Write xmobar command line string to file for debugging purposes.
writeXmobarCmd :: IO()
writeXmobarCmd = do
    d <- getXMonadDir
    cmd <- xmobarCmd (S 0)
    writeFile (d ++ "/.xmobarCmd") cmd

-- | Directory used for haddock output.
haddockDir :: MonadIO m => m String
haddockDir = (++ "/doc") <$> getXMonadDir

-- | Icon root directory used for status bars.
iconRoot :: String
iconRoot = ".xmobar/icons"

-- | dmenu with command line options.
dmenu :: String
dmenu = "dmenu_run -b -nb '" ++ bg ++ "' -nf '" ++ fg0
        ++ "' -sf '" ++ yellow ++ "' -sb '" ++ bg
        ++ "' -p '>' -fn '" ++ xftFont ++ "'"

-- | trayer command line string.
trayerCmd :: String
trayerCmd = "trayer"
         ++ " --SetDockType true"
         ++ " --edge top"
         ++ " --height 19"
         ++ " --distance 1"
         ++ " --widthtype request"
         ++ " --monitor primary"
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
xftFont = "Inconsolata:size=12:antialias=true"

-- | Bold version of 'xftFont'.
xftFontBold :: String
xftFontBold = xftFont ++ ":weight=bold"

-- | Default pretty printing options.
defaultPP' :: (String -> String -> String -> String) -- ^ color format function
           -> (String -> String) -- ^ icon format function
           -> PP
defaultPP' c i = def
    { ppCurrent         = c bg fg4 . pad
    , ppVisible         = c fg4 bg2 . pad
    , ppHidden          = c bg4 bg1 . pad
    , ppHiddenNoWindows = const ""
    , ppUrgent          = c bg red . pad
    , ppSep             = " "
    , ppWsSep           = ""
    , ppTitle           = (c orange "" ">>= " ++) . c fg0 "" . shorten 50
    , ppLayout          = ppLayout' i
    , ppOrder           = id
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
    , "Run Date " ++ (quote . xmobarColor yellow "") "%b %_d %l:%M" ++ " " ++ quote "date" ++ " 10"
    , "Run Uptime " ++ xmobarArgs upArgs ++ " 300"
    , "Run Volume " ++ quote "default" ++ " " ++ quote "Master" ++ " " ++ xmobarArgs volArgs ++ " 5"
    , "Run BatteryP " ++ xmobarArgs ["BAT0"] ++ " " ++ xmobarArgs batArgs ++ " 15" ]
  where
    wthrArgs = [ "-t", "<tempC>\\176C"
               , "-L", "18", "-H", "25"
               , "-l", blue, "-n", green, "-h", red ]
    netArgs  = [ "-t", "<rx> <rxipat>"
               , "-L", "500000", "-H", "1500000"
               , "-n", green, "-h", red
               , "-m", "6"
               , "-S", "True"
               , "--"
               , "--rx-icon-pattern", xmobarIcon "net_%%.xpm" ]
    cpuArgs  = [ "-t", "<total> <ipat>"
               , "-L", "3", "-H", "50"
               , "-n", green, "-h", red
               , "-p", "3"
               , "-S", "True"
               , "--"
               , "--load-icon-pattern", xmobarIcon "cpu_%%.xpm" ]
    memArgs  = [ "-t", "<usedratio> <usedipat>"
               , "-p", "3"
               , "-S", "True"
               , "--"
               , "--used-icon-pattern", xmobarIcon "mem_%%.xpm" ]
    upArgs   = [ "-t", "up <hours>:<minutes>" ]
    volArgs  = [ "-t", "<volume><status> <volumeipat>"
               , "-p", "3"
               , "-S", "True"
               , "--"
               , "-O", "", "-o", " [off]", "-c", red
               , "--volume-icon-pattern", xmobarIcon "vol_%%.xpm" ]
    batArgs  = [ "-t", "<left>% <leftipat>"
               , "-L", "15", "-H", "50"
               , "-l", red, "-h", green
               , "-p", "3"
               , "--"
               , "--off-icon-pattern", xmobarIcon "bat_off_%%.xpm"
               , "--on-icon-pattern", xmobarIcon "bat_on_%%.xpm"
               , "--idle-icon-pattern", xmobarIcon "bat_off_%%.xpm" ]

-- | Ordered list of Xmobar plugins.
xmobarTemplate :: IO [String]
xmobarTemplate = (\x -> if x then templ ++ ["battery"] else templ) <$> isLaptop
  where templ = ["cpu", "memory", "dynnetwork", xmobarWeatherStationId, "date", "uptime", "default:Master"]

-- | Seperator used between Xmobar plugin templates.
xmobarTemplateSep :: String
xmobarTemplateSep = " "

-- | The 'IO' 'Handle' to the spawned xmobar on screen with given 'ScreenId'.
xmobarStartup :: DynamicStatusBar
xmobarStartup = xmobarCmd >=> spawnPipe

-- | Kills xmobar on screen with given 'ScreenId'.
xmobarCleanup :: DynamicStatusBarPartialCleanup
xmobarCleanup (S id) = spawn $ "pkill -f 'xmobar.* -x '" ++ show id

-- | Spawns multiple xmobar instances and modifies config for status bars.
xmobar' :: LayoutClass l Window => XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
xmobar' = dynStatusBar xmobarStartup xmobarCleanup xmobarFocusedPP xmobarUnfocusedPP

-- | Xmobar command line string for given 'ScreenId'.
xmobarCmd :: ScreenId -> IO String
xmobarCmd (S id) = ("xmobar ~/.xmobar/xmobarrc " ++) <$> flags
  where
    asep   = "}{"
    sep    = "%"
    templL = pad' sep "StdinReader"
    templR = intercalate xmobarTemplateSep . map (pad' sep) <$> xmobarTemplate
    flags  = (\tr ->
               "-x " ++ show id
               ++ " -f 'xft:" ++ xftFont
               ++ "' -B '" ++ bg
               ++ "' -F '" ++ fg0
               ++ "' -a '" ++ asep
               ++ "' -s '" ++ sep
               ++ "' -i '" ++ iconRoot
               ++ "' -t '" ++ templL ++ asep ++ tr ++ " "
               ++ "' -c '" ++ listString ("Run StdinReader" : xmobarCommands)
               ++ "' -d" -- same as overrideRedirect = False
               ) <$> templR

-- | Custom pretty printing options for xmobar on focused screen.
xmobarFocusedPP :: PP
xmobarFocusedPP = defaultPP' xmobarColor xmobarIcon

-- | Custom pretty printing options for xmobar on unfocused screen.
xmobarUnfocusedPP = xmobarFocusedPP { ppTitle = const "" }

-- | The 'IO' 'Handle' to the spawned dzen on screen with given 'ScreenId'.
dzenStartup :: DynamicStatusBar
dzenStartup = spawnPipe . dzenCmd

-- | Kills dzen on screen with given 'ScreenId'.
dzenCleanup :: DynamicStatusBarPartialCleanup
dzenCleanup (S id) = spawn $ "pkill -f 'dzen2.* -xs '" ++ show id

-- | Spawns multiple dzen instances and modifies config for status bars.
dzen' :: LayoutClass l Window => XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
dzen' = dynStatusBar dzenStartup dzenCleanup dzenFocusedPP dzenUnfocusedPP

-- | Dzen command line string for given 'ScreenId'.
dzenCmd :: ScreenId-> String
dzenCmd (S id) = "dzen2 " ++ flags
  where
    height = 20
    align  = "left"
    events = [ ("onstart", [("lower", [])]) ]
    flags  = "-xs " ++ show id
            ++ " -fn '" ++ xftFont
            ++ "' -bg '" ++ bg
            ++ "' -fg '" ++ fg0
            ++ "' -h '"  ++ show height
            ++ "' -ta '" ++ align
            ++ "' -e '"  ++ dzenEvents events
            ++ "'"

-- | Custom pretty printing options for dzen on focused screen.
dzenFocusedPP :: PP
dzenFocusedPP = defaultPP' dzenColor dzenIcon

-- | Custom pretty printing options for dzen on unfocused screen.
dzenUnfocusedPP = dzenFocusedPP { ppTitle = const "" }

-- | Config modifier like 'XMonad.Hooks.DynamicLog.statusBar' but for dynamic
-- status bars.
dynStatusBar :: LayoutClass l Window
             => DynamicStatusBar -- ^ 'ScreenId' -> 'IO' 'Handle', status bar handle for screen
             -> DynamicStatusBarPartialCleanup -- ^ 'IO' (), status bar cleanup function
             -> PP -- ^ Focused 'PP'
             -> PP -- ^ Unfocused 'PP'
             -> XConfig l -- ^ Config to modify
             -> XConfig (ModifiedLayout AvoidStruts l) -- ^ Modified config
dynStatusBar start clean ppf ppu c = docks $ c
    { layoutHook      = avoidStruts (layoutHook c)
    , startupHook     = dynStatusBarStartup' start clean <+> startupHook c
    , handleEventHook = dynStatusBarEventHook' start clean <+> handleEventHook c
    , logHook         = multiPP ppf ppu <+> logHook c
    }

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
          -- , ((modMask', xK_m), withFocused minimizeWindow)
          -- , ((modMask' .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
          -- toggle horizontal and vertical reflection
          , ((modMask' .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
          , ((modMask' .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)
          -- hide status bar
          , ((modMask', xK_b), sendMessage ToggleStruts)
          -- run haddock together with recompile and restart
          , ((modMask', xK_q), runHaddock >> rr)
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
    , keys               = M.union (M.fromList keys') . keys def
    , layoutHook         = layout'
    , manageHook         = manageHook'
    , handleEventHook    = eventHook'
    , startupHook        = startupHook'
    }

{- | Main entry point.

Possible status bars: 'xmobar'', 'dzen''.
-}
main :: IO ()
main = (xmonad . xmobar') config'

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

> quote "" -> "\"\""
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
                    if null os then a else a ++ ":" ++ intercalate ":" os)

{- | List of 'String' as 'String'.

Example:

> listString [] = "[]"
> listString ["an", "example"] -> "[an, example]"

-}
listString :: [String] -> String
listString = wrap' "[" "]" . intercalate ", "

{- | String padded with the same string on both sides.

Example:

> pad' "%" "" -> "%%"
> pad' "%" "hello" -> "%hello%"

-}
pad' :: String -> String -> String
pad' w = wrap' w w

{- | String wrapped in given left and right string.

Example:

@wrap' "[" "]" "" -> "[]"
wrap' "[" "]" "myString" -> "[myString]" (same as 'wrap')@

-}
wrap' :: String -> String -> String -> String
wrap' l r w = l ++ w ++ r

{- | @True@ if the computer chassis value is 8 (Portable), 9 (Laptop),
10 (Notebook) or 14 (Sub Notebook).
-}
isLaptop :: IO Bool
isLaptop = (`elem` ["8", "9", "10", "14"]) . head . lines
    <$> readFile "/sys/class/dmi/id/chassis_type"

{- | Run haddock on xmonad.hs and the output in a \"doc\" folder next to it.

The used haddock interfaces are defined in 'iHaddock'.
-}
runHaddock :: MonadIO m => m ()
runHaddock = do
    d <- haddockDir
    c <- getXMonadDir
    i <- io $ concatMap (\(u,i) -> " -i " ++ u ++ "," ++ i) . iHaddock <$> basePkg
    spawn ("haddock" ++ o ++ i ++ " -o " ++ d ++ " " ++ c ++ "/xmonad.hs " ++ c ++ "/lib/*.hs")
      where
        o = " -h --pretty-html --hyperlinked-source --no-print-missing-docs"

-- | List of haddock interfaces.
iHaddock :: String -- ^ Base package name with version.
         -> [(String, String)] -- ^ (doc location, path to haddock file).
iHaddock b = let dd = "/usr/share/doc"
             in map (\(p, f) -> (dd ++ p, dd ++ p ++ "/" ++ f ++ ".haddock"))
               [ ("/ghc/html/libraries/" ++ b, "base")
               , ("/haskell-x11/html", "X11")
               , ("/xmonad/html", "xmonad")
               , ("/xmonad-contrib/html", "xmonad-contrib") ]

{- | Base package string with version (@ghc-pkg --simple-output list base@).

Example:

> basePkg -> "base-4.9.0.0"

-}
basePkg :: IO String
basePkg = head . lines <$> readProcess "ghc-pkg" ["--simple-output", "list", "base"] []

-- | Recompiles and restarts xmonad.
rr :: MonadIO m => m ()
rr = spawn "command -v xmonad && xmonad --recompile && xmonad --restart"

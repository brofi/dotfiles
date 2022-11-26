-- For type constraints like 'LayoutClass l Window'
{-# LANGUAGE FlexibleContexts #-}
-- Provide all standard warnings plus a few like -fwarn-unsued-imports
{-# OPTIONS_GHC -W #-}

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

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86 -- to use xF86XK_* in key bindings
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.Process (readProcess)

import XMonad hiding ((|||))
import XMonad.Actions.Minimize (minimizeWindow,withLastMinimized
    ,maximizeWindowAndFocus)
import XMonad.Actions.PhysicalScreens(viewScreen,sendToScreen
    ,horizontalScreenOrderer)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhFullscreen)
import XMonad.Hooks.ManageDocks (ToggleStruts(ToggleStruts))
import XMonad.Hooks.StatusBar (StatusBarConfig,statusBarProp,dynamicEasySBs
    ,xmonadDefProp)
import XMonad.Hooks.StatusBar.PP (PP,ppCurrent,ppVisible,ppHidden
    ,ppHiddenNoWindows,ppUrgent,ppSep,ppWsSep,ppTitle,ppLayout,ppOrder,ppSort
    ,ppExtras,xmobarColor,dzenColor,shorten,pad,wrap)
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Maximize (maximize,maximizeRestore)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.MultiToggle (Toggle(Toggle),mkToggle,single)
import XMonad.Layout.NoBorders (Ambiguity(Screen),lessBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (REFLECTX(REFLECTX),REFLECTY(REFLECTY))
import XMonad.Layout.Renamed (renamed,Rename(CutWordsLeft,Replace))
import XMonad.Layout.Spacing (spacingRaw,Border(Border))
import qualified XMonad.StackSet as W -- window key bindings (e.g. additional workspace)
    (greedyView,shift,focusUp,focusDown)
import XMonad.Prompt (XPrompt(showXPrompt),XPConfig(..),XPPosition(Bottom)
    ,mkXPrompt,mkComplFunFromList,defaultXPKeymap)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch,fuzzySort)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (windowPrompt,WindowPrompt(Goto),allWindows)
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

* Added 'spacingRaw' to \"add a configurable amount of space around windows\",
\"except when the window is the only visible window on the current workspace\".
-}
layout' =
    -- Cutting the layout modifier \"Spacing Minimize Maximize\" from the layout
    -- name. TODO There's probably a better solution.
    renamed [CutWordsLeft 3]

    -- Never draw borders on singleton screens
    . lessBorders Screen

    -- Set experimental layout on last workspace
    . onWorkspace (last workspaces') layoutExperimental

    -- Adds custom space between windows and screen
    . spacingRaw useSmartSpacing'
        screenSpacing' useScreenSpacing'
        windowSpacing' useWindowSpacing'

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
                [ className =? "Skype"        --> doShift (workspaces' !! 6)
                , className =? "Pidgin"       --> doShift (workspaces' !! 6)
                , className =? "Steam"        --> doShift (workspaces' !! 7)
                , className =? "Steam"        --> doFloat
                , className =? "csgo_linux64" --> doShift (workspaces' !! 8) ]

{- | Custom 'startupHook'.

Combine actions with '<+>'.
-}
startupHook' :: X ()
startupHook' = io (haddockDir >>= createDirectoryIfMissing False)
               <+> io writeTrayerCmd
               <+> io writeDzenCmd
               <+> io writeXmobarCmd
               <+> spawn "pkill trayer" >> spawn ("sleep 1 && " ++ trayerCmd)
               <+> spawn "! pidof picom && sleep 1 && picomr --symmetric-dock-instances xmobar"

-- | Write trayer command line string to file for debugging purposes.
writeTrayerCmd :: IO ()
writeTrayerCmd = pathInCfg ".trayerCmd" >>= flip writeFile trayerCmd

-- | Write dzen command line string to file for debugging purposes.
writeDzenCmd :: IO ()
writeDzenCmd = pathInCfg ".dzenCmd" >>= flip writeFile (dzenCmd (S 0))

-- | Write xmobar command line string to file for debugging purposes.
writeXmobarCmd :: IO ()
writeXmobarCmd = do
    f <- pathInCfg ".xmobarCmd"
    cmd <- xmobarCmd (S 0)
    writeFile f cmd

-- | Directory used for haddock output.
haddockDir :: IO FilePath
haddockDir = pathInCfg "doc"

-- | XMonad configuration directory.
xmonadDir :: IO FilePath
xmonadDir = cfgDir <$> getDirectories

-- | Path into the XMonad configuration directory.
pathInCfg :: FilePath -> IO FilePath
pathInCfg f = (</> f) <$> xmonadDir

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

-- | Used to create xmobars depending on the 'ScreenId'.
xmobar' :: ScreenId -> IO StatusBarConfig
xmobar' id = flip statusBarProp (pure xmobarPP') <$> xmobarCmd id

-- | Custom pretty printing options for xmobar.
xmobarPP' :: PP
xmobarPP' = defaultPP' xmobarColor xmobarIcon

-- | Xmobar command line string for given 'ScreenId'.
xmobarCmd :: ScreenId -> IO String
xmobarCmd (S id) = ("xmobar ~/.xmobar/xmobarrc " ++) <$> flags
  where
    asep   = "}{"
    sep    = "%"
    templL = pad' sep "XMonadLog"
    templR = intercalate xmobarTemplateSep . map (pad' sep) <$> xmobarTemplate
    flags  = (\tr ->
               "-x " ++ show id
               -- TODO use pango syntax once xmobar 0.45 is released
               ++ " -f 'xft:" ++ xftFont
               ++ "' -B '" ++ bg
               ++ "' -F '" ++ fg0
               ++ "' -a '" ++ asep
               ++ "' -s '" ++ sep
               ++ "' -i '" ++ iconRoot
               ++ "' -t '" ++ templL ++ asep ++ tr ++ " "
               ++ "' -c '" ++ listString ("Run XMonadLog" : xmobarCommands)
               ++ "' -d" -- same as overrideRedirect = False
               ) <$> templR

-- | Used to create dzen status bars depending on the 'ScreenId'.
dzen' :: ScreenId -> IO StatusBarConfig
dzen' id = pure $ statusBarProp (dzenCmd id) (pure dzenPP')

-- | Custom pretty printing options for dzen.
dzenPP' :: PP
dzenPP' = defaultPP' dzenColor dzenIcon

-- | Dzen command line string for given 'ScreenId'.
dzenCmd :: ScreenId-> String
dzenCmd (S id) = "xprop -root -notype -spy " ++ xmonadDefProp
              -- get value from: PROP_NAME = "value"
              ++ " | sed -u 's/" ++ xmonadDefProp ++ " = \"\\(.*\\)\"/\\1/'"
              ++ " | dzen2 " ++ flags
  where
    height = 20
    align  = "left"
    events = [ ("button4", [("hide", [])])
             , ("button5", [("unhide", [])]) ]
    flags  = "-p"
            ++ " -xs " ++ show id
            ++ " -fn '" ++ xftFont
            ++ "' -bg '" ++ bg
            ++ "' -fg '" ++ fg0
            ++ "' -h '"  ++ show height
            ++ "' -ta '" ++ align
            ++ "' -e '"  ++ dzenEvents events
            ++ "'"

-- | List of rebinds (defaultBinding, newBinding).
rebinds :: [((KeyMask, KeySym), (KeyMask, KeySym))]
rebinds = [ ((modMask' .|. shiftMask, xK_space), (modMask' .|. controlMask, xK_space)) ]

-- | List of additional key bindings.
keys' :: [((KeyMask, KeySym), X ())]
keys' = [-- launch dmenu
            ((modMask', xK_p), spawn dmenu)
          -- dmenu-like xmonad prompt
          , ((modMask', xK_o), shellPrompt promptConfigFuzzy)
          -- goto window prompt
          , ((modMask', xK_i), windowPrompt promptConfigFuzzy Goto allWindows)
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
          , ((modMask' .|. shiftMask, xK_m), withLastMinimized maximizeWindowAndFocus)
          -- toggle horizontal and vertical reflection
          , ((modMask' .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
          , ((modMask' .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)
          -- jump directly to layout "Full"
          , ((modMask' .|. shiftMask, xK_space), sendMessage $ JumpToLayout "Full")
          -- hide status bar
          , ((modMask', xK_b), sendMessage ToggleStruts)
          -- run haddock together with recompile and restart
          -- FIXME no more docs for archlinux haskell packages
          -- , ((modMask', xK_q), io $ runHaddock >> rr)
          , ((modMask', xK_q), rr)
          -- lock screen
          , ((modMask' .|. shiftMask, xK_l), spawn "physlock -ms -p $(whoami)@$(hostname)")
          -- ask before exiting
          , ((modMask' .|. shiftMask, xK_q), ynPrompt' promptConfig "Quit?" $ io exitSuccess)
        ]
        ++
        -- 'XMonad.Actions.PhysicalScreens' version of:
        -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
        -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
        [((modMask' .|. mask, key), f sc)
            | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
            , (f, mask) <- [(viewScreen horizontalScreenOrderer, 0)
                , (sendToScreen horizontalScreenOrderer, shiftMask)]
        ]

-- | Yes/No Prompt type instance of 'XPrompt'.
newtype YNPrompt = YNPrompt String
instance XPrompt YNPrompt where
    showXPrompt (YNPrompt title) = title ++ " "

{- | Creates a Yes/No Prompt. Positive input values are "y", "Y" and "" if no
input should be considered positive input.
-}
ynPrompt :: XPConfig -- ^ config
         -> Bool     -- ^ if no input means yes
         -> String   -- ^ title
         -> X ()     -- ^ action for positive input
         -> X ()
ynPrompt c e t a = mkXPrompt (YNPrompt t) c (mkComplFunFromList c []) ynAction
    where ynAction [] = when e a
          ynAction s  = when (length s == 1 && (toLower . head) s == 'y') a

-- | Creates a Yes/No Prompt. Positive input values are "y", "Y" and "".
ynPrompt' :: XPConfig -- ^ config
          -> String   -- ^ title
          -> X ()     -- ^ action for positive input
          -> X ()
ynPrompt' = flip ynPrompt True

-- | Base 'XMonad.Prompt' config.
promptConfig:: XPConfig
promptConfig = def
    { font = "xft:" ++ xftFont
    , bgColor = bg
    , fgColor = fg0
    , bgHLight = bg
    , fgHLight = yellow
    , borderColor = bg
    , promptBorderWidth = 0
    , position = Bottom
    , height = 20
    , defaultPrompter = ("> " ++)
    , promptKeymap =
        rewrite ((controlMask, xK_g), (controlMask, xK_d)) defaultXPKeymap
    }

-- | One-line 'XMonad.Prompt' config with with 'XMonad.Prompt.FuzzyMatch'.
promptConfigFuzzy:: XPConfig
promptConfigFuzzy = promptConfig
    { alwaysHighlight = True
    , maxComplRows = Just 1
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    }

-- | Override default configuration.

-- Additional keys are added as per XMonad.Doc.Extending
-- (http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html#g:10)
-- or rather XMonad.Util.EZConfig (additionalKeys)
config' = def
    { terminal           = terminal'
    , borderWidth        = borderWidth'
    , modMask            = modMask'
    , workspaces         = workspaces'
    , normalBorderColor  = normalBorderColor'
    , focusedBorderColor = focusedBorderColor'
    , keys               = M.union (M.fromList keys')
                         . (\k -> foldr rewrite k rebinds)
                         . keys def
    , layoutHook         = layout'
    , manageHook         = manageHook'
    , startupHook        = startupHook'
    }

{- | Main entry point.

Possible status bars: 'xmobar'', 'dzen''.
-}
main :: IO ()
main = xmonad
    . ewmhFullscreen
    . ewmh
    -- already applies 'docks' to config and 'avoidStruts' to layout
    . dynamicEasySBs xmobar'
    $ config'

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

{- | Changes a key of a map to a new key if the key to change exists and the new
key doesn't.

Example:

> mapOfSth = fromList [(1,"sth1"),(2,"sth2"),(3,"sth3")]
> rewrite (3,4) mapOfSth == fromList [(1,"sth1"),(2,"sth2"),(4,"sth3")]
> rewrite (3,2) mapOfSth == mapOfSth
> rewrite (4,1) mapOfSth == mapOfSth
> rewrite (4,5) mapOfSth == mapOfSth

-}
rewrite :: Ord k => (k,k) -- ^ (key,newKey)
                 -> M.Map k a -> M.Map k a
rewrite (o,n) m = rewrite' (M.lookup o m) (M.lookup n m)
    where rewrite' (Just a) Nothing = (M.delete o . M.insert n a) m
          rewrite' _ _ = m

{- | @True@ if the computer chassis value is 8 (Portable), 9 (Laptop),
10 (Notebook) or 14 (Sub Notebook).
-}
isLaptop :: IO Bool
isLaptop = (`elem` ["8", "9", "10", "14"]) . head . lines
    <$> readFile "/sys/class/dmi/id/chassis_type"

{- | Run haddock on xmonad.hs and the output in a \"doc\" folder next to it.

The used haddock interfaces are defined in 'iHaddock'.
-}
runHaddock :: IO ()
runHaddock = do
    d <- haddockDir
    c <- xmonadDir
    i <- concatMap (\(u,i) -> " -i " ++ u ++ "," ++ i) . iHaddock <$> basePkg
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

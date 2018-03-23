-----------------------------------------------------------------------------
-- |
-- Module      : Colors
-- Description : Color constants
--
-- Custom colors used with XMonad.
--
-----------------------------------------------------------------------------

-- TODO provide system colors.

module Colors (
    Color, Contrast(..),
    bg0, bg1, bg2, bg3, bg4,
    fg0, fg1, fg2, fg3, fg4,
    red,
    green,
    yellow,
    blue,
    purple,
    aqua,
    orange) where

-- | 'Color' is a hexadecimal RGB string with prefix @#@.
type Color = String

-- | Available modes of background contrast.
data Contrast = Hard | Medium | Soft

-- | Background color 0, depending on contrast.
bg0 :: Contrast -> Color
bg0 Hard   = bg0h
bg0 Medium = bg0m
bg0 Soft   = bg0s

-- | Background color 0 for hard contrast.
bg0h :: Color
bg0h = "#1d2021"

-- | Background color 0 for medium contrast.
bg0m :: Color
bg0m  = "#282828"

-- | Background color 0 for soft contrast.
bg0s :: Color
bg0s = "#32302f"

-- | Background color 1.
bg1 :: Color
bg1  = "#3c3836"

-- | Background color 2.
bg2 :: Color
bg2  = "#504945"

-- | Background color 3.
bg3 :: Color
bg3  = "#665c54"

-- | Background color 4.
bg4 :: Color
bg4  = "#7c6f64"

-- | Foreground color 0.
fg0 :: Color
fg0  = "#fbf1c7"

-- | Foreground color 1.
fg1 :: Color
fg1  = "#ebdbb2"

-- | Foreground color 2.
fg2 :: Color
fg2  = "#d5c4a1"

-- | Foreground color 3.
fg3 :: Color
fg3  = "#bdae93"

-- | Foreground color 4.
fg4 :: Color
fg4  = "#a89984"

-- | Color 9.
red :: Color
red = "#fb4934"

-- | Color 10.
green :: Color
green = "#b8bb26"

-- | Color 11.
yellow :: Color
yellow = "#fabd2f"

-- | Color 12.
blue :: Color
blue = "#83a598"

-- | Color 13.
purple :: Color
purple = "#d3869b"

-- | Color 14.
aqua :: Color
aqua = "#8ec07c"

-- | Additional Color.
orange :: Color
orange = "#fe8019"

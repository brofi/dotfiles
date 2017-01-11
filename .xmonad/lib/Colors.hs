-----------------------------------------------------------------------------
-- |
-- Module      : Colors
-- Description : Color constants
--
-- Custom colors used with XMonad.
--
-----------------------------------------------------------------------------

-- TODO provide system colors.

module Colors (Color,bg,fg,red,green,orange,blue) where

-- | 'Color' is a hexadecimal RGB string with prefix @#@.
type Color = String

-- | Background color.
bg :: Color
bg = "#2f343b"

-- | Foreground color.
fg :: Color
fg = "#d3dae3"

-- | Color 9.
red :: Color
red = "#ff00a0"

-- | Color 10.
green :: Color
green = "#ccff00"

-- | Color 11.
orange :: Color
orange = "#ff9f00"

-- | Color 12.
blue :: Color
blue = "#48c6ff"


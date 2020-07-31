module Terminal where

import String

data Color =
    Black   |
    Red     |
    Green   |
    Yellow  |
    Blue    |
    Magenta |
    Cyan    |
    White   |
    Reset


instance Show Color where
    show Black   = "30"
    show Red     = "31"
    show Green   = "32"
    show Yellow  = "33"
    show Blue    = "34"
    show Magenta = "35"
    show Cyan    = "36"
    show White   = "37"
    show Reset   = "0"


setColor :: Color -> String
setColor color = "\x1b[" ++ (show color) ++ "m"


resetColor :: String
resetColor = setColor Reset


{-|
  Highlight only part of the string captured by 'start' and 'end' indices
-}
highlightPartially :: String -> Int -> Int -> String
highlightPartially string start end = left ++ (setColor Red) ++ middle ++ resetColor ++ right
    where (left, middle, right) = ternarySplit string start end


{-|
  Highlight the complete string with the given color
-}
highlight :: String -> Color -> String
highlight string color = (setColor color) ++ string ++ resetColor

{-|
  Default highlighting (red)
-}
defaultHighlight :: String -> String
defaultHighlight string = highlight string Red

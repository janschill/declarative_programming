module Graphics where

{- Data definitions -}
data Point = Point Float Float
  deriving Show

data Form = Rectangle Point Point Style | Circle Point Float Style
  deriving Show

data Color = Schwarz | Rot | Gruen | Blau | Gelb
  deriving Show

data Style = Style Color
  deriving Show

{- Helper function to translate Color to english -}
translateColor :: Color -> String
translateColor Schwarz = "black"
translateColor Rot = "red"
translateColor Gruen = "green"
translateColor Blau = "blue"
translateColor Gelb = "yellow"

{- Convert a style to the CSS property and set color as value -}
styleToAttr :: Style -> String
styleToAttr (Style c) =
  let
    translatedColor = translateColor c
    eof = "\" "
    stroke = "stroke=\""
    fill = "fill=\""
  in
    stroke ++ translatedColor ++ eof ++ fill ++ translatedColor ++ eof

defaultStyle :: Style
defaultStyle = Style Schwarz

defaultPoint :: Point
defaultPoint = Point 0 0

{- Convert data type Form to a SVG representation -}
formToSVG :: Form -> String
formToSVG (Rectangle p1 p2 s) = "<rect " ++ (setSizeRectangle (Rectangle p1 p2 s)) ++ " " ++ (styleToAttr s) ++ "/>"
  where
    setSizeRectangle :: Form -> String
    setSizeRectangle (Rectangle (Point x1 y1) (Point x2 y2) _) = "x=\"" ++ (show x1) ++ "\" y=\"" ++ (show y1) ++ "\" width=\"" ++ (show (width (Point x1 y1) (Point x2 y2))) ++ "\" height=\"" ++ (show (height (Point x1 y1) (Point x2 y2))) ++ "\""

    width :: Point -> Point -> Float
    width (Point x1 _) (Point x2 _) = if x1 < x2 then x2 - x1 else x1 - x2
    height :: Point -> Point -> Float
    height (Point _ y1) (Point _ y2) = if y1 < y2 then y2 - y1 else y1 - y2

formToSVG (Circle (Point x y) r s) = "<circle cx=\"" ++ (show x) ++ "\" cy=\"" ++ (show y) ++ "\" r=\"" ++ (show r) ++ "\" " ++ (styleToAttr s) ++ "/>"


{- Wrap SVG tag around converted Form -}
toSVG :: Form -> String
toSVG form = "<svg width=\"100\" height=\"100\" xmlns=\"http://www.w3.org/2000/svg\">" ++ (formToSVG form) ++"</svg>"

{- Generate a Form Rectangle -}
rectangle :: Float -> Float -> Form
rectangle width height = Rectangle defaultPoint (Point width height) defaultStyle

{- Generate a Form Circle -}
circle :: Float -> Form
circle radius = Circle (translatePoint defaultPoint radius) radius defaultStyle
    where
      translatePoint :: Point -> Float -> Point
      translatePoint (Point x y) amount = Point (x + amount) (y + amount)

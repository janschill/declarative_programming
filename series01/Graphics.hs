module Graphics where

{- Data definitions -}
data Point = Point Float Float
  deriving Show

data Form = Rectangle Point Point Style | Circle Point Float Style
  deriving Show

data Color = Black | Red | Green | Blue | Yellow
  deriving Show

data Style = Style Color
  deriving Show

{- Some default constants -}
defaultStyle :: Style
defaultStyle = Style Black

defaultPoint :: Point
defaultPoint = Point 0 0

colorToString :: Color -> String
colorToString Black = "black"
colorToString Red = "red"
colorToString Green = "green"
colorToString Blue = "blue"
colorToString Yellow = "yellow"

{- Convert a style to the CSS property and set color as value -}
styleToAttr :: Style -> String
styleToAttr (Style c) = "stroke=\"" ++ colorToString c ++ "\" " ++ "fill=\"" ++ colorToString c ++ "\" "

{- Convert data type Form to a SVG representation -}
formToSVG :: Form -> String
formToSVG (Rectangle (Point x1 y1) (Point x2 y2) s) = "<rect x=\"" ++ show x1 ++ "\" y=\"" ++ show y1 ++ "\" width=\"" ++ show (width (Point x1 y1) (Point x2 y2)) ++ "\" height=\"" ++ show (height (Point x1 y1) (Point x2 y2)) ++ "\"" ++ " " ++ styleToAttr s ++ "/>"
  where
    width :: Point -> Point -> Float
    width (Point x1 _) (Point x2 _) = abs (x1 - x2)
    height :: Point -> Point -> Float
    height (Point _ y1) (Point _ y2) = abs (y1 - y2)

formToSVG (Circle (Point x y) r s) = "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"" ++ show r ++ "\" " ++ styleToAttr s ++ "/>"

{- Wrap SVG tag around converted Form -}
toSVG :: Form -> String
toSVG form = "<svg width=\"100\" height=\"100\" xmlns=\"http://www.w3.org/2000/svg\">" ++ formToSVG form ++"</svg>"

{- Generate a Form Rectangle -}
rectangle :: Float -> Float -> Form
rectangle width height = Rectangle defaultPoint (Point width height) defaultStyle

translatePoint :: Point -> Float -> Point
translatePoint (Point x y) amount = Point (x + amount) (y + amount)

{- Generate a Form Circle -}
circle :: Float -> Form
circle radius = Circle (translatePoint defaultPoint radius) radius defaultStyle

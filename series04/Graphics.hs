module Graphics where

import Markup

data Point = Point Float Float
  deriving Show

data Form = Rectangle Point Point Style | Circle Point Float Style
  deriving Show

data Color = Black | Red | Green | Blue | Yellow | Gray
  deriving Show

data Style = Style Color
  deriving Show

type Graphic = [Form]

data BoundingBox = Nil | Box Point Point
  deriving Show

backgroundRect :: Form
backgroundRect = Rectangle (Point 0 0) (Point 500 500) (Style Gray)

defaultStyle :: Style
defaultStyle = Style Black

colorToString :: Color -> String
colorToString Black = "black"
colorToString Red = "red"
colorToString Green = "green"
colorToString Blue = "blue"
colorToString Yellow = "yellow"
colorToString Gray = "gray"

translatePoint ::  Float -> Float -> Point -> Point
translatePoint xAmount yAmount (Point x y) = Point (x + xAmount) (y + yAmount)

translateForm :: Float -> Float -> Form ->  Form
translateForm xAmount yAmount (Rectangle p1 p2 style) =
  Rectangle (translatePoint xAmount yAmount p1) (translatePoint xAmount yAmount p2) style
translateForm xAmount yAmount (Circle p radius style) =
  Circle (translatePoint xAmount yAmount p) radius style

translate :: Float -> Float -> Graphic -> Graphic
translate xAmount yAmount graphic = map (translateForm xAmount yAmount) graphic

styleToAttr :: Style -> [(String, String)]
styleToAttr (Style c) = [("stroke", colorToString c), ("fill", colorToString c)]

formToXML :: Form -> Xml
formToXML (Rectangle (Point x1 y1) (Point x2 y2) s) =
  Tree (Element (Empty "rect" ([
    ("x", show x1), ("y", show y1),
    ("width", show (width (Point x1 y1) (Point x2 y2))),
    ("height", show (height (Point x1 y1) (Point x2 y2)))] ++ styleToAttr s))) []
  where
    width :: Point -> Point -> Float
    width (Point x1 _) (Point x2 _) = abs (x1 - x2)
    height :: Point -> Point -> Float
    height (Point _ y1) (Point _ y2) = abs (y1 - y2)
formToXML (Circle (Point x y) r s) =
  Tree (Element (Pair "circle" ([("cx", show x), ("cy", show y), ("r", show r)] ++ styleToAttr s) [])) []

toSVG :: Graphic -> Xml
toSVG graphic =
  Tree (Element (Pair "svg" [
    ("width", "500"), ("height", "500"),
    ("xmlns", "http://www.w3.org/2000/svg")] (map formToXML graphic))) []

single :: Form -> Graphic
single form = [form]

rectangle :: Float -> Float -> Graphic
rectangle width height = single (Rectangle (Point 50 50) (Point width height) defaultStyle)

circle :: Float -> Graphic
circle radius = single (Circle (translatePoint radius radius (Point 50 50)) radius defaultStyle)

{- Combine two Graphics to a single one -}
(<+>) :: Graphic -> Graphic -> Graphic
(<+>) g1 g2 = g1 ++ g2

{- Apply a color onto a Form -}
applyColorOnForm :: Color -> Form -> Form
applyColorOnForm color (Rectangle p1 p2 _) = Rectangle p1 p2 (Style color)
applyColorOnForm color (Circle p radius _) = Circle p radius (Style color)

{- Apply a color onto a Graphic -}
colored :: Color -> Graphic -> Graphic
colored color graphic = map (applyColorOnForm color) graphic

{- Combine two BoundingBoxes -}
union :: BoundingBox -> BoundingBox -> BoundingBox
union box Nil = box
union Nil box = box
union (Box (Point x11 y11) (Point x12 y12)) (Box (Point x21 y21) (Point x22 y22)) =
  Box (Point (minimum [x11, x12, x21, x22]) (minimum [y11, y12, y21, y22]))
      (Point (maximum [x11, x12, x21, x22]) (maximum [y11, y12, y21, y22]))

unions :: [BoundingBox] -> BoundingBox
unions boxes = foldr union Nil boxes

{- Get BoundingBox of a single Form -}
boundingBoxForm :: Form -> BoundingBox
boundingBoxForm (Rectangle p1 p2 _) = Box p1 p2
boundingBoxForm (Circle point radius _) =
  Box (translatePoint (-radius) (-radius) point) (translatePoint radius radius point)

{- Get BoundingBox of a Graphic -}
boundingBox :: Graphic -> BoundingBox
boundingBox graphic = unions (map boundingBoxForm graphic)

{- Get x-axis value of BoundingBox -}
rightBoundingBorder :: BoundingBox -> Float
rightBoundingBorder (Box _ (Point x _)) = x

{- Get x-axis value of BoundingBox -}
leftBoundingBorder :: BoundingBox -> Float
leftBoundingBorder (Box (Point x _) _) = x

{- Translate second Graphic on x-axis until it sits next to the BoundingBox of first Graphic -}
(|||) :: Graphic -> Graphic -> Graphic
(|||) g1 g2 =
  (<+>) g1 (translate (rightBoundingBorder (boundingBox g1) - leftBoundingBorder (boundingBox g2)) 0 g2)

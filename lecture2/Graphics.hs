module Graphics where

{- Data definitions -}
data Point = Point Float Float
  deriving Show

data Form = Rectangle Point Point Style | Circle Point Float Style
  deriving Show

data Color = Black | Red | Green | Blue | Yellow | Gray
  deriving Show

data Style = Style Color
  deriving Show

data Graphic = Nil | Cons Form Graphic
  deriving Show

data BoundingBox = Nul | Box Point Point
  deriving Show

{- Some default constants -}
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
translatePoint xamount yamount (Point x y) = Point (x + xamount) (y + yamount)

translateForm :: Float -> Float -> Form ->  Form
translateForm xamount yamount (Rectangle p1 p2 style) =
  Rectangle (translatePoint xamount yamount p1) (translatePoint xamount yamount p2) style
translateForm xamount yamount (Circle p radius style) =
  Circle (translatePoint xamount yamount p) radius style

translate :: Float -> Float -> Graphic -> Graphic
translate _ _ Nil = Nil
translate xamount yamount (Cons form graphics) =
  Cons (translateForm xamount yamount form) (translate xamount yamount graphics)

{- Convert a style to the CSS property and set color as value -}
styleToAttr :: Style -> String
styleToAttr (Style c) = "stroke=\"" ++ colorToString c ++ "\" " ++ "fill=\"" ++ colorToString c ++ "\" "

{- Convert data type Form to a SVG representation -}
formToSVG :: Form -> String
formToSVG (Rectangle (Point x1 y1) (Point x2 y2) s) =
  "<rect x=\"" ++ show x1
  ++ "\" y=\"" ++ show y1
  ++ "\" width=\"" ++ show (width (Point x1 y1) (Point x2 y2))
  ++ "\" height=\"" ++ show (height (Point x1 y1) (Point x2 y2))
  ++ "\"" ++ " " ++ styleToAttr s ++ "/>"
  where
    width :: Point -> Point -> Float
    width (Point x1 _) (Point x2 _) = abs (x1 - x2)
    height :: Point -> Point -> Float
    height (Point _ y1) (Point _ y2) = abs (y1 - y2)

formToSVG (Circle (Point x y) r s) =
  "<circle cx=\"" ++ show x
  ++ "\" cy=\"" ++ show y
  ++ "\" r=\"" ++ show r
  ++ "\" " ++ styleToAttr s ++ "/>"

{- Wrap SVG tag around Graphic -}
toSVG :: Graphic -> String
toSVG graphic =
  "<svg width=\"500\" height=\"500\" xmlns=\"http://www.w3.org/2000/svg\">"
  ++ formToSVG backgroundRect ++ aux graphic
  ++ "</svg>"
    where
      aux :: Graphic -> String
      aux Nil = ""
      aux (Cons form graphic) = formToSVG form ++ aux graphic

{- Generate a Graphic Rectangle -}
rectangle :: Float -> Float -> Graphic
rectangle width height = single (Rectangle (Point 50 50) (Point width height) defaultStyle)

{- Generate a Graphic Circle -}
circle :: Float -> Graphic
circle radius = single (Circle (translatePoint radius radius (Point 50 50)) radius defaultStyle)

{- Convert Form to Graphic -}
single :: Form -> Graphic
single form = Cons form Nil

{- Combine two Graphics to a single one -}
(<+>) :: Graphic -> Graphic -> Graphic
(<+>) graphic Nil = graphic
(<+>) Nil graphic = graphic
(<+>) (Cons n Nil) graphic = Cons n graphic
(<+>) (Cons n ns) graphic = Cons n ((<+>) ns graphic)

{- Apply a color onto a Form -}
applyColorOnForm :: Color -> Form -> Form
applyColorOnForm color (Rectangle p1 p2 _) = Rectangle p1 p2 (Style color)
applyColorOnForm color (Circle p radius _) = Circle p radius (Style color)

{- Apply a color onto a Graphic -}
colored :: Color -> Graphic -> Graphic
colored color Nil = Nil
colored color (Cons form graphics) = Cons (applyColorOnForm color form) (colored color graphics)

{- Combine two BoundingBoxes -}
union :: BoundingBox -> BoundingBox -> BoundingBox
union box Nul = box
union Nul box = box
union (Box (Point x11 y11) (Point x12 y12)) (Box (Point x21 y21) (Point x22 y22)) =
  Box (Point (minimum [x11, x12, x21, x22]) (minimum [y11, y12, y21, y22]))
      (Point (maximum [x11, x12, x21, x22]) (maximum [y11, y12, y21, y22]))

{- Get BoundingBox of a single Form -}
boundingBoxForm :: Form -> BoundingBox
boundingBoxForm (Rectangle p1 p2 _) = Box p1 p2
boundingBoxForm (Circle point radius _) =
  Box (translatePoint (-radius) (-radius) point) (translatePoint radius radius point)

{- Get BoundingBox of a Graphic -}
boundingBox :: Graphic -> BoundingBox
boundingBox Nil = Nul
boundingBox (Cons form graphic) = union (boundingBoxForm form) (boundingBox graphic)

{- Get x-axis value of BoundingBox -}
rightBoundingBorder :: BoundingBox -> Float
rightBoundingBorder (Box _ (Point x _)) = x

{- Get x-axis value of BoundingBox -}
leftBoundingBorder :: BoundingBox -> Float
leftBoundingBorder (Box (Point x _) _) = x

{- Translate second Graphic on x-axis until it sits next to the BoundingBox of first Graphic  -}
(|||) :: Graphic -> Graphic -> Graphic
(|||) g1 g2 =
  (<+>) g1 (translate ((rightBoundingBorder (boundingBox g1)) - (leftBoundingBorder (boundingBox g2))) 0 g2)

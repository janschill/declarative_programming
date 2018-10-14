import Graphics
import Markup

g1 = [
  Rectangle (Point 0 0) (Point 100 100) defaultStyle,
  Rectangle (Point 200 100) (Point 400 250) defaultStyle
  ]

g2 = [Circle (Point 350 350) 100 defaultStyle]

g3 = ((|||) g1 g2)
g3SVG = toSVG g3
g3XML = xmlToString g3SVG

main::IO ()
main = writeFile "test.svg" (xmlToString g3SVG)

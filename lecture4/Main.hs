import Graphics
import Markup

g1 = [
  Rectangle (Point 50 50) (Point 200 200) defaultStyle
  ]

g2 = [
  Circle (Point 100 100) 100 defaultStyle,
  Rectangle (Point 200 100) (Point 400 250) defaultStyle
  ]

g3 = ((|||) g1 g2)
g3SVG = toSVG g2
g3XML = xmlToString g3SVG

main::IO ()
main = writeFile "test.svg" (xmlToString g3SVG)

import Graphics

example :: Form
example = circle 50

main::IO ()
main = writeFile "graphic.svg" (toSVG example)

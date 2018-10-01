import Graphics

example :: Form
example = rectangle 100 100

main::IO ()
main = writeFile "graphic.svg" (toSVG example)

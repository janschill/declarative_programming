import Graphics

trafficLightContainer :: Graphic
trafficLightContainer = Cons (Rectangle (Point 0 0) (Point 400 200) (Style Black)) Nil

redLight :: Graphic
redLight = Cons (Circle (Point 100 100) 50 (Style Red)) Nil

yellowLight :: Graphic
yellowLight = Cons (Circle (Point 100 100) 50 (Style Yellow)) Nil

greenLight :: Graphic
greenLight = Cons (Circle (Point 100 100) 50 (Style Green)) Nil

trafficLight :: Graphic
trafficLight = (<+>) trafficLightContainer ((|||) ((|||) redLight yellowLight) greenLight)

main::IO ()
main = writeFile "trafficlight.svg" (toSVG trafficLight)

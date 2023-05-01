--- Copyright 2022 The Australian National University, All rights reserved

module View where

import CodeWorld
import Data.Text (pack)
import Model

-- | Render all the parts of a Model to a CodeWorld picture.
-- | You do not need to understand all parts of this function.
modelToPicture :: Model -> Picture
modelToPicture (Model ss t c)
  = translated 0 8 toolText
  & translated 0 7 colourText
  & translated 0 (-8) areaText
  & colourShapesToPicture ss
  & coordinatePlane
  where
    colourText = stringToText (show c)
    toolText = stringToText (toolToLabel t)
    areaText = stringToText (case t of
      RectangleTool r _ -> "Current scaling factor: " ++
        takeWhile (/='.') (show r) ++ take 2 (dropWhile (/='.') (show r))
      _ -> "")
    stringToText = lettering . pack

-- TODO
toolToLabel :: Tool -> String
toolToLabel t = case t of
  LineTool _ -> "Line: click-drag-release"
  PolygonTool _ -> "Polygon: click 3 or more times then spacebar"
  CircleTool _ -> "Circle: click-drag-release between centre and circumference"
  TriangleTool _ ->"Triangle: click-drag release for first 2 corners"
  RectangleTool _ _ -> "Rectangle: +/- to increase/decrease scaling factor; click-drag release for first 2 corners"
  CapTool _ _  -> "Cap: click-drag-release for circle, then click for cap level"


-- TODO
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture [] = blank
colourShapesToPicture ((x,colourName):a) = colourShapeToPicture (x,colourName) & 
  colourShapesToPicture a

-- TODO
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (x, cName) = coloured (colourNameToColour cName) 
  (shapeToPicture x)

-- TODO
colourNameToColour :: ColourName -> Colour
colourNameToColour  Black = black
colourNameToColour  Red = red
colourNameToColour  Orange = orange
colourNameToColour  Yellow = yellow
colourNameToColour  Green = green
colourNameToColour  Blue = blue
colourNameToColour  Purple = purple
colourNameToColour  White = white

-- TODO
shapeToPicture :: Shape -> Picture
shapeToPicture shape = case shape of 
  Line (a,b) (c,d) -> polyline [(a,b),(c,d)]
  Polygon a  -> solidPolygon a
  Rectangle r (a,b) (c,d) -> (translated (c+d-b) 
    (b*(1/70)+d*(26/35)))  (rotated (abs(c*d*pi))
      (solidRectangle ((sqrt (((b-d)^2) + ((c-a)^2)))) (r*(sqrt (((b-d)^2) + ((c-a)^2))))))
  Triangle (a,b) (c,d) ->  (solidPolygon[(a,b),(c,(d)),((a-c+a),d)])
  Circle (a,b) (c,d) -> translated (a) (b) (solidCircle (sqrt (((c-a)^2) + ((d-b)^2))))
  Cap (a,b) (c,d) s ->  translated a (s+a) --translated to the height
       (clipped (2*rad) (2* rad ) -- find the display area parallel to x-axis
         (translated 0 (-a) -- translated to actuallt solid circle point
           (solidCircle rad))) -- create solid circle
    where
      rad = (sqrt (((c-a)^2) + ((d-b)^2))) -- it means the radius of the solidcircle
    
    

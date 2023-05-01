module Main where

import Controller
import Model
import View
import Testing

-- | The list of all tests to run.
tests :: [Test]
tests = toolLabelTests ++ nextColourTests ++ nextToolTests

toolLabelTests :: [Test]
toolLabelTests =
  [ Test "LineTool"
      (assertEqual (toolToLabel (LineTool Nothing))
       "Line: click-drag-release")
  , Test "PolygonTool"
      (assertEqual (toolToLabel (PolygonTool []))
      "Polygon: click 3 or more times then spacebar")
  , Test "CircleTool"
      (assertEqual (toolToLabel (CircleTool Nothing))
      "Circle: click-drag-release between centre and circumference")
  , Test "TriangleTool"
      (assertEqual (toolToLabel (TriangleTool Nothing))
      "Triangle: click-drag release for first 2 corners")
  , Test "RectangleTool"
      (assertEqual (toolToLabel (RectangleTool 1.0 Nothing))
      "Rectangle: +/- to increase/decrease scaling factor; click-drag release for first 2 corners")
  , Test "CapTool"
      (assertEqual (toolToLabel (CapTool Nothing Nothing))
      "Cap: click-drag-release for circle, then click for cap level")
  ]

nextColourTests :: [Test]
nextColourTests =
  [ Test "Black -> Red" (assertEqual (nextColour Black) Red)
  , Test "Red -> Orange" (assertEqual (nextColour Red) Orange)
  , Test "Orange -> Yellow" (assertEqual (nextColour Orange) Yellow)
  , Test "Yellow -> Green" (assertEqual (nextColour Yellow) Green)
  , Test "Green -> Blue" (assertEqual (nextColour Green) Blue)
  , Test "Blue -> Purple" (assertEqual (nextColour Blue) Purple)
  , Test "Purple -> White" (assertEqual (nextColour Purple) White)
  , Test "White -> Black" (assertEqual (nextColour White) Black)
  ]

-- | Tests for nextTool, including tests that it doesn't cycle tools
-- midway through a draw.
nextToolTests :: [Test]
nextToolTests =
  [ Test "Line -> Polygon"
      (assertEqual (nextTool (LineTool Nothing)) (PolygonTool []))
  , Test "Polygon -> Circle"
      (assertEqual (nextTool (PolygonTool [])) (CircleTool Nothing))
  , Test "Circle -> Triangle"
      (assertEqual (nextTool (CircleTool Nothing)) (TriangleTool Nothing))
  , Test "Triangle -> Rectangle"
      (assertEqual (nextTool (TriangleTool Nothing)) (RectangleTool 1.0 Nothing))
  , Test "Rectangle -> Cap (1)"
      (assertEqual (nextTool (RectangleTool 1.0 Nothing)) (CapTool Nothing Nothing))
  , Test "Rectangle -> Cap (2)"
      (assertEqual (nextTool (RectangleTool 10.0 Nothing)) (CapTool Nothing Nothing))
  , Test "Cap -> Line"
        (assertEqual (nextTool (CapTool Nothing Nothing)) (LineTool Nothing))
  , Test "Line (in use) -> Line"
      (assertEqual (nextTool (LineTool (Just (0,1)))) (LineTool (Just (0,1))))
  , Test "Polygon (in use) -> Polygon"
      (assertEqual (nextTool (PolygonTool [(2,3)])) (PolygonTool [(2,3)]))
  , Test "Circle (in use) -> Circle"
      (assertEqual (nextTool (CircleTool (Just (4,5)))) (CircleTool (Just (4,5))))
  , Test "Triangle (in use) -> Triangle"
      (assertEqual (nextTool (TriangleTool (Just (6,7)))) (TriangleTool (Just (6,7))))
  , Test "Rectangle (in use) -> Rectangle (1)"
      (assertEqual (nextTool (RectangleTool 1.0 (Just (8,9)))) 
      (RectangleTool 1.0 (Just (8,9))))
  , Test "Rectangle (in use) -> Rectangle (2)"
      (assertEqual (nextTool (RectangleTool 10.0 (Just (0,1))))
      (RectangleTool 10.0 (Just (0,1))))
  , Test "Cap (in use) -> Cap (1)"
      (assertEqual (nextTool (CapTool (Just (1,1)) Nothing)) 
      (CapTool (Just (1,1)) Nothing))
  , Test "Cap (in use) -> Cap (1)"
      (assertEqual (nextTool (CapTool (Just (1,1)) (Just (2,2)))) 
      (CapTool (Just (1,1)) (Just (2,2))))
  ]

-- | A Haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests
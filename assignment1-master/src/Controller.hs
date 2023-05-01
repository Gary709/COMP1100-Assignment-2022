 --- Copyright 2022 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event (Model shapes tool colour) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> startModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show currentModel)) currentModel

      -- display the mystery image
      | k == "S" -> Model sample tool colour

      | k == "Backspace" || k == "Delete" -> case shapes of
         [] -> Model [] tool colour
         _:xs -> Model xs tool colour
          

      | k == " " -> case (Model shapes tool colour) of
          Model shapes (PolygonTool a ) colour -> Model 
            ((Polygon a,colour):shapes) (PolygonTool []) colour  
          _ -> Model shapes tool colour
   
      | k == "T" -> Model shapes  (nextTool tool)  colour

      | k == "C" -> Model shapes tool (nextColour colour) 

      | k == "+" || k == "=" -> case (shapes,tool) of
        ([((Rectangle a b c),colour)],(RectangleTool s Nothing)) -> Model 
          [((Rectangle (a+0.1) b c),colour)] 
            (RectangleTool (s+0.1)  Nothing) colour
        _ -> Model shapes tool colour

      | k == "-" || k == "_" -> case (shapes,tool) of
        ([((Rectangle a b c),colour)],(RectangleTool s Nothing)) -> Model 
          [((Rectangle (a-0.1) b c),colour)]  
             (RectangleTool (s-0.1)  Nothing) colour   
        _ -> Model shapes tool colour 

      -- ignore other keys
      | otherwise -> currentModel
      
      where
        k = unpack key
        

    PointerPress p -> case tool of
      PolygonTool [] -> Model 
        shapes (PolygonTool[p]) colour
      LineTool (Nothing) -> Model 
        shapes (LineTool(Just p)) colour
      CircleTool (Nothing) -> Model 
        shapes (CircleTool(Just p)) colour
      TriangleTool (Nothing) -> Model 
        shapes (TriangleTool(Just p)) colour
      RectangleTool _ (Nothing) -> Model 
        shapes (RectangleTool 0.1 (Just p )) colour
      CapTool Nothing (Nothing) -> Model 
        shapes (CapTool (Just p) Nothing ) colour
      _ -> (Model shapes tool colour)
        

    PointerRelease p -> case tool of
      PolygonTool xs -> Model [] (PolygonTool (p:xs)) colour
      LineTool (Just point) -> Model 
        [( Line point p, colour)] (LineTool (Nothing)) colour
      RectangleTool _ (Just point) -> Model 
        [(Rectangle 0.1 point p,colour)] (RectangleTool 0.1 (Nothing)) colour
      CircleTool (Just point) -> Model 
        [(Circle point p,colour)] 
          (CircleTool (Nothing)) colour

      CapTool (Just point) (Just point1) -> Model 
        shapes (CapTool (Nothing) (Nothing)) colour
      TriangleTool (Just point)  -> Model
        [( Triangle point p,colour)] 
          (TriangleTool (Nothing)) colour
      _ -> Model shapes tool colour
        
    
    -- ignore other events
    _ -> currentModel

    where
     currentModel = Model shapes tool colour

-- TODO
nextColour :: ColourName -> ColourName
nextColour c = case c of
  Black -> Red
  Red -> Orange
  Orange -> Yellow
  Yellow -> Green
  Green -> Blue
  Blue -> Purple
  Purple -> White
  White -> Black

-- TODO
nextTool :: Tool -> Tool
nextTool tool = case tool of
  LineTool Nothing -> PolygonTool []
  PolygonTool [] -> CircleTool Nothing
  CircleTool Nothing -> TriangleTool Nothing
  TriangleTool Nothing -> RectangleTool 1.0 Nothing
  RectangleTool _ Nothing -> CapTool Nothing Nothing 
  CapTool Nothing Nothing -> LineTool Nothing
  _ -> tool
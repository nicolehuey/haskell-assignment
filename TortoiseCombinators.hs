module TortoiseCombinators
       ( andThen
       , loop
       , invisibly
       , retrace
       , overlay
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen Stop i1 = i1
andThen i1 Stop = i1
andThen (Move distance i1) i2 = Move distance (i1 `andThen` i2)
andThen (Turn angle i1) i2 = Turn angle (i1 `andThen` i2)
andThen (SetStyle s i1) i2 = SetStyle s (i1 `andThen` i2)
andThen (SetColour colour i1) i2 = SetColour colour (i1 `andThen` i2)
andThen (PenDown i1) i2 = PenDown (i1 `andThen` i2)
andThen (PenUp i1) i2 = PenUp (i1 `andThen` i2)

-- Stop if loop less than 0 times
-- repeat the instructions n times using andThen and replicating the instructions
loop :: Int -> Instructions -> Instructions
loop n i
    | n <= 0 = Stop
    | otherwise = foldr (\x y -> x `andThen` y) Stop (replicate n i)

-- always ensure pen is up when the instructions are performed
-- so that drawing is invisible
invisibly :: Instructions -> Instructions
invisibly i  = PenUp (drawInvisibily False i)

-- helper function which takes in whether pen is currently up
drawInvisibily :: Bool -> Instructions -> Instructions
drawInvisibily isPenUp (Move distance i) = Move distance (drawInvisibily isPenUp i)
drawInvisibily isPenUp (Turn angle i) = Turn angle (drawInvisibily isPenUp i)
drawInvisibily isPenUp (SetStyle s i) = SetStyle s (drawInvisibily isPenUp i)
drawInvisibily isPenUp (SetColour colour i)= SetColour colour (drawInvisibily isPenUp i)
-- change status for isPenUp after PenDown instruction
drawInvisibily isPenUp (PenUp i) = drawInvisibily True i
drawInvisibily isPenUp (PenDown i) = drawInvisibily False i
-- Pendown and stop if PenDown instruction was called before
drawInvisibily False Stop = PenDown Stop
drawInvisibily True Stop = Stop

retrace :: Instructions -> Instructions
-- Set initial line and colour of start state and keep track if pen is down
retrace i = traceback i Stop initial_line initial_colour isPenDown
  where initial_line = Solid 1
        initial_colour = white
        isPenDown = True

-- helper function which keeps track of prev instruction, linestyle, colour and pen status
traceback :: Instructions -> Instructions -> LineStyle -> Colour -> Bool -> Instructions
-- Move distance and turn angle in opposite directions
traceback (Move distance i) ins initial_line initial_colour isPenDown =
  traceback i (Move (-distance) ins) initial_line initial_colour isPenDown
traceback (Turn angle i) ins initial_line initial_colour isPenDown =
  traceback i (Turn (-angle) ins) initial_line initial_colour isPenDown
-- reverse the current and new colour,linestyle,pen instructions
traceback (SetStyle s i) ins initial_line initial_colour isPenDown =
  traceback i (SetStyle initial_line ins) s initial_colour isPenDown
traceback (SetColour colour i) ins initial_line initial_colour isPenDown =
  traceback i (SetColour initial_colour ins) initial_line colour isPenDown
traceback (PenDown i) ins initial_line initial_colour isPenDown
     | isPenDown == False = traceback i (PenUp ins) initial_line initial_colour True
     | otherwise = traceback i (PenDown ins) initial_line initial_colour True
traceback (PenUp i) ins initial_line initial_colour isPenDown
    | isPenDown == True = traceback i (PenDown ins) initial_line initial_colour False
    | otherwise = traceback i (PenUp ins) initial_line initial_colour False
traceback Stop ins initial_line initial_colour isPenDown = ins

-- return to the initial state after drawing one instruction the draw over it
overlay :: [Instructions] -> Instructions
overlay [] = Stop
overlay (x:xs) = x `andThen` (invisibly $ retrace x) `andThen` overlay xs

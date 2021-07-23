module Main exposing (..)

import Playground exposing (..)


main =
    game view update initialMemory



-- Constants


jumpVelocity : Number
jumpVelocity =
    12


gravity : Number
gravity =
    -0.5


birdXPosition : Screen -> Number
birdXPosition screen =
    screen.left + screen.width / 8



-- Memory


type alias Memory =
    { y : Number
    , yVel : Number
    , isUpPressed : Bool
    }


initialMemory : Memory
initialMemory =
    { y = 0, yVel = 0, isUpPressed = False }



-- View


view : Computer -> Memory -> List Shape
view computer memory =
    [ drawBird (birdXPosition computer.screen) memory.y
    , drawPipe computer.screen
    ]


drawBird : Number -> Number -> Shape
drawBird x y =
    move x y (square blue 40)


drawPipe : Screen -> Shape
drawPipe screen =
    move 100 10 (rectangle green 40 100)



-- Update


yVelocityChanges : Memory -> Keyboard -> Number
yVelocityChanges { yVel, isUpPressed } keyboard =
    if keyboard.up && not isUpPressed then
        jumpVelocity

    else
        yVel + gravity


detectIfUpPressed : Keyboard -> Bool
detectIfUpPressed keyboard =
    if keyboard.up then
        True

    else
        False


update : Computer -> Memory -> Memory
update computer memory =
    { y = memory.y + memory.yVel
    , yVel = yVelocityChanges memory computer.keyboard
    , isUpPressed = detectIfUpPressed computer.keyboard
    }



{--
1. Get a bird that reacts to gravity and moves
 - apply gravity to the bird
 - make the bird jump




2. Pipes
3. Collisions
4. Death
4. Score
--}

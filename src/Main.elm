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


birdXPosition : Number
birdXPosition =
    -350


pipeWidth : Number
pipeWidth =
    40


pipeSpeed : Number
pipeSpeed =
    8



-- Memory


type alias Memory =
    { y : Number
    , yVel : Number
    , isUpPressed : Bool
    , pipeXPosition : Number
    , pipeHeights : ( PipeHeight, PipeHeight )
    , pipeGapModifier : Number
    }


type alias PipeHeight =
    Number


initialMemory : Memory
initialMemory =
    { y = 0
    , yVel = 0
    , isUpPressed = False
    , pipeXPosition = -3000
    , pipeHeights = ( 0, 0 )
    , pipeGapModifier = 0
    }



-- View


view : Computer -> Memory -> List Shape
view computer memory =
    [ drawBird birdXPosition memory.y
    , drawBottomPipe computer.screen.bottom (Tuple.first memory.pipeHeights) memory.pipeXPosition
    , drawTopPipe computer.screen.top (Tuple.second memory.pipeHeights) memory.pipeXPosition
    ]


drawBird : Number -> Number -> Shape
drawBird x y =
    move x y (square blue 40)


drawBottomPipe : Number -> PipeHeight -> Number -> Shape
drawBottomPipe bottomOfScreen pipeHeight pipeXPosition =
    move pipeXPosition bottomOfScreen (rectangle green pipeWidth pipeHeight)


drawTopPipe : Number -> PipeHeight -> Number -> Shape
drawTopPipe topOfScreen pipeHeight pipeXPosition =
    move pipeXPosition topOfScreen (rectangle green pipeWidth pipeHeight)



-- Update


yVelocityChanges : Memory -> Keyboard -> Number
yVelocityChanges { yVel, isUpPressed } keyboard =
    if keyboard.up && not isUpPressed then
        jumpVelocity

    else
        yVel + gravity


setPipeHeights : Screen -> Memory -> ( PipeHeight, PipeHeight )
setPipeHeights screen { pipeGapModifier, pipeXPosition, pipeHeights } =
    let
        pipeGap : Number
        pipeGap =
            screen.height / 5

        pipeHeight : Number
        pipeHeight =
            screen.height - pipeGap
    in
    if pipeXPosition < screen.left then
        ( pipeHeight + pipeGapModifier, pipeHeight - pipeGapModifier )

    else
        pipeHeights


setPipeXPosition : Screen -> Number -> Number
setPipeXPosition screen currentXPosition =
    if currentXPosition < screen.left then
        screen.right - 50

    else
        currentXPosition - pipeSpeed



-- NOTE that elm playground does not have random :(


incrementPipeGap : Screen -> Memory -> Number
incrementPipeGap screen { pipeGapModifier, pipeXPosition } =
    if pipeXPosition < screen.left then
        if pipeGapModifier + 100 > screen.top then
            screen.bottom + 100

        else
            pipeGapModifier + 100

    else
        pipeGapModifier


update : Computer -> Memory -> Memory
update computer memory =
    { y = memory.y + memory.yVel
    , yVel = yVelocityChanges memory computer.keyboard
    , isUpPressed = computer.keyboard.up
    , pipeXPosition = setPipeXPosition computer.screen memory.pipeXPosition
    , pipeHeights = setPipeHeights computer.screen memory
    , pipeGapModifier = incrementPipeGap computer.screen memory
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

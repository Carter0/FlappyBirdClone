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
    , isBirdDead : Bool
    , score : Number
    , pipeHasPassed: Bool
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
    , isBirdDead = False
    , score = 0
    , pipeHasPassed = False
    }



-- View


view : Computer -> Memory -> List Shape
view computer memory =
    [ renderPlayerOrGameOver memory
    , drawBottomPipe computer.screen.bottom (Tuple.first memory.pipeHeights) memory.pipeXPosition
    , drawTopPipe computer.screen.top (Tuple.second memory.pipeHeights) memory.pipeXPosition
    ]


renderPlayerOrGameOver : Memory -> Shape
renderPlayerOrGameOver memory =
    if memory.isBirdDead then
        drawLoseText memory

    else
        drawBird birdXPosition memory.y


drawLoseText : Memory -> Shape
drawLoseText memory =
    scale 3 (words black <| "You have died. Your score is: " ++ (String.fromFloat memory.score))


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


isBirdOffScreen : Number -> Screen -> Bool
isBirdOffScreen birdYPosition screen =
    birdYPosition < screen.bottom


isBirdTouchingPipe : Screen -> Memory -> Bool
isBirdTouchingPipe screen { y, pipeXPosition, pipeHeights, pipeGapModifier } =
    let
        xPositionsColliding : Bool
        xPositionsColliding =
            pipeXPosition - pipeWidth < birdXPosition && pipeXPosition + pipeWidth > birdXPosition

        topPipeColliding : PipeHeight -> Bool
        topPipeColliding topPipeHeight =
            y > screen.top - topPipeHeight / 2

        bottomPipeColliding : PipeHeight -> Bool
        bottomPipeColliding bottomPipeHeight =
            y < screen.bottom + bottomPipeHeight / 2

        yPositionsColliding : Bool
        yPositionsColliding =
            topPipeColliding (Tuple.second pipeHeights) || bottomPipeColliding (Tuple.first pipeHeights)
    in
    xPositionsColliding && yPositionsColliding


checkGameOver : Screen -> Memory -> Bool
checkGameOver screen memory =
    if memory.isBirdDead then
        True

    else
        isBirdOffScreen memory.y screen || isBirdTouchingPipe screen memory


checkGameOver2 : Screen -> Memory -> Number
checkGameOver2 screen memory =
    if memory.isBirdDead then
        screen.left - 1000
    else
        setPipeXPosition screen memory.pipeXPosition

incrementScore : Memory -> Number
incrementScore {score, pipeXPosition, pipeHasPassed, isBirdDead} =
    if pipeXPosition < birdXPosition && not pipeHasPassed && (not isBirdDead) then
        score + 1
    else
        score

didPipeRespawn : Memory -> Bool
didPipeRespawn memory =
    memory.pipeXPosition > 0

hasPipePassed : Memory -> Bool
hasPipePassed memory =
    if memory.pipeHasPassed then
        not (didPipeRespawn memory)
    else
        memory.pipeXPosition < birdXPosition

update : Computer -> Memory -> Memory
update computer memory =
    { y = memory.y + memory.yVel
    , yVel = yVelocityChanges memory computer.keyboard
    , isUpPressed = computer.keyboard.up
    , pipeXPosition = checkGameOver2 computer.screen memory
    , pipeHeights = setPipeHeights computer.screen memory
    , pipeGapModifier = incrementPipeGap computer.screen memory
    , isBirdDead = checkGameOver computer.screen memory
    , score = incrementScore memory
    , pipeHasPassed = hasPipePassed memory
    }

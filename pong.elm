-- this is the pong tutorial

type Input = { space:Bool, paddle:Int, paddle2:Int, delta:Time }

delta: Signal Time
delta = inSeconds <~fps 35

input: Signal Input
input = sampleOn delta <| Input <~ Keyboard.space
      		       	  	 ~ lift .y Keyboard.wasd
				 ~ lift .y Keyboard.arrows
				 ~ delta
(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

type Object a = { a | x:Float, y:Float, vx:Float vy:Float }

type Ball = Object {}

type Player = Object { score:Int }
data State = Play | Pause

type Game = { state:State, ball:Ball, player1:Player, player2:Player }

player : Float -> Player
player x = { x=x, y=0, vx=0, vy=0, score=0 }

defaultGame : Game
defaultGame =
  { state   = Pause
  , ball    = { x=0, y=0, vx=200, vy=200 }
  , player1 = player (20-halfWidth)
  , player2 = player (halfWidth-20)
  }


-- are n and m near each other?
-- specifically are they within c of each other?
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c

-- is the ball within a paddle?
within : Ball -> Player -> Bool
within ball player =
    (ball.x |> near player.x 8) && (ball.y |> near player.y 20)

-- change the direction of a velocity based on collisions
stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v



-- step the position of an object based on its velocity and a timestep
stepObj : Time -> Object a -> Object a
stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx * t
          , y <- y + vy * t }

-- move a ball forward, detecting collisions with either paddle
stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall t ({x,y,vx,vy} as ball) player1 player2 =
  if not (ball.x |> near 0 halfWidth)
  then { ball | x <- 0, y <- 0 }
  else
    let vx' = stepV vx (ball `within` player1) (ball `within` player2)
        vy' = stepV vy (y < 7-halfHeight) (y > halfHeight-7)
    in
        stepObj t { ball | vx <- vx', vy <- vy' }

-- step a player forward, making sure it does not fly off the court
stepPlyr : Time -> Int -> Int -> Player -> Player
stepPlyr t dir points player =
  let player' = stepObj t { player | vy <- toFloat dir * 200 }
      y'      = clamp (22-halfHeight) (halfHeight-22) player'.y
      score'  = player.score + points
  in
      { player' | y <- y', score <- score' }



stepGame : Input -> Game -> Game
stepGame {space,paddle1,paddle2,delta}
         ({state,ball,player1,player2} as game) =
  let score1 = if ball.x >  halfWidth then 1 else 0
      score2 = if ball.x < -halfWidth then 1 else 0

      state' = if | space            -> Play
                  | score1 /= score2 -> Pause
                  | otherwise        -> state

      ball' = if state == Pause then ball else
                  stepBall delta ball player1 player2

      player1' = stepPlyr delta paddle1 score1 player1
      player2' = stepPlyr delta paddle2 score2 player2
  in
      { game | state   <- state'
             , ball    <- ball'
             , player1 <- player1'
             , player2 <- player2' }





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


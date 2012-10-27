

There are several different way of describ games mathematically.

For our purposes we will define a game as the following:

Nodes represent places where something happens in thre game (such as a decision
by one of the players, and branches indicate the various actions the player can
choose.

A properly construced tree is called an extensive-form representation.

> type Label = String
> type Utility = Double
> data Player = Player Int deriving Show
> data Outcome = Outcome Label deriving Show
> data Tree = Terminal Outcome | Decision Player [Tree] deriving Show
> data Game = Game [Player] (Player -> Outcome -> Utility) Tree

> ex1u :: Player -> Outcome -> Utility
> ex1u (Player 1) (Outcome o) = case (o) of
>   "a" -> 0
>   "b" -> 0
>   "c" -> 3
>   "d" -> 6
>   "e" -> 1
>   "f" -> 5
> ex1u (Player 2) (Outcome o) = case (o) of
>   "a" -> 4
>   "b" -> 3.5
>   "c" -> 3
>   "d" -> 1
>   "e" -> 6
>   "f" -> 5

> ex1 :: Game
> ex1 = Game [Player 1, Player 2]
>            ex1u
>            (Decision (Player 1) [
>               Decision (Player 2) [
>                   Terminal (Outcome "a"),
>                   Terminal (Outcome "b")
>               ],
>               Decision (Player 1) [
>                   Decision (Player 2) [
>                       Terminal (Outcome "c"),
>                       Terminal (Outcome "d")
>                   ],
>                   Decision (Player 2) [
>                       Terminal (Outcome "e"),
>                       Terminal (Outcome "f")
>                   ]
>               ]
>           ])


> chooseBest :: (Outcome -> Utility) -> [Outcome] -> Outcome
> chooseBest u [] = error "no outcomes"
> chooseBest u [o] = o
> chooseBest u (o:os)
>   | u o > u o2 = o
>   | otherwise = o2
>       where o2 = chooseBest u os

> solveTree :: (Player -> Outcome -> Utility) -> Tree -> Outcome
> solveTree u (Terminal o) = o
> solveTree u (Decision p ts) = chooseBest (u p) solutions
>   where
>       solutions = map (solveTree u) ts

> solveGame :: Game -> Outcome
> solveGame (Game _ u t) = solveTree u t

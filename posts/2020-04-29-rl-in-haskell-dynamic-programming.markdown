---
title: Reinforcement Learning in Haskell - Dynamic Programming
---

# Reinforcement Learning


_OK, I'll bite. What's Reinforcement Learning?_  
Suppose you wanted to build a program to play a game like Space Invaders, or Chess, or Go. Any game where you pick up rewards along the way (in Space Invaders) or at the end (by winning in Chess), and you want to maximise your score. That's what Reinforcement Learning is for.

_So it's just for games?_  
It's the field for any kind of automated goal seeking - like, for example, self-driving cars.

_And Dynamic Programming?_  
I'm getting there!

# Approach #1: Dynamic Programming

_It sounds like the thing someone might shout in a Japanese cartoon. *DYNAMIC PROGRAMMING!*_  

Dynamic Programming is breaking up your problems into subproblems, solving those, putting it all together. Like building a house one room at a time. I'll talk through it with the code.

## Preliminaries

```haskell
> {-# LANGUAGE RecordWildCards #-}
> module ReinforcementLearning.DP where
>
> import qualified Control.Foldl as L  
> import Data.Map.Strict (Map)  
> import qualified Data.Map.Strict as Map  
> import qualified Data.Set as Set  
> import Data.Maybe (maybe)
```

We're gonna think about what we mean by a 'game' here. When you're playing Space Invaders,  you don't need to remember how you've played up to this point; everything you need to know (your points, how healthy your barriers are, etc) are on the screen. In Chess, everything is represented by your board state.

This idea of 'everything you need to know is on the board' is called the _Markov Property_, and games which have this property and you need to make decisions at each point are called _Markov Decision Problems_.

Let's define a representation of MDPs with states of type s and rewards of type r.

```haskell
> data MDP r s = MDP  
```
We want to be able to get out a list of possible states:

```haskell
>  {  states :: [s],  
```
We also want a list of things we can do if we're in a state of type s
```haskell
>     actions :: s -> [Action r s],
```
And finally we'll scale down future rewards by a 'discount' each time-step as punishment for taking too long. Double is a fraction represented in binary scientific notation to double precision.
```haskell
>     rewardDiscount :: Double
>  }
```

_Hey, what the hell's this Action business?_

Perceptive readers might have realised that we sneakily used `Action` without defining it. An action is a valid thing we can do in a state, such as move a pawn forward in chess when there's nothing blocking it. Any outcome it has gives an immediate reward and moves you to a state (which might be the same state you started in, or might be a different one).

```haskell
> data ActionResult reward state = ActionResult {_immediateReward :: reward, _targetState :: state} deriving Show
```
`deriving Show` lets us easily print out an ActionResult.

Consider the action of you flipping a coin and betting 10 quid it comes up heads. Then you have a 50/50 chance of immediately winning 10 quid and the state of the coin being heads or losing 10 quid and the state of the coin being tails. As we can see, and `Action` should be a Distribution of ActionResults. We also add an id so we know what each `Action` is in a list. Here we use a number as an identifier, but we could have used `Text` with minimal changes.

```haskell
> data Action reward state
>   = Action
>       { _actionId :: Int,
>         _possibleTransitions :: Dist (ActionResult reward state)
>       }
>   deriving Show
```

A `Dist a` is going to be a list of things of type a, with associated probabilities.
```haskell
> type Probability = Double
> data Dist a = Dist [(a, Probabilty)] deriving Show
```
A helper function to create a distribution with equal chances of picking anything from the supplied list:
```haskell
> mkUniformDist :: [a] -> Dist a
> mkUniformDist ls = Dist (map f ls) where
>   f x = (x, 1/len)
>   len = fromIntegral (length ls)
```
Here, `map` applies a function to every element in the list. In this case, the function attaches the probability `1/len` to each of the elements of the list. We need `fromIntegral` because Haskell doesn't automatically convert between number types, and refuses to divide `Int`s to give a `Double`.

We want to be able to do this with `Dist` - to be able to easily apply a function to each of the elements. A thing which can do this is called a `Functor` in Haskell:
```haskell
instance Functor (Dist) where
  fmap f (Dist ls) = Dist $ map g ls where g (a,d) = (f a, d)
```

In our previous coin-flip bet, we would have initially had a distribution of outcomes of 
`coinFlip = Dist [(10, 0.5), (-10, 0.5)]`. Suppose we wanted to double the stakes. Then we could write `fmap (*2) coinFlip`, which would give us `Dist [(20,0.5),(-20,0.5)]`.

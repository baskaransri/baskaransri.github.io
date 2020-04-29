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
And finally we'll scale down future rewards by a 'discount' each time-step as punishment for taking too long.
```haskell
>     rewardDiscount :: Double
>  }
```

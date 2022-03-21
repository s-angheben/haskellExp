# Monads for functional programmig

Experiments with haskell, in particular the idea of using monads to structure functional program.
The code is inspire by the paper "Monads for functional programming".


The main program consist of a parser and an evaluator for division expression off integer.
The idea of monad is used in the evaluator to handle
 - Exceptions: catch the divide by zero case
 - State: count the number of evaluation of an expression
 - Output: log the execution trace

Node that in these example only one type of effects can be handle at a time depending of the monad implementation, to solve the problem is possible to use monad transformes.

The evaluation function is written using the monad "interface". In this way it is possible to give a single definition of the eval function and depending on the monad used, without changing too much the code, we can have different functionality (Exceptions, State, Output).

`
------------ MONADIC VERSION
evalM :: Term -> M Int
eval (Con a) = return a
eval (Div t u) = eval t >>= 
   \a -> eval u >>= 
       \b -> return (specific a b)

-- specific is a specific function depending on the monad used
`

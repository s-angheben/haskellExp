# Monads for functional programmig

Experiments with haskell, in particular the idea of using monads to structure functional program.
The code is inspire by the paper "Monads for functional programming".

For the sake of learning no library is used and a lot of type and functions are redefined (eg: MyList, MyMonad, ...)

The main program consist of a parser and an evaluator for division expression off integer.

**evaluator**
The idea of monad is used in the evaluator to handle
 - Exceptions: catch the divide by zero case
 - State: count the number of evaluation of an expression
 - Output: log the execution trace

Node that in these example only one type of effects can be handle at a time depending of the monad choosed, so its implementation, to solve the problem is possible to use monad transformes.

The evaluation function is written using the monad "interface". In this way it is possible to give a single definition of the eval function and depending on the monad used, without changing too much the code, we can have different functionality (Exceptions, State, Output).

``` haskell
------------ MONADIC VERSION
evalM :: Term -> M Int
eval (Con a) = return a
eval (Div t u) = eval t >>= 
   \a -> eval u >>= 
       \b -> return (specific a b)

-- specific is a specific function depending on the monad used
```

**parser**
In this case the idea of monads provide a simple framework for costructing parser. At the end the parser function for the expression is the compositions of simpler parsers.

Using the do notation to make the code more clear:

``` haskell
data Term = Con Int | Div Term Term
expr = "(15/(6/2))"

termP :: MP Term
termP = do
   a <- number
   return (Con a)
   (+)
   lit '('
   t <- termP
   lit '/'
   u <- termP
   lit ')'
   return (Div t u))   
```

In conclusion Monads in functional programming can be used to help to use existing programming language features more effectively as in the evaluator example, a simple framework for compose functions in a specific way as in the parser example. It's also possible to define new language features through monads for example to give an array the property of single threaded to provide in-place array update in a functional language.

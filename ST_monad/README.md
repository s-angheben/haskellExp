Reference: "Lazy Functional State Thread"

Example.hs: contains example \
Graph.hs: contains a dfs on graph

# ST MONAD

The ST Monad give the ability to update in place a state guaranteeing that all action on the state are done in a single threaded way.
This is usefull when an algorithm need a mutable state and the performance are important, but also this concepts is used to perform input and outpu (IO monad) and to call external c program.

The ST monad is an "interface" that securely encapsulate stateful computations that manipulate multiple, named, mutable objects and at the same time keeps:

- independence order of evaluation \
        Church-Rosser property: "the ordering in which the reductions are chosen does not make a difference to the eventual result". In other words this means that there aren't side effects between functions so the ordern doesn't matter at all.
- referential transparency \
        Every expression can be replaced with its corresponding value without changing the program's behavior. This property give the possibility of equation reasoning.

- lazyness
  
## State transformer
A state transformer is a computation which transform a state indexed by the type s and return a value of type a with the new state.
``` haskell
type ST s a = State s -> (a, State s)
```
This definition is the same as the State Monad but the implemenation update the state in-place.

In this case the state is a finite mapping from reference to value, or reference to indexed arrays.

Some primitive functions:
- `newSTRef :: a -> ST s (STRef s a)`
    create a new pointer and space for the value of type a in the State s. Return the reference. STRef is parameterised over the type s and over the type a.
- `readSTRef :: STRef s a -> ST s a`
    given a reference return the value pointed without modify the State s.
- `writeSTRef :: STRef s a -> a -> ST s ()`
    update the given reference with the given value of type a.

A State transformer can be see as a thread.

**Encapsulation**
The encapsulation is assuered by the type system using parametricity. This requires the provision of a single constant with a rank-2 polumorphic type. 

### Monad
ST s is an instance of Monad.
`return :: a -> ST s a` delivers a value without affecting the state.
`thenST :: ST s a -> (a -> ST s b) -> ST s b` is the bind operator for ST s moand (>>=).
These means that is possible to compose State transformers to form a larger one. In case of bind the computations are sequential, because the state consumed by the second computation is thath produced by the first.

Escape the monad
`runST :: (forall s. ST s a) -> a` with this function is possible to extract a value from the State, so it's possible to define wrapper or larger function that use the ST monad but then only the result is important. runST takes a State transformers, create an initalial State (empty, zero reference) execute the operation and extract the result, discarding the final state.

### IO
`type IO a = ST RealWorld a`

Important: even if the IO Monad is a special type of State Transformer is not possible to escape the State Transformer, thanks to encapusulation. 
### ARRAY
operations:
    - `newSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)` 
    - `readSTArray :: Ix i => STArray s i e -> i -> ST s e`
    - `writeSTArray :: Ix i => STArray s i e -> i -> e -> ST s ()`
    - `freezeSTArray :: STArray s i e -> ST s (Array i e)` 
        Turn a mutable array into a standard haskell array, this is done through a copy. It's usefull for escape the ST monad.

reference: "Imperative functional programming", "how to declare imperative"

## Imperative style

As we already seen in the paper "monads for functional programming" monads can be used to create an "interface" to write code more effectively, when we can identify a pattern. In these example we will explore how is possible to write code in a similar way as in imperative programming languages in pure functional languages. In particular I will focus on the IO monad and Cont monad.

### IO Monad

The type `IO (a)` indicate an action that when performed may do some I/O operatuin and the return a value of type _a_.
Some primitive IO function are:
- `getChar :: IO Char` \
    is an action that, when performed, reads character from the standard input, and return the character as _Char_ type.
- `putChar :: Char -> IO ()` \
    is an action that, send the characted to the standard output when performed and then return the unit type to indicate nothing.

#### Composition operation

Monad operation: \
- `returnIO :: a -> IO a`
    doesn't perform I/O but return the value of type a in the IO context
- `bindIO :: IO a -> (a -> IO b) -> IO b`
    perform the first action (first paramenter) then use the result type (_a_) as argument to perform the second action and then return the result (_IO b_).

With these two operation is possible to define other usefull operation for example \
`thenIO :: IO a -> IO b -> IO b`: that ignore the result of the first computation (_seqIO_)




# Monad transformers and parsec

This program implement a parse and an evaluator to manipulate division expression of type Int.

**evaluator**
The mtl library is used to define through transformers a monad with State, Exception, Writer.

This is the type:
``` haskell
newtype EvalM err state log a = EvalM ( ExceptT err (StateT state (Writer log)) a)
  deriving (Functor, Applicative, Monad, MonadError err, MonadState state, MonadWriter log)

runEvalM :: EvalM e s l a -> s -> ((Either e a, s), l)
runEvalM (EvalM m) initialState = runWriter $ runStateT (runExceptT m) initialState
```

**parser**
The parsec library is used to implement this part. The type Parser is a MonadPlus.

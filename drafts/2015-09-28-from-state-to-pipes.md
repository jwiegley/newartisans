In this post we're going to look at few closely related concepts, to arrive at
an understanding of the Pipes library "from the bottom up":

  - The State and StateT monads
  - Shallow vs. embedded DSLs
  - Using the Free monad to build DSLs
  - How the pipes library is a smart evaluator

# The State monad

This article will make a lot more sense if you're already familiar with the
State monad, but in case not, here is the basic data type:

``` haskell
newtype State s a = State { runState :: s -> (a, s) }
```

I highly recommend working out the `Monad` definition for this type as an
exercise, if you haven't before; observing how the previous and subsequent
states interact via the bind operator `>>=` is good to know first-hand.

Using this type, we have two basic operations available:

``` haskell
get :: State s s
get = State $ \s -> (a, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
```

These two functions provide a *shallowly embedded DSL* for expressing state
manipulations in Haskell. It's shallow because it relies on the host language
to provide execution semantics, and does not allow us to reflect on the
structure of programs written by combining `get` and `put` -- the only
structure we can know is their type, which is too shallow to do anything but
execute them.

Do-notation turns the DSL into a mini-language right within Haskell:

``` haskell
foo :: State Int Int
foo = do $
    x <- get
    put $ x + 1
    y <- get
    return y
```

Such programs are executed by calling the function they represent with an
initial state value (here `runState` just unwraps the `State` type wrapper,
it's not doing any work at all):

``` haskell
main = print $ runState foo 0
  -- prints "1"
```

# Adding effects with StateT

We can add another term to our DSL, named `lift`, that allows arbitrary
effects to be utilized in our state manipulation language. Doing so requires
upgrading the `State` type to express such effects, and implementing the
`MonadTrans` typeclass to make the `lift` function available:

``` haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

get :: StateT s m s
put :: s -> StateT s m ()

instance MonadTrans (StateT s) where
    lift x = StateT $ \s -> (,s) <$> x
```

We can write the same programs that we could before, but now with the ability
to inject effects wherever we need to:

``` haskell
foo = do $
    x <- get
    y <- lift $ someActionInM x
    put y
    return y
```

This StateT shallow DSL explicitly using four things:

  1. The `get` function.
  2. The `put` function.
  3. The `lift` function, from `MonadTrans`
  4. The `return` function, from `Monad`

It also implicitly uses the `>>=` function from `Monad`, but I want to
emphasize these four, because by the end of this post we will see how these
four become exactly the four constructors of the Pipes Proxy type.

# From shallow to deep

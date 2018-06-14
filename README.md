# Make your action to be observable and listen events from them.

Let's imagine simple example: we want to listen to STDIN. We have `getLine` function that can capture a list of ASCII-symbols - all we need is to make this action to be observable.

```haskell
obs :: Monad f => f a -> Observable f a r
```
Good, now we want to subscribe on events and set up callback, if we want to listen events forever, we need `subscribe` function:

```haskell
subscribe :: Applicative f => Observable f a r -> (a -> f r) -> f r
```

First, we make action to be observable, then set up callback and at the end, subscribe on events:

```haskell
subscribe (obs getLine) handler
```

Our handler will count amount of characters in strings and send this into STDOUT:

```haskell
handler = print . (<>) "Length of string was: " . show . length
```

Let's try it out:
```haskell
> hello, my dear friend!
> "Length of string was: 22"
```

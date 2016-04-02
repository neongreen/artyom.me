% lens over tea #7: indexed traversals

---
series:
  top: “lens over tea”
  toplink: /#lens-over-tea
  prev: /lens-over-tea-6
unfinished: true
---

At long last, the power of indexed traversals is within our grasp.

~~~ haskell
class (Choice p,
       Corepresentable p, 
       Comonad (Corep p),
       Traversable (Corep p),
       Strong p,
       Representable p, 
       Monad (Rep p),
       MonadFix (Rep p),
       Distributive (Rep p),
       Costrong p,
       ArrowLoop p,
       ArrowApply p,
       ArrowChoice p,
       Closed p)
  => Conjoined p
~~~

Yes, all this. But before that...

# How to use indexed traversals

An indexed traversal is a traversal that provides the index of the traversed element to the function. Indexed traversals can be used like ordinary traversals too – that's what [`Conjoined`][] is for, actually.

Most of the time you can make an indexed operator of an ordinary one by adding `@` to it, or an indexed function out of ordinary one by sticking `i` in front of it. A lot of standard traversals are already indexed (e.g. [`traversed`][]). For instance, here we multiply each element in the list by its index:

~~~ {.haskell .repl}
> [1,4,2,3,1] & traversed %@~ (\i x -> (i+1) * x)
[1,8,6,12,5]
~~~

A lot of functions from base have their indexed counterparts in lens. For instance, you can use [`iany`][] to check whether any element in the list is equal to its index, and [`ifind`][] to actually find that element:

~~~ {.haskell .repl}
> iany (==) [4,2,3,3]
True

> ifind (==) [4,2,3,3]
Just (3,3)
~~~

You can combine `indices` with another traversal to only traverse elements with index satisfying some condition:

~~~ {.haskell .repl}
> over (traversed.indices (>0)) reverse ["He","was","stressed","o_O"]
["He","saw","desserts","O_o"]
~~~

When you compose indexed traversals, by default the index from the right traversal is retained:

~~~ {.haskell .repl}
> ["abcd","efgh"] ^@.. traversed.traversed
[(0,'a'),(1,'b'),(2,'c'),(3,'d'),
 (0,'e'),(1,'f'),(2,'g'),(3,'h')]
~~~

You can use [`<.`][] to retain the index from the left traversal:

~~~ {.haskell .repl}
> ["abcd","efgh"] ^@.. traversed<.traversed
[(0,'a'),(0,'b'),(0,'c'),(0,'d'),
 (1,'e'),(1,'f'),(1,'g'),(1,'h')]
~~~

Or you can use [`<.>`][] to combine both indexes into a tuple:

~~~ {.haskell .repl}
> ["abcd","efgh"] ^@.. traversed<.>traversed
[((0,0),'a'),((0,1),'b'),((0,2),'c'),((0,3),'d'),
 ((1,0),'e'),((1,1),'f'),((1,2),'g'),((1,3),'h')]
~~~

For some types, several variants of indexing are possible – for instance, when you're traversing a `Map k v`, you could say that the index should be `k` (i.e. the element's key), or `Int` (i.e. the order in which it was traversed). [`itraversed`][] gives you the former, and [`traversed`][] gives you the latter:

~~~ {.haskell .repl}
> let m = Map.fromList [("John","Doe"), ("Jane","Roe")]

> m ^@.. traversed
[(0,"Roe"),(1,"Doe")]

> m ^@.. itraversed
[("Jane","Roe"),("John","Doe")]
~~~

[`indexing`][] can turn any traversal into an indexed traversal, and [`indexed64`][] does the same but uses `Int64` as the index (in case your structure is really big).

# How indexed traversals work

Let's begin with writing a very simple indexed traversal. An ordinary `traverse` for lists looks like this:

~~~ haskell
traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse f = go
  where
    go   []   = pure []
    go (x:xs) = (:) <$> f x <*> go xs
~~~

An indexed traversal, therefore, would look like this:

~~~ haskell
itraverse :: Applicative f => (Int -> a -> f b) -> [a] -> f [b]
itraverse f = go 0
  where
    go i   []   = pure []
    go i (x:xs) = (:) <$> f i x <*> go (i+1) xs
~~~

Excellent. Now let's use our favorite trick – typeclasses – to make a type for things that can be both ordinary and indexed traversals. Basically we just need to be able to turn `a -> b` into `Int -> a -> b` (which we can always do) and then feed it to the indexed traversal. Well:

~~~ haskell
class Indexable i p where
  indexed :: p a b -> (i -> a -> b)

instance Indexable i (->) where
  indexed = const
~~~

(I use `i` instead of `Int` because in reality it doesn't have to be `Int` and also because it's exactly what lens does and this way we're just writing lens-compatible code from the start.)

Then we also need to make an instance for `Indexable i (i -> a -> b)` so that we'd be able to use indexed traversals with indexed functions. However, it's a bit complicated – how are you going to write `i -> a -> b` in the `p a b` form? What will `p` be?

Instead, we have to create a newtype:

~~~ haskell
newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance Indexable i (Indexed i) where
  indexed = runIndexed
~~~

Okay, and now we're ready to write a real indexed traversal:

~~~ haskell
type IndexedTraversal i s t a b =
  forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> f t

itraversed :: IndexedTraversal Int [a] [b] a b
itraversed f = go (0 :: Int)
  where
    go i   []   = pure []
    go i (x:xs) = (:) <$> (indexed f) i x <*> go (i+1) xs
~~~

Let's test it. First, as an ordinary traversal:

~~~ {.haskell .repl}
> itraversed print "abc"
'a'
'b'
'c'
[(),(),()]
~~~

And then as an indexed one:

~~~ {.haskell .repl}
> itraversed (Indexed (\i c -> print (i, c))) "abc"

<interactive>:1-10:
    No instance for (Indexable Int (Indexed t0))
      arising from a use of ‘itraversed’
    The type variable ‘t0’ is ambiguous
    Note: there is a potential instance available:
      instance Indexable i (Indexed i)
    In the expression:
      itraversed (Indexed (\ i c -> print (i, c))) "abc"
    In an equation for ‘it’:
        it = itraversed (Indexed (\ i c -> print (i, c))) "abc"
~~~

Ouch. What's going on? If we specify `i :: Int`, it works:

~~~ {.haskell .repl}
> itraversed (Indexed (\i c -> print (i :: Int, c))) "abc"
(0,'a')
(1,'b')
(2,'c')
[(),(),()]
~~~

If GHC says that the type variable is ambiguous, it means that some other instance is possible *in theory*, even if it's not actually present. So, can we write any other instance that would fit here?

Actually, yes:

~~~ haskell
instance Indexable Int (Indexed Int64) where
  indexed p = \i a -> (runIndexed p) (fromIntegral i) a
~~~

~~~ {.haskell .repl}
> itraversed (Indexed (\i c -> print (i :: Int64, c))) "abc"
(0,'a')
(1,'b')
(2,'c')
[(),(),()]
~~~

Okay, let's allow only one instance of `Indexable Indexed` by using type equality and the knowledge of how GHC resolves instances (which I already talked about in part 1):

~~~ haskell
instance (i ~ j) => Indexable i (Indexed j) where
  indexed = runIndexed
~~~

~~~ {.haskell .repl}
> itraversed (Indexed (\i c -> print (i, c))) "abc"
(0,'a')
(1,'b')
(2,'c')
[(),(),()]
~~~

If we don't want the user to have to use `Indexed`, we'll have to write a helper:

~~~ haskell
-- As usual, the type is kept as specific as possible
itraverseOf
  :: (Indexed i a (f b) -> s -> f t)
  -> (i -> a -> f b)    -> s -> f t
itraverseOf l f = l (Indexed f)
~~~

~~~ {.haskell .repl}
> itraverseOf itraversed (\i c -> print (i, c)) "abc"
(0,'a')
(1,'b')
(2,'c')
[(),(),()]
~~~

# Creating and modifying indexed traversals

## [`indexing`][]

Writing indexed traversals by hand isn't *that* hard, but it'd be much nicer if we could create them automatically. It's actually easy – we just need to store the current index in `State`, and increment it every time we traverse another element.

To add `State` to any `Applicative`, we can write something like this:

~~~ haskell
newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }
~~~

`runIndexing` here means “given initial index, compute new index and some result”. So, for instance, traversing just one element would look like `Indexing (\i -> (i+1, f a))`.

We also have to write an `Applicative` instance for `Indexing` so that traversing several elements would work:

~~~ haskell
instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)

  Indexing mf <*> Indexing ma = Indexing $ 
    \i -> case mf i of
      (j, ff) -> case ma j of
        (k, fa) -> (k, ff <*> fa)
~~~

`pure` doesn't traverse anything and so doesn't change the index. `mf <*> ma` passes initial index to `mf`, then passes the new index to `ma`, and then applies (unwrapped) `mf` to (unwrapped) `ma`.

We would also need a `Functor` instance:

~~~ haskell
instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
~~~

And now we can finally write [`indexing`][]. The type signature is as follows:

~~~ haskell
indexing
  :: ((a -> Indexing f b) -> s -> Indexing f t)
  -> Indexed Int a (f b) -> s -> f t
~~~

It takes an ordinary traversal (with `Indexing f` being the choice of functor – remember, we can choose any), and turns it into an `Int`-indexed traversal. (At this point I should probably remind that `Indexed Int a (f b)` is just wrapped `Int -> a -> f b`.)

Now, the implementation:

~~~ haskell
indexing l f s = ...

-- l :: (a -> Indexing f b) -> s -> Indexing f t
-- f :: Indexed Int a (f b)
-- s :: s
~~~

First we need something of type `a -> Indexing f b` to feed to `l`. This is easy – `a -> Indexing f b` is the same as `a -> Int -> (Int, f b)`, so we know `a` and current index, so we can feed them to `f` and get `f b` back, and the new index is just current index + 1.

~~~ haskell
indexing l f s = ...
  where
    f' a = Indexing $ \i -> (i+1, (runIndexed f) i a)
~~~

And after feeding `f'` and `s` to `l` we'll get `Indexing f t`, which again requires some index, but we'll just give it 0 because indexing starts at 0 – and we'll throw away the final index. Again, easy.

~~~ haskell
indexing l f s = snd $ runIndexing (l f' s) 0
  where
    f' a = Indexing $ \i -> (i+1, (runIndexed f) i a)
~~~

Let's test:

~~~ {.haskell .repl}
> itraverseOf (indexing traverse) (\i a -> print (i, a)) "abc"
(0,'a')
(1,'b')
(2,'c')
[(),(),()]
~~~

There's just one minor flaw – we produce an indexed traversal which can't be used as an ordinary traversal. This can be fixed with `Indexable` (and replacing `runIndexed` with `indexed`):

~~~ haskell
indexing
  :: Indexable Int p
  => ((a -> Indexing f b) -> s -> Indexing f t)
  -> p a (f b) -> s -> f t
indexing l f s = snd $ runIndexing (l f' s) 0
  where
    f' a = Indexing $ \i -> (i+1, (indexed f) i a)
~~~

## [`<.`][], [`.>`][], [`<.>`][]

Composing indexed traversals isn't as simple as composing ordinary traversals, because we can choose to combine indexes, use the index of the left traversal, or the index of the right traversal. I'll repeat the examples from before:

~~~ {.haskell .repl}
> ["abcd","efgh"] ^@.. traversed.traversed
[(0,'a'),(1,'b'),(2,'c'),(3,'d'),
 (0,'e'),(1,'f'),(2,'g'),(3,'h')]

> ["abcd","efgh"] ^@.. traversed<.traversed
[(0,'a'),(0,'b'),(0,'c'),(0,'d'),
 (1,'e'),(1,'f'),(1,'g'),(1,'h')]

> ["abcd","efgh"] ^@.. traversed<.>traversed
[((0,0),'a'),((0,1),'b'),((0,2),'c'),((0,3),'d'),
 ((1,0),'e'),((1,1),'f'),((1,2),'g'),((1,3),'h')]
~~~

By the way, an exercise: write `^@..` by yourself. This type will be useful:

~~~ haskell
type IndexedGetting i m s a = Indexed i a (Const m a) -> s -> Const m s

-- compare with Getting:
-- type Getting r s a = (a -> Const r a) -> s -> Const r s
~~~

And don't forget `infixl 8 ^@..`, otherwise examples might not work.

-----------------------------------------------------------------------------

Okay, now `<.>`. What should its type be? Well, the type of `(.)` for traversals is roughly as follows:

~~~ haskell
(.) :: ((a -> f b) -> s -> f t)
    -> ((x -> f y) -> a -> f b)
    -> ((x -> f y) -> s -> f t)
~~~

Then the type of `<.>` will be:

~~~ haskell
(<.>) :: (Indexed  i    a (f b) -> s -> f t)
      -> (Indexed    j  x (f y) -> a -> f b)
      -> (Indexed (i,j) x (f y) -> s -> f t)
~~~

You can write it by blindly connecting things with matching types, but it's still better to understand what's going on.

~~~ haskell
(<.>) abst xyab (Indexed fij) s = ...

-- abst :: Indexed i a (f b) -> s -> f t
-- xyab :: Indexed j x (f y) -> a -> f b

-- fij :: (i,j) -> x -> f y
-- s :: s
~~~

We're traversing all `x`s in `s`. Those `x`s are inside `a`s, and `a`s are inside `s`. When traversing an `x`, we want to know both the index of that `x` in the `a`, and the index of the `a` in `s`.

We're given a traversing function that needs an index of form `(i,j)`, so we have to somehow construct `(i,j)` for it. The `i` part is provided by `abst`, so we first run the `abst` traversal, which traverses all `a`s in our `s`:

~~~ haskell
(<.>) abst xyab (Indexed fij) s = abst (Indexed ab) s
  where
    ab i a = ...

-- ab :: i -> a -> f b
~~~

Once we have access to `a`, we can traverse all `x`s in `a`:

~~~ haskell
(<.>) abst xyab (Indexed fij) s = abst (Indexed ab) s
  where
    ab i a = xyab (Indexed xy) a
      where
        xy j x = ...

-- ab :: i -> a -> f b
-- xy :: j -> x -> f y
~~~

And finally, inside `xy` we've got access to both `x` and `i,j`, so we can give the original `fij` function what it wants:

~~~ haskell
(<.>) :: (Indexed  i    a (f b) -> s -> f t)
      -> (Indexed    j  x (f y) -> a -> f b)
      -> (Indexed (i,j) x (f y) -> s -> f t)
(<.>) abst xyab (Indexed fij) s = abst (Indexed ab) s
  where
    ab i a = xyab (Indexed xy) a
      where
        xy j x = fij (i,j) x

infixr 9 <.>
~~~

This *almost* concludes it – we also want to be able to use the resulting indexed traversal as an ordinary traversal, so we apply `Indexable` to it:

~~~ haskell
(<.>) :: Indexable (i,j) p
      => (Indexed i   a (f b) -> s -> f t)
      -> (Indexed   j x (f y) -> a -> f b)
      -> (   p        x (f y) -> s -> f t)
(<.>) abst xyab fij s = abst (Indexed ab) s
  where
    ab i a = xyab (Indexed xy) a
      where
        xy j x = (indexed fij) (i,j) x

infixr 9 <.>
~~~

And we can generalise it a bit (like it's done in lens) since we don't depend on `f` and we can also replace `s -> f t` with any `r` and nothing would change:

~~~ haskell
(<.>) :: Indexable (i,j) p
      => (Indexed i a b -> r)
      -> (Indexed j x y -> a -> b)
      -> (    p     x y -> r)
(<.>) abst xyab fij = abst (Indexed ab)
  where
    ab i a = xyab (Indexed xy) a
      where
        xy j x = (indexed fij) (i,j) x

infixr 9 <.>
~~~

Exercise: write `<.` and `.>`.

# `Conjoined`

[`Conjoined`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Internal-Indexed.html#t:Conjoined
[`traversed`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Traversal.html#v:traversed
[`itraversed`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:itraversed
[`indices`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:indices
[`indexing`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:indexing
[`indexing64`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:indexing64
[`iany`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:iany
[`ifind`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:ifind
[`<.`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:-60-.
[`<.>`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:-60-.-62-
[`.>`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html#v:.-62-

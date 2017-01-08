% lens over tea #3: folds

---
series:
  top: “lens over tea”
  toplink: /#lens-over-tea
  prev: /lens-over-tea-2
  next: /lens-over-tea-4
---

Okay, the exploration of lens continues. This post is a bit boring because folds are much less interesting than, say, prisms, but hey, I promised shorter posts.

# `Fold`

A fold is to a getter what a traversal is to a lens:

~~~ haskell
type Lens      s t a b = forall f. Functor     f => ...
type Traversal s t a b = forall f. Applicative f => ...

type Getter s a = forall f. (Contravariant f, Functor     f) => ...
type Fold   s a = forall f. (Contravariant f, Applicative f) => ...
~~~

(Isn't it cool how each combination of constraints gives us a different useful type?)

So, as you may expect, a fold is simply a getter that can return multiple values. Now let's write one without looking anywhere!

Uhhh... my imagination is broken. I'm going to look at [`Control.Lens.Fold`][] to choose a fold to reimplement.

[looks]

Meh. Most things there – like [`worded`][] – are not folds, but traversals or something else in disguise, and other things are more general than folds. We don't have much choice – let's do [`folded`][].

~~~ haskell
folded :: Foldable t => Fold (t a) a
~~~

(Actually it's an `IndexedFold`, but I'm still ignoring everything indexed.)

## `Foldable`

Just in case you don't know what [`Foldable`][] is: it's basically a class for things that have elements and from which you can extract said elements (some of the examples are `Maybe`, [`Set`][], lists, trees, etc). A popular definition is “everything that can be turned into a list”, since [`toList`][] is method of `Foldable`:

~~~ haskell
class Foldable t where
  ...
  toList :: t a -> [a]
  ...
~~~

However, it's a bit misleading, because `toList` doesn't work well on structures that can be infinite to the left. A snoc list, for instance (“snoc” is “cons” backwards):

~~~ haskell
data SList a = Empty | SList a :> a
~~~

An infinite snoc list has a last element, but no *first* element; thus, you can't apply `toList` to it and get back anything. But you can still apply [`foldMap`][] – which is the real fundamental method of `Foldable` – to it and get something back! Here's the type of `foldMap`:

~~~ haskell
-- Map each element of the structure to a monoid, and combine the results.
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
~~~

I've no idea whether it's easy to understand what it does or not so easy, but the best way to understand is probably just to look at the implementation for trees:

~~~ haskell
data Tree a = Leaf a | Branch (Tree a) a (Tree a)

instance Foldable Tree where
  -- Only 1 element – apply the function to it.
  foldMap f (Leaf a) = f a
  -- Several elements – combine everything that can be combined.
  foldMap f (Branch left x right) = foldMap f left <> f x <> foldMap f right
~~~

Well, actually the best way is to write your own implementation for various types, but I don't expect you to write anything, because this is a post and posts are meant to be read and not... whatever. Back to snoc lists, here's an instance for them as well:

~~~ haskell
instance Foldable SList where
  foldMap f Empty     = mempty
  foldMap f (xs :> x) = foldMap f xs <> f x
~~~

And an example of a left-infinite list:

~~~ haskell
enumS :: Int -> SList Int
enumS n = enumS (n+1) :> n

-- A left-infinite list.
inf :: SList Int
inf = enumS 0
~~~

And a demonstration of how `foldMap` works on it, using the [`Last`][] monoid:

~~~ {.haskell .repl}
> foldMap (Last . Just) inf
Last {getLast = Just 0}
~~~

If you want to read more about `Foldable`, there's no good place to do it, because `Foldable` is simple and boring. All knowledge you need is here:

  * [`Foldable`][] documentation.
  * The [story][wiki FTP] about how and why `Foldable` got so many extra methods in GHC 7.10 (which, by the way, means that all `Foldable` tutorials are out of date now) and also why lots of functions in `Prelude` mention `Foldable` now when previously they were just operating on lists.
  * [A post][comonad foldable] about that thing with snoc lists and how `Foldable` isn't merely the `toList` class.

[wiki FTP]: https://wiki.haskell.org/Foldable_Traversable_In_Prelude
[comonad foldable]: http://comonad.com/reader/2015/free-monoids-in-haskell/

## `folded`

Okay, yeah, right, back to `folded`. First, the signature (which I got just by expanding the definition of `Fold`):

~~~ haskell
-- folded :: Foldable t => Fold (t a) a
folded :: (Contravariant f, Applicative f, Foldable t) =>
          (a -> f a) -> t a -> f (t a)
~~~

At this point it would be helpful to recall the idea of getters *once again*.

(And at this point I also wonder: if I'm writing this whole thing and it takes me more than a dozen repetitions to understand what I'm doing, could it be a clue that something isn't right with my brain? To rephrase: am I dumb?)

The idea is as follows:

  * The getter is given a function which hides an `a` in the functor. For instance, `Const`.
  * This function is applied to the target element.
  * The type of the result, `f a`, is changed to match the type we need to get – `f s` (but the `a` is still stored in the functor).
  * That's all, the `a` has been safely carried out and can then be extracted from the result.

With folds, it becomes a bit more interesting. Instead of applying the function to one element, we apply it to every element we need to carry out – and then we *combine* resulting functors. It's not something we could actually do with functors, but that's exactly why `Fold` has an `Applicative` constraint instead – unlike ordinary functors, applicative functors can be combined.

If this isn't very clear, it'll become clearer in a moment.

~~~ haskell
folded f s = ...
~~~

Okay, so we have `f :: a -> f a` and `s :: t a`. The only thing to do is to apply one to the other, which is something that `foldMap` does:

~~~ haskell
foldMap f s :: f a
~~~

(The reason the result is `f a` is that each function returns an `f a` and then those are simply combined into a single `f a`.)

Finally, the type has to be changed from `f a` to `f (t a)`, and that's what [`coerce`][] does. Thus, the definition of `folded` is:

~~~ haskell
folded f s = coerce $ foldMap f s
~~~

Except that it's not, because if you try to compile it, you would get this type error:

~~~
Could not deduce (Data.Monoid.Monoid (f a)) …
  arising from a use of ‘foldMap’
from the context (Contravariant f, Applicative f, Foldable t)
  bound by the type signature for
             folded :: (Contravariant f, Applicative f, Foldable t) =>
                       (a -> f a) -> t a -> f (t a)
~~~

How so? Well, apparently there's no instance of `Monoid` for `f a`, even if we know that if `f` is `Contravariant` and `Applicative`, `f a` *has* to be a `Monoid`. No problem, we'll just make a newtype and write our own instance.

~~~ haskell
newtype Folding f a = Folding { getFolding :: f a }
~~~

(It's tempting to just write an instance like `(...) => Monoid (f a)` without bothering with newtypes, but then you're going to have a huge problem with overlapping instances because – remember – GHC doesn't look at the constraints when resolving instances, so it would overlap with everything else from `[a]` to `Maybe a`.)

Here goes:

~~~ haskell
instance (Contravariant f, Applicative f) => Monoid (Folding f a) where
~~~

To make it easier, just pretend that `f` is `Const` here. Do you remember the discussion of monoids and applicative functors from the previous part, where we used the `Monoid r => Applicative (Const r)` instance? Well, now we go backwards from that – if something is `Const`-like and also `Applicative`, we assume that it hides a monoid inside.

`pure` for `Applicative (Const r)`:

~~~ haskell
pure _ = Const mempty
~~~

`mempty` for `Folding`:

~~~ haskell
mempty = Folding (coerce $ pure ())
~~~

`<*>` for `Applicative (Const r)`:

~~~ haskell
Const f <*> Const v = Const (f `mappend` v)
~~~

`mappend` for `Folding`:

~~~ haskell
mappend (Folding fa) (Folding fb) = Folding (fa *> fb)
~~~

The resulting instance again because I'm not sure that the mess I wrote before is understandable:

~~~ haskell
instance (Contravariant f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding (coerce $ pure ())
  mappend (Folding fa) (Folding fb) = Folding (fa *> fb)
~~~

And `folded` itself, with newtype wrapping and unwrapping:

~~~ haskell
folded f s = coerce . getFolding $ foldMap (Folding . f) s
~~~

And a demonstration:

~~~ {.haskell .repl}
> Just True ^.. folded
[True]
~~~

## The `Monoid` formulation of `Fold`

If you have looked at [`Control.Lens.Fold`][], you could've noticed this alternative explanation of `Fold` at the top:

> A `Fold s a` is a generalization of something `Foldable`. It allows you to extract multiple results from a container. A `Foldable` container can be characterized by the behavior of
>
> ~~~ haskell
> foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
> ~~~
>
> Since we want to be able to work with monomorphic containers, we could generalize this signature to
>
> ~~~ haskell
> forall m. Monoid m => (a -> m) -> s -> m
> ~~~
>
> and then decorate it with `Const` to obtain
>
> ~~~ haskell
> type Fold s a = forall m. Monoid m => Getting m s a
> ~~~
>
> Every `Getter` is a valid `Fold` that simply doesn't use the `Monoid` it is passed.
>
> In practice the type we use is slightly more complicated to allow for better error messages and for it to be transformed by certain `Applicative` transformers.

It's a pretty clear explanation, and I don't really have anything to add to it.

## `both`

This one is an exercise, which is much simpler than `folded` and so should be really trivial if it has clicked for you.

~~~ haskell
bothF :: Fold (a, a) a
bothF f s = ...
~~~

~~~ {.haskell .repl}
> (1,2) ^.. bothF
[1,2]
~~~

If you did it right, your code should be simpler than the code for actual `both`.

Also, by the way, write actual `both :: Traversal' (a, a) a` as well.

Also, using `both` in the definition of `bothF` is prohibited.

Also, I have said before that I don't expect you to write anything, but come on, this one is *really* simple. If you don't write it or at least spell mentally in your head, you're a chicken.

## `replicated`

Yet another fold to reimplement because reimplementing things is *so* fun.

Fine, not really.

Whatever.

~~~ {.haskell .repl}
> 5 ^.. replicated 3
[5,5,5]
~~~

It'd be as easy as `bothF` if we had `replicateA`, but we only have [`replicateM`][] and that one is for monads and not applicatives, so we have to write it ourselves with [`sequenceA`][] and [`replicate`][]:

~~~ haskell
replicated :: Int -> Fold a a
replicated n f s = coerce $ sequenceA (replicate n (f s))
~~~

## Combining folds

There's a somewhat not very well-known trick for combining folds. Say, we want to make a fold that would combine elements from several folds – how could we do it?

Pretty easy, actually. Just get the result from one fold, from another fold, and combine them with `*>`:

~~~ haskell
foldCombine :: Fold s a -> Fold s a -> Fold s a
foldCombine fa fb = \l s -> fa l s *> fb l s
~~~

~~~ {.haskell .repl}
> (1,2) ^.. foldCombine _2 _1
[2,1]
~~~

However, for common usage – when you're going to apply `^..` or something like that straight away – there's a much simpler way. Just use `<>`:

~~~ {.haskell .repl}
> (1,2,3) ^.. (_3 <> _1 <> _2)
[3,1,2]
~~~

Why does it work? There are 2 pieces to the puzzle:

  1. Due to an existing instance of `Monoid b => Monoid (a -> b)`, all functions with *any* amount of arguments – provided that the result is a monoid – are monoids themselves.

  2. Usually the functor used in place of `f` would be `Const`, and it's a monoid already without the need for `Folding` and our additional instance.

(If it's still not clear, then I guess it's going to be another exercise.)

# `Fold1`

An ordinary `Fold` can easily return 0 elements. Can we write a type for folds that *have* to return 1 or more elements?

With the monoid formulation, it'd be pretty easy – just replace `Monoid` with [`Semigroup`][]. (A semigroup is a monoid without `mempty`, and we might [get them in base][semigroup proposal] soon, or we might not. Currently they live in the [semigroups](@hackage) package, and it has more dependencies than I would like. Actually, I don't use lens itself for the same reason.) Anyway, since a fold with a `Semigroup m` constraint would have to work on *anything* that is a semigroup, and a non-empty list is a semigroup, there'd be no way for it to return an empty list. Ha.

[semigroup proposal]: http://www.reddit.com/r/haskell/comments/30s1t2/proposal_make_semigroup_as_a_superclass_of_monoid/

However, we use the `Contravariant f, Applicative f` formulation. Here, the analog of `mempty` is `pure` – so, to get rid of empty folds, we have to get rid of `pure`. Is there some class which gives us composition without `pure`?..

Yep. It's called [`Apply`][], it lives in Edward's [semigroupoids](@hackage) package, and its sole operation is [`<.>`][] (`.>` and `<.` don't count). With it, we can declare [`Fold1`][]:

~~~ haskell
type Fold1 s a = forall f. (Contravariant f, Apply f) =>
                 (a -> f a) -> s -> f s
~~~

And with `.>`, we can write non-empty folds. Such as [`iterated`][], for instance:

~~~ {.haskell .repl}
> 3 ^.. takingWhile (<100000000) (iterated (^2))
[3,9,81,6561,43046721]

> 3 ^.. iterated (^2)
[3,9,81,6561,43046721,...
~~~

~~~ haskell
iterated :: (a -> a) -> Fold1 a a
iterated f g a = go a
  where go a = g a .> go (f a)
~~~

**Open question:** `iterated` doesn't actually use `Contravariant`, so its real type signature is

~~~ haskell
Apply f => (a -> a) -> (a -> f a) -> a -> f a

Apply f => (a -> a) -> LensLike' f a a
~~~

and if you tried to apply `set` or `over` to it, everything would typecheck. Can this less restricted `iterated` can be used in any way in which our `iterated` can't?

-----------------------------------------------------------------------------

Oh, and by the way: the same trick gives us [`Traversal1`][], which traverses 1 or more values. Another name for it is “relevant traversal”.

-----------------------------------------------------------------------------

Some googling revealed that nobody ever uses `Traversal1` or `Fold1` (in public..?), so I can't say when they are useful.

`Fold` by itself is probably not very useful either, because most things that are folds are actually more than folds, the same way most instances of `Foldable` are also functors.

Such is life.

-----------------------------------------------------------------------------

The most used functions from `Control.Lens.Fold` are `*Of` functions, but I don't like them. Besides, they aren't really related to folds, they just take everything that is *at least* a fold, which is pretty much everything.



[`Control.Lens.Fold`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html
[`Apply`]: http://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html#t:Apply
[`<.>`]: http://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html#v:-60-.-62-
[`folded`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:folded
[`Foldable`]: http://hackage.haskell.org/package/base/docs/Data-Foldable.html#t:Foldable
[`toList`]: http://hackage.haskell.org/package/base/docs/Data-Foldable.html#v:toList
[`foldMap`]: http://hackage.haskell.org/package/base/docs/Data-Foldable.html#v:foldMap
[`Set`]: http://hackage.haskell.org/package/containers/docs/Data-Set.html#t:Set
[`Last`]: http://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Last
[`coerce`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:coerce
[`replicateM`]: http://hackage.haskell.org/package/base/docs/Control-Monad.html#v:replicateM
[`sequenceA`]: http://hackage.haskell.org/package/base/docs/Data-Traversable.html#v:sequenceA
[`replicate`]: http://hackage.haskell.org/package/base/docs/Data-List.html#v:replicate
[`Fold1`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Fold1
[`Traversal1`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Traversal1
[`iterated`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:iterated
[`worded`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:worded
[`Semigroup`]: http://hackage.haskell.org/package/semigroups/docs/Data-Semigroup.html#t:Semigroup

% lens over tea #5: prisms

---
series:
  top: “lens over tea”
  toplink: /#lens-over-tea
  prev: /lens-over-tea-4
---

The exploration of lenses continues! Today we'll finally figure out how prisms are implemented and what they are for. Also there are more exercises than in the previous posts, so perhaps you should read it with GHCi running (and not on your phone).

# A recap of isomorphisms

Okay, I wrote the last post like 5 months ago and I forgot a lot since then and you probably forgot everything too, so first there'll be a short recap of isomorphisms (which I don't recommend skipping because it has drawings of boxes, which I'll also be using when talking about prisms).

An isomorphism is a thing that lets you convert `s` to `a` and vice-versa:

~~~ haskell
type Iso s t a b =
  forall p f. (Profunctor p, Functor f) =>
  p a (f b) -> p s (f t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b

from :: Iso s t a b -> Iso b a t s
~~~

You can use an `Iso` as a lens (to get `a` from `s`), or you can reverse it with `from` and use it as a wrapper (to get `s` from `a`).

Well, actually you're getting `t` from `b`, and not `s` from `a`, but all `Iso`s have to be polymorphic enough for it not to matter; that is, you should be able to turn `s` into `t` and `a` into `b` just by renaming type variables. Here's an example showing how `_1` is polymorphic in this way:

~~~ haskell
_1 :: Lens (a, x) (b, x) a b
_1 :: Lens (b, x) (a, x) b a  -- the same thing!
~~~

(This all is explained in greater detail [here][lot 4 lens families].)

[lot 4 lens families]: /lens-over-tea-4#lens-families

Okay, but what are profunctors? Again, this was explained in the previous post:

> The point of profunctors is that if you're given a `p a b`, you can treat it as an opaque “black box”, some kind of relationship between `a` and `b` – you can add a filter to the black box which would modify its output, and you can add another filter which would modify its input, but you can't modify the black box itself in any way and you can't inspect the input in any way (because, after all, there might not even be any) or get any information from one filter to another.

~~~ haskell
class Profunctor p where
  -- Attach a function to the input.
  lmap :: (a -> b) -> p b c -> p a c
  -- Attach a function to the output.
  rmap :: (b -> c) -> p a b -> p a c
~~~

This is `a -> b` (from now on I'll be referring to profunctors as boxes):

               x-----------x
               |           |
               |_____)_____|
               aaaaaa)bbbbbb
               |‾‾‾‾‾)‾‾‾‾‾|
               |           |
               x-----------x

This is what applications of `lmap` and `rmap` do:

               x-----------x
          _____|           |_____
         |  )  |_____)_____|  )  |
         sss)aaaaaaaa)bbbbbbbb)ttt
         |  )  |‾‾‾‾‾)‾‾‾‾‾|  )  |
          ‾‾‾‾‾|           |‾‾‾‾‾
               x-----------x

Now: an iso can convert `p a (f b)` to `p s (f t)`, for any `p`. And `p` is a profunctor. And the only thing you can do with an arbitrary profunctor is attach a function to its input or output (or both). So, in order for an iso to work, it must contain functions `s -> a` and `f b -> f t` in it, and `f b -> f t` can be converted to `b -> t`:

~~~ haskell
type FBT b t = forall f. Functor f => f b -> f t

convert :: FBT b t -> (b -> t)
convert fbt = runIdentity
            . (fbt :: Identity b -> Identity t)
            . Identity
~~~

## Getting `s -> a`

To invert an iso, you have to be able to get `s -> a` and `b -> t` out of it. The only thing *we* can do is give the iso some box of form `p a (f b)`; to get `s -> a`, let's give it a box that memorises applications of `lmap`. I mean this:

               x-----------x
               |           |
               |___________|
               aaaaaa|
               |‾‾‾|a|‾‾‾‾‾|
               |   |aaaaaaaa
               x-----------x

(It doesn't have any official output, but it has its input flowing out of it via a hidden pipe.)

After `lmap` it would look like this:

               x-----------x
          _____|           |
         |  )  |___________|
         sss)aaaaaaaa|
         |  )  |‾‾‾|a|‾‾‾‾‾|
          ‾‾‾‾‾|   |aaaaaaaa
               x-----------x

So, we'll give this box to the iso, the iso would apply `s -> a` to it, and we would have an `s -> a` pipe in our disposal.

Here's the same thing but without pictures:

~~~ haskell
lmap :: (s -> a) -> p a (f b) -> p s (f t)
~~~

The result, `p s (f t)`, has to hold `s -> a` somewhere and can ignore `(f t)`. Our box, `Foo`, shall look as follows:

~~~ haskell
-- “s” = input
-- “a” = output via hidden pipe
-- “x” = official output (which can be anything 'cause there's no output)
data Foo a s x = Foo (s -> a)

unFoo :: Foo a s x -> (s -> a)
~~~

Replacing `p` with `Foo a` makes `lmap` look like this:

~~~ haskell
lmap :: (s -> a) -> Foo a a (f b) -> Foo a s (f t)
~~~

We can get `Foo a a (f b)` by wrapping `id` into `Foo`, and we can get `s -> a` out of `Foo a s (f t)` by using `unFoo`. Cool.

`Foo` is actually called [`Forget`][], by the way.

## Getting `b -> t`

To get `b -> t`, we can take a `b`, make a box that outputs `b`, and the iso would turn it into a box that outputs `t`. This wouldn't even require any hidden pipes:

               x-----------x
               |           |_____
               |___________|  )  |
                     |bbbbbbbb)ttt
               |‾‾‾‾‾‾‾‾‾‾‾|  )  |
               |           |‾‾‾‾‾
               x-----------x

~~~ haskell
-- “b” = output
-- “x” = official input (which can be anything 'cause there's no input)
data Bar x b = Bar b

unBar :: Bar x b -> b
~~~

~~~ haskell
rmap :: (f b -> f t) -> Bar x (f b) -> Bar x (f t)
~~~

The actual name of `Bar` is [`Tagged`][].

Note that here we have to know `b` in order to even *create* the box that would be given to `rmap`. So, to extract `b -> t` out of an `Iso`, we do this:

~~~ haskell
bt :: Iso s t a b -> (b -> t)
bt i = \b -> runIdentity . unTagged $ i (Tagged (Identity b))

-- In pseudocode:
--
-- bt i = \b -> unwrapBox (i (makeBox b))
~~~

(`Identity` is needed because `Iso` deals with `f b` and not `b`.)

We could just as well get `b -> t` directly using this box:

               x-----------x
               bbbbbbbb|   |_____
               |_____|b|___|  )  |
                     |bbbbbbbb)ttt
               |‾‾‾‾‾‾‾‾‾‾‾|  )  |
               |           |‾‾‾‾‾
               x-----------x

~~~ haskell
-- In pseudocode:
--
-- bt i = unwrapBox (i (makeBox id))
~~~

## Getting both things in one go

To get both things in one go, we can take this box (which is called [`Exchange`][] in lens):

~~~ haskell
data Exchange a b s t = Exchange (s -> a) (b -> t)
~~~

               x-----------x
               bbbbbbbb|   |
               |_____|b|___|
               aaaaaa|bbbbbb
               |‾‾‾|a|‾‾‾‾‾|
               |   |aaaaaaaa
               x-----------x

which the iso would turn into this box:

               x-----------x
          _____bbbbbbbb|   |_____
         |  )  |_____|b|___|  )  |
         sss)aaaaaaaa|bbbbbbbb)ttt
         |  )  |‾‾‾|a|‾‾‾‾‾|  )  |
          ‾‾‾‾‾|   |aaaaaaaa‾‾‾‾‾
               x-----------x

and from which we'd extract `s -> a` and `b -> t`:

          _____               x-----------x
         |  )  |_____         bbbbbbbb|   |_____
         sss)aaaaaaaa|        ‾‾‾‾‾‾|b|___|  )  | 
         |  )  |‾‾‾|a|‾‾‾‾‾|        |bbbbbbbb)ttt 
          ‾‾‾‾‾|   |aaaaaaaa         ‾‾‾‾‾|  )  | 
               x-----------x               ‾‾‾‾‾

## Exercises

Pictures are fun and all, but do the following things:

  * look up [`dimap`][] and understand what it does
  * write an instance of `Profunctor` for `->`
  * write `iso :: (s -> a) -> (b -> t) -> Iso s t a b`
  * write an instance of `Profunctor` for `Forget`
  * write an instance of `Profunctor` for `Tagged`
  * write an instance of `Profunctor` for `Exchange`
  * write `from :: Iso s t a b -> Iso b a t s` using `Exchange`

Okay, and now let's move on to prisms.

# Affine traversals

Or, well, let's not. I just found out that I had written a small section about affine traversals back when the plan for this post was:

  * “A wrong way to approach prisms (involving affine traversals)”
  * “Isomorphisms”
  * “The right way to approach prisms”

And then the post grew and grew and I decided to split off isomorphisms into their own part. Anyway, affine traversals are something that is *not* in lens (and might never be in lens), but this hasn't stopped me from writing about them, so here goes.

-----------------------------------------------------------------------------

An affine traversal (or a 0-or-1 traversal) is a traversal that always extracts either nothing or a single value. (By the way, traversals that always return 1 or more values are called relevant traversals, and lenses may be called linear traversals.) If you remember, in post #3 we modified a `Traversal` to get `Traversal1` (which always traversed 1 or more element). I'll remind you how it was done:

* `Traversal` has an `Applicative` constraint
* `Applicative` provides `pure` and `<*>`
* `pure` is like `mempty` (it lets you create actions which have no effects)
* `<*>` is like `<>` (it lets you combine effects from 2 actions)
* when `pure` is removed, the traversal can no longer do nothing
* to remove `pure`, we used the [`Apply`][] typeclass, which is just like `Applicative` but without `pure`

So, the “0” part comes from `pure`, the “+” part comes from `<*>`, and the “1” part (i.e. returning at least 1 value) comes from the `a -> f b` function we give to the traversal. If we remove `pure`, we can't have “0”. From this follows that if we remove `<*>`, we'll lose “+”, which is exactly what we want.

(No, really, is this clear to you? It might not be, because I only got it after thinking about it for 5m and I'm the one who is actually writing the posts (but on the other hand maybe I just have really bad memory), so... Okay, whatever. We give the traversal an `a -> f b` function (which actually just stores `a` in `f b`). The traversal applies this function to all the elements it wants to return. Then it combines the results with `<*>`, which also combines “effects” to return several stored `a`s instead of just 1. If the traversal doesn't want to return anything, it still can use `pure` to get an `f t` back. Now, if we don't let the traversal use `<*>`, it won't be able to combine those `f b`s, and so we'll get at most 1 `a`.)

I don't know how to google for “`pure` without `<*>`”, but I knew that there was such a class somewhere, and then I was just reading unrelated stuff and completely accidentally stumbled upon [`Pointed`][], which is just what we need. (But if I hadn't found it, I would've asked on #haskell or #haskell-lens, so it's not like this depended much on pure luck. I hate things that depend on pure luck.)

`Pointed` is a class with a single operation – `point`, which... well, lifts a value into something. It's not really useful for anything because for everyday programming `pure` and `return` are alright (not to mention that most people prefer being more explicit and writing e.g. `Just []` instead of `pure []`), and it's not suitable for writing more generic functions either because it's got no laws. (Here's a [typical jump-all-over-the-place-but-still-nice comment][why not Pointed] from Edward about `Pointed`.)

[why not Pointed]: https://wiki.haskell.org/Why_not_Pointed%3F

With `Pointed`, we can give this definition for `Traversal01`:

~~~ haskell
type Traversal01 s t a b =
  forall f. (Functor f, Pointed f) => (a -> f b) -> s -> f t
~~~

# Prisms

I like this description of prisms Edward Kmett wrote somewhere recently and I can't be bothered to google where exactly okay *fine* I will. [Here](https://github.com/sdiehl/wiwinwlh/issues/67#issue-73707312). (In case you're wondering, Tony is being sarcastic in the next comment. I think.)

> A lens describes something isomorphic to a product with some extra context. A lens from `s` to `a` indicates there exists `c` such that `s` is isomorphic to `(c, a)`.
>
> On the other hand, a prism from `s` to `a` indicates there exists `c` such that `s` is isomorphic to `(Either c a)`.

So basically lenses deconstruct [product types](@w:product type) and prisms deconstruct [sum types](@w:tagged union).

-----------------------------------------------------------------------------

Here's a longer explanation!

If you have a lens, you have `get` and `put`:

~~~ haskell
get :: s -> a
put :: s -> a -> s
~~~

You can combine these into 1 function:

~~~ haskell
lens :: s -> (a, a -> s)
~~~

`lens` breaks `s` into 2 values – `a` and something of type `a -> s`. Conceptually, you can say that `a -> s` is “`s` with an `a`-shaped hole” – it contains all information needed to construct `s`, apart from `a`. You can also combine `a` and `a -> s` to get `s` back. Unless you violate lens laws, `(a, a -> s)` is isomorphic to just `s`.

What about prisms? They're the same, but instead of deconstructing product types (e.g. tuples) they deconstruct sum types (i.e. something like `Either`). With a prism, you get these 2 operations:

~~~ haskell
get :: s -> Maybe a
put :: a -> s
~~~

This still doesn't sound clear to me, so I'll give a concrete example.

Let's say you have a type for integers – `Integer`. You can say that every integer is *either* a natural number, 0, or a negated natural number (assuming that naturals start from 1). So, while it's not actually represented like this, conceptually you could say that this is the definition of `Integer`:

~~~ haskell
data Integer = Positive Natural | Zero | Negative Natural
~~~

This is a sum type. You can't deconstruct it with tuples, but you can deconstruct it with `Either` (where `()` shall denote `Zero`):

~~~ haskell
type Integer = Either Natural (Either Natural ())
~~~

In other words, “not every integer contains a positive number, but some integers are positive numbers”.

Now, a prism is something that lets you *maybe* get a positive number out of an integer (if it's there), and it also lets you “embed” a positive number into an integer:

~~~ haskell
prism :: (Integer -> Maybe Natural, Natural -> Integer)
~~~

Of course, `Integer` doesn't have to be an actual sum type for that, we can just pretend it is and use some `if`s and stuff:

~~~ haskell
prism = (toNatural, toInteger)
  where
    toNatural n = if n > 0 then Just (fromInteger n) else Nothing
~~~

(Did you know that GHC now has a [type for naturals][`Natural`], by the way?)

(Also, have you noticed that `get :: s -> Maybe a` means that every prism is an affine traversal?)

(Also also, have you noticed that prisms are almost like isomorphisms? Except that isomorphisms always work in both directions, and prisms always work in one direction but can fail in the other direction – so, they could be called partial isomorphisms.)

## Simple and not-simple prisms

We have simple lenses (ones that don't change the type) and not-simple lenses (ones that can change the type). Can we have not-simple prisms?

A simple prism is something like this:

~~~ haskell
natural :: Prism' Integer Natural
~~~

It can be represented as a pair of functions:

~~~ haskell
getNatural :: Integer -> Maybe Natural
putNatural :: Natural -> Integer
~~~

The key to not-simple prisms is noticing the fact that with `get*` and `put*` you can write `modify*` (which would modify the integer if and only if it's a natural number):

~~~ haskell
modifyNatural :: (Natural -> Natural) -> Integer -> Integer
modifyNatural f int = case getNatural int of
  Nothing  -> int
  Just nat -> putNatural (f nat)
~~~

Thus, with a not-simple prism...

~~~ haskell
_Left :: Prism (Either a c) (Either b c) a b
~~~

you ought to be able to write this type-changing `modifyLeft`:

~~~ haskell
modifyLeft :: (a -> b) -> Either a c -> Either b c
~~~

Can we write it using a version of `get*` and `put*`? Look at the definition for `modifyNatural`, and notice where and how it would fail if it was rewritten for `_Left`. Have you noticed?

(This wasn't an exercise really, it was more like “hey, are you here? are you reading? do you know that if you can't answer this question, you probably won't understand later stuff, so maybe you should reread a bit or something?”.)

(On the other hand, you still should try to answer this question before reading further. Spend 2 minutes on it – by the clock – and if you still don't know, keep reading.)

The answer is that it would fail in the `Nothing` branch. We have `Either a c`; we know that it's actually just `c`. So, we *should* be able to return it as `Either b c`, but we can't, because the original value we have is still of type `Either a c`.

How to fix it? Well, we can add the type-changing behavior to our `get*` function:

* currently `get*` returns either “yep, here's the extracted value” or “couldn't extract anything, just take the old value since you have it already”

* a better `get*` would return either “yep, here's the extracted value” or “couldn't extract anything, which means that I can safely change the type to match what you want”

For `getLeft`, it means that it would return either `a` (if it could be extracted) or `Either b c` (if it couldn't), because if it couldn't it means that our `Either a c` was just `c` and so we can coerce it to *any* `Either x c` because, c'mon, it's just `c`, no, seriously— ahem.

~~~ haskell
getLeft :: Either a c -> Either (Either b c) a
putLeft :: b -> Either b c

modifyLeft :: (a -> b) -> Either a c -> Either b c
modifyLeft f ac = case getLeft ac of
  Left bc -> bc
  Right a -> putLeft (f a)
~~~

Yay. Now let's generalise everything!

A general prism:

~~~ haskell
type Prism s t a b = ...
~~~

A simple prism:

~~~ haskell
type Prism' s a = Prism s s a a
~~~

A function to make a general prism from a setter and a getter:

~~~ haskell
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
~~~

A function to make a simple prism from a setter and a getter:

~~~ haskell
prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
~~~

## Fitting prisms into the lens framework

Okay, now, we have this cool type template `... => (a -> f b) -> (s -> f t)` and we want to somehow fit prisms into this type so that they would combine with everything else, and the only thing we can actually choose is what constraints we put on `f`. If you have chosen the constraints right, you should be able to write [`review`][]:

~~~ haskell
review :: Prism' s a -> a -> s
~~~

as well as [`prism`][]:

~~~ haskell
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
~~~

and don't forget that [`^?`][] should work as well:

~~~ haskell
(^?) :: s -> Getting (First a) s a -> Maybe a
(^?) :: s -> Prism' s a -> Maybe a
~~~

Any ideas?

-----------------------------------------------------------------------------

Actually, this is impossible. Look at the type again:

~~~ haskell
type Prism s t a b =
  ... => (a -> f b) -> (s -> f t)
~~~

The whole point of `review` is to get us an `s`, but the prism produces a function that *takes* an `s`. Where'd we get it?

It's not enough to put constraints on `f`; we need to use profunctors. (Which makes sense, by the way – if we used profunctors for isomorphisms, and prisms are almost like isomorphisms, we'd need something like profunctors for prisms too.)

Okay, let's try drawing prisms with boxes.

~~~ haskell
type Prism s t a b =
  ... => p a (f b) -> p s (f t)
~~~

A prism takes an `a -> b` box, and turns it into an `s -> t` box. (Again, I'm ignoring `f` for now.) The `s -> t` box operates like this:

               x-----------x
        _______|           |_____ _______
       |       |_____)_____|  )  |       |
       sss///aaaaaaaa)bbbbbbbb)ttttt\\\ttt
       |  |t|  |‾‾‾‾‾)‾‾‾‾‾|  )  |  |t|  |
        ‾‾|t|‾‾|           |‾‾‾‾‾ ‾‾|t|‾‾
          |t|  x-----------x        |t|
          |t|_______________________|t|
          |ttttttttttttttttttttttttttt|
           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

It takes an `s` and looks at it. If the `s` is actually an `a`, great, this is the isomorphism case – `a` goes into the box, `b` gets out, `b` is turned into `t`, the end. If the `s` isn't an `a`, it's a `t` – and the `t` bypasses the box altogether and just becomes the output of the whole mechanism.

So, a prism adds 2 parts to the original `a -> b` box:

  * an output filter that turns `b` into `t`
  * a mechanism that lets us bypass the box

We already know how to do the 1st thing – just use `rmap`. The 2nd thing is new; we could write it like this:

~~~ haskell
class Bypass p where
  bypass :: (s -> Either a t) -> p a t -> p s t
~~~

This would give us the following constraints on `Prism`:

~~~ haskell
type Prism s t a b =
  (Profunctor p, Bypass p, Applicative f) => p a (f b) -> p s (f t)
~~~

(Why `Applicative` and not `Functor`? Because we'd need to be able to turn `t` into `f t` in case we do the bypass, and `Applicative` provides `pure`.)

-----------------------------------------------------------------------------

This lets us write prisms already (an exercise: write the instance of `Bypass` for `Tagged`). However, there's a way to do it with a more elegant class than `Bypass`. I won't try to guess how I could've thought of it independently, I'll just tell you about it and we'll move on.

The class is called [`Choice`][]:

~~~ haskell
class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)
~~~

(It's called `right'` because [`right`][] is already taken by a similar method in [`ArrowChoice`][].)

An exercise: implement `left'` using `right'` (thus showing that it's enough to have only 1 method out of 2).

The difference between `right'` and `bypass` is that `bypass` requires us to provide a splitting function, while `right'` just assumes that the input is already split.

This is the result of `bypass`:

                       x-----------x
                _______|           |_______
               |       |_____)_____|       |
               sss///aaaaaaaa)bbbbbbbb\\\bbb
               |  |b|  |‾‾‾‾‾)‾‾‾‾‾|  |b|  |
                ‾‾|b|‾‾|           |‾‾|b|‾‾
                  |b|  x-----------x  |b|
                  |b|_________________|b|
                  |bbbbbbbbbbbbbbbbbbbbb|
                   ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

This is the result of `right'`:

                       x-----------x
                _______|           |_______
               |       |_____)_____|       |
       Either c a ///aaaaaaaa)bbbbbbbb\\\ Either c b
               |  |c|  |‾‾‾‾‾)‾‾‾‾‾|  |c|  |
                ‾‾|c|‾‾|           |‾‾|c|‾‾
                  |c|  x-----------x  |c|
                  |c|_________________|c|
                  |ccccccccccccccccccccc|
                   ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

An exercise: implement `instance Choice p => Bypass p` (don't forget that you can use `Profunctor` methods too).

Hm, isn't that too much exercises— nah, it's not. Another exercise (a more complicated one): implement `instance (Profunctor p, Bypass p) => Choice p`.

## Basic operations on prisms

`review` lets us convert `b` to `t` using a prism, and it's the same as for isomorphisms:

~~~ haskell
type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

review :: AReview t b -> b -> t
review r = runIdentity . unTagged . r . Tagged . Identity
~~~

`prism` lets us construct a prism from 2 functions; it's trivial with `bypass` and slightly less trivial with `right'`. You should try implementing it by yourself before looking at this definition:

~~~ haskell
-- This uses dimap instead of lmap+rmap because why not (and also because
-- that's what is used in lens):
--
--     dimap f g = lmap f . rmap g

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
~~~

## `Market`

As with isomorphisms, we'd like to be able to decompose a prism back into functions from which that prism was created. We used [`Exchange`][] with isomorphisms:

~~~ haskell
data Exchange a b s t = Exchange (s -> a) (b -> t)
~~~

The corresponding type for prisms is called [`Market`][]:

~~~ haskell
data Market a b s t = Market (b -> t) (s -> Either t a)
~~~

Writing instances of `Functor`, `Profunctor`, and `Choice` for `Market` is boring and doesn't require almost any thinking (just follow the types), so you can copy them from here:

~~~ haskell
instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)

instance Profunctor (Market a b) where
  lmap f (Market bt seta) = Market bt (seta . f)
  rmap f (Market bt seta) = fmap f (Market bt seta)

instance Choice (Market a b) where
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
~~~

Decomposing the prism actually doesn't require any thinking either, especially if you use holes (`_`) liberally.

~~~ haskell
unPrism :: Prism s t a b -> (b -> t, s -> Either t a)
unPrism p =
  let -- bft   :: b -> Identity t
      -- setfa :: s -> Either (Identity t) a
      Market bft setfa = p (Market Identity Right)
      -- bt    :: b -> t
      -- seta  :: s -> Either t a
      bt = runIdentity . bft
      seta = either (Left . runIdentity) Right . setfa
  in (bt, seta)
~~~

Instead of `unPrism`, lens defines [`withPrism`][], which is almost the same thing:

~~~ haskell
withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r 
~~~

It also uses [`APrism`][] instead of `Prism` to require as little polymorphism as possible:

~~~ haskell
type APrism s t a b = Market a b a (Identity b) -> Market a b s (Identity t)
~~~

## Coercions everywhere

If you look at the definition of `withPrism` in lens, you'll see that starting from GHC 7.8 it uses a much simpler definition:

~~~ haskell
withPrism p f = case coerce (p (Market Identity Right)) of
  Market bt seta -> f bt seta
~~~

[`coerce`][] is a function that lets you convert between things that have the same memory representation:

~~~ haskell
coerce :: Identity a -> a
coerce :: [Identity a] -> [a]
coerce :: [Identity (Maybe a)] -> Identity [Maybe (Identity a)]
...
~~~

Generally, if you have 2 types that only differ in newtypes (`Identity`, `Tagged`, `Const`, etc), `coerce` would always be able to convert one to another. In case of `withPrism`, `coerce` removes 2 applications of `Identity`.

Now look at `review`:

~~~ haskell
review r = runIdentity . unTagged . r . Tagged . Identity
~~~

It does nothing but wrap and unwrap newtypes. We can replace it *all* with `coerce`:

~~~ haskell
review = coerce
~~~

In lens, however, it's implemented like this:

~~~ haskell
review r = runIdentity #. unTagged #. r .# Tagged .# Identity
~~~

Here [`#.`][] and [`.#`][] are operators that do the same thing as `lmap` and
`rmap` (you could think of them both as of `.`) but require one argument to be something like `Tagged` or `runIdentity`. So, the whole chain still gets interpreted as a coercion (and thus it's fast), but it shows the steps more clearly.

## Some prisms

Okay, back to prisms. The simplest ones are [`_Left`][] and [`_Right`][]:

~~~ haskell
_Left :: Prism (Either a c) (Either b c) a b
_Right :: Prism (Either c a) (Either c b) a b 
~~~

Unlike with lenses, there's no manual way to write a prism (apart from using `dimap` and so on), so you'll have to use `prism`:

~~~ haskell
_Left  = prism Left  (either Right (Left . Right))
_Right = prism Right (either (Left . Left) Right)
~~~

You can use prisms as traversals:

~~~ {.haskell .repl}
> Left 0 ^? _Left
Just 0

> Right 1 ^? _Left
Nothing
~~~

You can use prisms as constructors, too:

~~~ {.haskell .repl}
> review _Left 0
Left 0
~~~

[`#`][] is another name for `review`:

~~~ {.haskell .repl}
> _Left # 0
Left 0
~~~

Prisms don't necessarily have to be constructors – e.g. look at [`_Show`][]:

~~~ haskell
_Show :: (Read a, Show a) => Prism' String a
~~~

~~~ {.haskell .repl}
> _Show # 0
"0"

> "EQ" ^? _Show :: Maybe Ordering
Just EQ
~~~

Or [`hex`][] (from [`Numeric.Lens`][]):

~~~ haskell
hex :: Integral a => Prism' String a 
~~~

~~~ {.haskell .repl}
> "ad32" ^? hex
Just 44338

> hex # 5710
"164e"

> 5710 ^. re hex
"164e"
~~~

[`re`][] makes a getter out of a prism:

~~~ haskell
re :: Prism s t a b -> Getter b t
re p = to (p #)
~~~

## Prisms as smart constructors

Prisms are quite useful as smart constructors, because you can compose them and the resulting thing would still be usable both as a constructor and as a deconstructor/pattern:

~~~ haskell
-- duplication!

makeL3 = Left . Left . Left

getL3 (Left (Left (Left x))) = Just x
getL3 _                      = Nothing

-- better

_L3 = _Left . _Left . _Left
~~~

This is particularly useful when you're working with e.g. JSON or Template Haskell and want to create “aliases” for long chains of patterns.

Another nice thing about prisms is that you can check them with [`has`][], but you can't do the same with constructors:

~~~ {.haskell .repl}
> has _Left (Right 0)
False

-- Without prisms, we have to use a helper that somebody else wrote:
> isLeft (Right 0)
False
~~~

(You can even use [`is`][] instead of `has` if you export [`Control.Lens.Extras`][].)

Here are some places with more prisms:

  * [`Data.List.Lens`][] provides `prefixed` and `suffixed`
  * [`Numeric.Lens`][] provides `integral` and `base`/`hex`/`octal`/etc
  * [`Control.Exception.Lens`][], [`System.Exit.Lens`][], [`System.IO.Error.Lens`][] are for exceptions and error codes
  * [`Data.Dynamic.Lens`][] has `_Dynamic`
  * [`GHC.Generics.Lens`][] has `_V1`, `_U1`, etc
  * [`Language.Haskell.TH.Lens`][] has prisms for all Template Haskell data types
  * [`Data.Aeson.Lens`][] has prisms (and lenses) for Aeson types

## Half-simple prisms

Remember how I said that you can build simple prisms with `prism'`?

~~~ haskell
prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
~~~

Well, the actual type in lens is slightly more general:

~~~ haskell
prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
~~~

There are several prisms in lens that are “half-simple” – for instance, [`integral`][] and [`_Void`][]:

~~~ haskell
integral :: (Integral a, Integral b) => Prism Integer Integer a b 

_Void :: Prism s s a Void
~~~

And now, instead of explanations – exercises:

  * What can you do with lens's `integral` that you can't do with this one?

    ~~~ haskell
    integral :: Integral a => Prism' Integer a
    ~~~

  * Reflect on the nature of `_Void`.

## `Prism` utilities

lens defines some utilities for prisms, so let's look at them and figure out what they are for. (We won't bother implementing them, because they all are pretty easy to get with `withPrism`.)

### [`aside`][], [`without`][]

~~~ haskell
aside :: Prism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
~~~

~~~ haskell
without :: Prism s t a b -> Prism u v c d
        -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
~~~

It should be obvious from the signatures what these do.

To be honest, I don't know when they are useful. 

### [`below`][]

~~~ haskell
below :: Traversable f => Prism' s a -> Prism' (f s) (f a)
~~~

`below` lets you define patterns like this:

~~~ haskell
-- pseudocode

f [Right a, Right b, ..., Right x] = do something with [a,b,...,x]
f _                                = do whatever else
~~~

In other words, `below p` checks that all elements in some structure match the prism `p`, and if they do, it strips `p` from those elements:

~~~ {.haskell .repl}
> [Right 'a', Right 'b', Right 'c'] ^? below _Right
Just "abc"

> [Right 'a', Right 'b', Left 'c'] ^? below _Right
Nothing
~~~

The structure can be anything `Traversable` (so it doesn't have to be a list).

### [`outside`][]

~~~ haskell
outside :: Prism s t a b -> Lens (t -> r) (s -> r) (b -> r) (a -> r)
~~~

The documentation for `outside` is a bit mysterious:

> Use a `Prism` as a kind of first-class pattern.

To figure it out, let's apply `outside` to some prism and look at the resulting type:

~~~ haskell
outside _Right
  :: Lens (Either c b -> r) (Either c a -> r) (b -> r) (a -> r)
~~~

And let's restrict the type to `Lens'`:

~~~ haskell
outside _Right :: Lens' (Either c a -> r) (a -> r)
~~~

Okay, it's clearer now. Given some function that works on `Either`, we can:

  * find out what it does with the right part of `Either`
  * make it do something *else* with the right part of `Either`

Let's define `fromLeft`:

~~~ haskell
fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _        = error "right"
~~~

What does `fromLeft` do with a `Left`?

~~~ {.haskell .repl}
> (fromLeft ^. outside _Left) 0     -- equivalent to “fromLeft (Left 0)”
0
~~~

And a `Right`?

~~~ {.haskell .repl}
> (fromLeft ^. outside _Right) 0    -- equivalent to “fromLeft (Right 0)”
*** Exception: hi
~~~

Okay, that's not terribly interesting. The modifying part is better:

~~~ {.haskell .repl}
> let fromLeftDef def = fromLeft & outside _Right .~ const def

> fromLeftDef 0 (Left 5)
5

> fromLeftDef 0 (Right 5)
0
~~~

We've already seen similar function-modifying behavior with `ix`:

~~~ {.haskell .repl}
> let safeDiv x = (x `div`) & at 0 .~ 0

> safeDiv 6 2
3

> safeDiv 6 0
0
~~~

The difference between `ix` and `outside` is that you can only use `ix` with arguments that can be compared with `==`, while `outside` is more like pattern-matching. (An exercise: look at [`only`][] and figure out how to use it with `outside` to modify a function's value at some point.)

## Final exercise

We made `Traversal1` out of `Traversal` by switching `Applicative` with `Apply`. Similarly we could try to get `Prism1`:

~~~ haskell
type Prism1 s t a b =
  forall p f. (Choice p, Apply f) => p a (f b) -> p s (f t)
~~~

Since `Prism1` can return at least one `a` and *at most* one `a`, it's the same as `Iso`. (Remember: prisms are 0-or-1 traversals, so `Prism1` is a 1-or-1 traversal, so it's a getter in one direction and a getter in another direction, so it's simply `s -> a` plus `b -> t`, so it's the same as `Iso`.)

(Actually, since it won't ever return more than 1 element, we can even replace `Apply` with `Functor`.)

Now, getting `b -> t` out of `Prism1` is easy and we've done it before:

~~~ haskell
prism1ToIso :: Prism1 s t a b -> Iso s t a b
prism1ToIso p =
  let bt = runIdentity . unTagged . p . Tagged . Identity
      sa = ...
  in  iso sa bt
~~~

Your task is to get `s -> a` and complete `prism1ToIso`.

# Answers to some exercises

> convert between `Bypass` and `Choice`

~~~ haskell
instance Choice p => Bypass p where
  bypass sat = rmap (either id id) . lmap sat . left'

instance (Profunctor p, Bypass p) => Choice p where
  right' = bypass (either (Right . Left) Left) . rmap Right
~~~

-----------------------------------------------------------------------------

> What can you do with lens's `integral` that you can't do with this one?
>
> ~~~ haskell
> integral :: Integral a => Prism' Integer a
> ~~~

With this `integral` you wouldn't be able to use a type-changing function internally. For instance, imagine that you have `f :: Word -> Int`, and you want to convert the `Integer` to `Word`, apply `f`, and convert the `Int` back to `Integer`. You can do it with original `integral` like this:

~~~ {.haskell .repl}
> n & integral %~ f
~~~

but not with a type-restricted `integral`.

-----------------------------------------------------------------------------

> Reflect on the nature of `_Void`.

~~~ haskell
_Void :: forall a. Prism s s a Void
~~~

`_Void` shows that `forall a. a` is equivalent to `Void` (that is, both are uninhabited types, and don't nitpick about `undefined`.)

Well, it probably shows more things than that, but whatever.


[`#.`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor-Unsafe.html#v:-35-.
[`#`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Review.html#v:-35-
[`.#`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor-Unsafe.html#v:.-35-
[`APrism`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#t:APrism
[`Apply`]: http://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html#t:Apply
[`ArrowChoice`]: http://hackage.haskell.org/package/base/docs/Control-Arrow.html#t:ArrowChoice
[`Choice`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#t:Choice
[`Control.Exception.Lens`]: http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html
[`Control.Lens.Extras`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Extras.html
[`Data.Aeson.Lens`]: http://hackage.haskell.org/package/lens-aeson/docs/Data-Aeson-Lens.html
[`Data.Dynamic.Lens`]: http://hackage.haskell.org/package/lens/docs/Data-Dynamic-Lens.html
[`Data.List.Lens`]: http://hackage.haskell.org/package/lens/docs/Data-List-Lens.html
[`Exchange`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Internal-Iso.html#t:Exchange
[`Forget`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#t:Forget
[`GHC.Generics.Lens`]: http://hackage.haskell.org/package/lens/docs/GHC-Generics-Lens.html
[`Language.Haskell.TH.Lens`]: http://hackage.haskell.org/package/lens/docs/Language-Haskell-TH-Lens.html
[`Market`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Internal-Prism.html#t:Market
[`Natural`]: http://hackage.haskell.org/package/base/docs/Numeric-Natural.html#t:Natural
[`Numeric.Lens`]: http://hackage.haskell.org/package/lens/docs/Numeric-Lens.html
[`Pointed`]: http://hackage.haskell.org/package/pointed/docs/Data-Pointed.html#t:Pointed
[`System.Exit.Lens`]: http://hackage.haskell.org/package/lens/docs/System-Exit-Lens.html
[`System.IO.Error.Lens`]: http://hackage.haskell.org/package/lens/docs/System-IO-Error-Lens.html
[`Tagged`]: http://hackage.haskell.org/package/tagged/docs/Data-Tagged.html#t:Tagged
[`^?`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:-94--63-
[`_Left`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:_Left
[`_Right`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:_Right
[`_Show`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:_Show
[`_Void`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:_Void
[`aside`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:aside
[`below`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:below
[`coerce`]: http://hackage.haskell.org/package/base/docs/Data-Coerce.html#v:coerce
[`dimap`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#v:dimap
[`has`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:has
[`hex`]: http://hackage.haskell.org/package/lens/docs/Numeric-Lens.html#v:hex
[`integral`]: http://hackage.haskell.org/package/lens/docs/Numeric-Lens.html#v:integral
[`is`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Extras.html#v:is
[`only`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:only
[`outside`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:outside
[`prism`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:prism
[`re`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Review.html#v:re
[`review`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Review.html#v:review
[`right`]: http://hackage.haskell.org/package/base/docs/Control-Arrow.html#v:right
[`withPrism`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:withPrism
[`without`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#v:without

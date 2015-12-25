% lens over tea #4: isomorphisms, some profunctors, lens families

---
series:
  top: “lens over tea”
  toplink: /#lens-over-tea
  prev: /lens-over-tea-3
  next: /lens-over-tea-5
---

...as well as human sacrifice, dogs and cats living together, mass hysteria— ahem. There'll be prisms in the next post, but first we'll have to understand isomorphisms, because isomorphisms are easier and prisms kinda follow from isomorphisms.

Also:

  * I'm tempted to ditch prisms and spend several days figuring out pure profunctor lenses instead (yes I'll explain what it means in this very post). Apparently currently knowledge of profunctor lenses is being passed orally on #haskell-lens and nowhere else (not counting [this post][profunctor lenses] which isn't much more than a bunch of functions and types), and I hate the fact that there exist things which I can't learn without having to talk to someone else.

  * Thanks to ion on #haskell-lens for explaining things and making it click for me (primarily by showing me [`Tagged`][] and explaining what [`Choice`][] is about). (This doesn't contradict the previous point – I'm still annoyed that to figure out prisms I had to talk to someone instead of, say, just consulting lens docs.)

  * I wonder how much upvotes will this part get, because on one hand the trend is rather bad ([62][lot 1]–[25][lot 2]–[18][lot 3]), but on the other hand this one has a more interesting-sounding title (I think?), so... we'll see. Yeah, I'm a sucker for upvotes.

  * Here's a [cat video][] for you (watch with sound).

[lot 1]: https://reddit.com/r/haskell/comments/2igp1v/lens_over_tea_part_1_lenses_101_traversals_101
[lot 2]: https://reddit.com/r/haskell/comments/2zg3fm/lens_over_tea_2_composition_laws
[lot 3]: https://reddit.com/r/haskell/comments/366pb3/lens_over_tea_3_folds
[profunctor lenses]: http://r6research.livejournal.com/27476.html
[cat video]: /cat.webm

# Lens families

There's a thing about polymorphic lenses which I haven't told you before because I didn't know about it.

Here's a polymorphic lens:

~~~ haskell
type Lens s t a b
~~~

We know the following about such lenses:

* `a` is a part of `s`
* `b` is a part of `t`
* when you replace `a` in `s` with `b`, its type changes to `t`

(Just in case: “`a` is a part of `s`” doesn't necessarily mean that `s` looks like `Maybe a` or something else of the shape `g a`. It can perfectly be `(a, b)`, for instance, which can't be represented as a `g a` because Haskell lacks type-level lambdas (for the same reason you wouldn't be able to e.g. [make `(a, a)` a functor][tuple functor]). Moreover, there are lenses like [`united`][] (of type `Lens' s ()`), which work for any `s`, no matter whether `()` is contained in `s` as a type parameter or not. So, “`a` is a part of `s`” should be understood as something conceptual and not literal.)

[tuple functor]: http://stackoverflow.com/questions/4812633/making-a-a-a-functor

This intuitive understanding of polymorphic lenses is kinda formalised in [this post][mirrored lenses]:

[mirrored lenses]: http://comonad.com/reader/2012/mirrored-lenses/

> So, why do I use the term “lens family” rather than “polymorphic lens”?
>
> In order for the lens laws to hold, the 4 types parameterizing our lens family must be interrelated.
>
> In particular you need to be able to put back (with `.~`) what you get out of the lens (with `^.`) and put multiple times.
>
> This effectively constrains the space of possible legal lens families to those where there exists an index kind `i`, and two type families `outer :: i -> *`, and `inner :: i -> *`. If this were a viable type signature, then each lens family would actually have 2 parameters, yielding something like:
>
> ~~~ haskell
> -- pseudo-Haskell
> type LensFamily outer inner =
>   forall a b. Lens (outer a) (outer b) (inner a) (inner b)
> ~~~

This `forall a b` there implies that you should be able to swap pairs of types and nothing should change. See for yourself:

  * ~~~ haskell
    _1 :: Lens (a, x) (b, x) a b
    _1 :: Lens (b, x) (a, x) b a
    ~~~

  * ~~~ haskell
    _Left :: Prism (Either a c) (Either b c) a b
    _Left :: Prism (Either b c) (Either a c) b a
    ~~~

  * ~~~ haskell
    traversed :: Traversable f => Traversal (f a) (f b) a b
    traversed :: Traversable f => Traversal (f b) (f a) b a
    ~~~

In each pair, both types are *completely the same* because the only thing that's different is variable names, and renaming a type variable doesn't change the type.

If you have a weird lens like this:

~~~ haskell
Lens [Int] [Bool] (Maybe Char) String
~~~

it'll never satisfy any lens laws, because they won't even typecheck (like, “you get what you put in”, yeah, except that you get `Maybe Char` and you put in `String` and they don't even *have* equality defined on them).

I'm not sure whether such lenses can ever be useful – the inner/outer law only restricts your setter, not your getter, so you'd only need to throw it away if you needed a weirder setter— I'll just make up an example, y'know.

Let's say you have strict `Text` and you want to simultaneously convert it to lazy `Text` and modify some characters:

~~~ haskell
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy

strictToLazy :: Traversal Strict.Text Lazy.Text Char Char
~~~

It looks like “simultaneously convert and modify” is this traversal's primary purpose. Why would you want to use it as a getter, then? You would use `each` as a generic getter/setter, and you would only use `strictToLazy` when you actually want to do this convert-and-modify thing, and then `strictToLazy` can be a setter instead.

But actually, I don't know. Let's move on to isomorphisms.

# `Iso`

(Isos are defined in [`Control.Lens.Iso`][].)

“Iso” is a shortening of “isomorphism”; an `Iso' s a`:

* is a lens which lets you access `a` in `s`
* is a lens which lets you access `s` in `a` (when inverted)
* is isomorphic to `(s -> a, a -> s)`

So, if you have an `Iso' s a`, it means that you can convert between `s` and `a` without losing any information.

An `Iso s t a b` is a generalisation that also lets you change the types:

~~~ haskell
Iso s t a b ~ (s -> a, b -> t)

over someIso        :: (a -> b) -> (s -> t)
over (from someIso) :: (t -> s) -> (b -> a)
view someIso        :: s -> a
view (from someIso) :: b -> t
~~~

And if you assume that the iso follows the inner/outer law and types can be safely swapped:

~~~ haskell
over someIso        :: (a -> b) -> (s -> t)
over (from someIso) :: (s -> t) -> (a -> b)
view someIso        :: s -> a
view (from someIso) :: a -> s
~~~

Now, as an example, let's take [`enum`][]. Previously you could've said it was a lens to “access” the value corresponding to a number:

~~~ {.haskell .repl}
> 88 ^. enum :: Char
'X'

> 88 & enum %~ toLower
120

> fromEnum 'x'
120
~~~

However, you can also use it to convert things in the other direction:

~~~ {.haskell .repl}
> 0 & enum .~ 'x'
120
~~~

We were able to do it because we could create 0 out of thin air. In fact, we could even use `undefined` and `enum` wouldn't care:

~~~ {.haskell .repl}
> undefined & enum .~ 'x'
120
~~~

`Iso` generalises this notion of *lenses that don't care*.

## An obvious approach

If we want isos to work in both directions, we just need some type of “bidirectional function” which could work in either direction. Then constructing and inverting isos would be trivial:

~~~ haskell
type Iso s t a b =
  forall f. Functor f => (a -> f b) <-> (s -> f t)
~~~

In Haskell, when you want something to be several things at once, you define a class (`1` can be both `Int` and `Double` thanks to `Num`, [`empty`][] can be both `[]` and `Nothing` thanks to [`Alternative`][], etc). So, let's create a class for bidirectional functions:

~~~ haskell
class Isomorphic k where
  isomorphic :: (a -> b) -> (b -> a) -> k a b
~~~

Now, if `->` is an instance of `Isomorphic`, then functions created with `isomorphic` would be usable as ordinary functions:

~~~ haskell
instance Isomorphic (->) where
  -- we just don't need the other direction
  isomorphic f _ = f
~~~

You might think that now we would write an instance for `<-` (well, `<-` doesn't exist, so we'd have to write an instance for [`Op`][] really, but still). But we won't – there's really no reason to bother with `<-` when we can just store *both* directions:

~~~ haskell
data Isomorphism a b = Isomorphism (a -> b) (b -> a)

instance Isomorphic Isomorphism where
  isomorphic = Isomorphism
~~~

With `Isomorphism`, it's much easier to write a function that would reverse an iso:

~~~ haskell
-- from :: Isomorphism a b -> Isomorphism b a
-- from :: Isomorphism a b -> (b -> a)
from :: Isomorphic k => Isomorphism a b -> k b a
from (Isomorphism a b) = isomorphic b a
~~~

Finally, we make `Iso` itself a bidirectional function:

~~~ haskell
type Iso s t a b =
  forall k f. (Isomorphic k, Functor f) =>
  k (a -> f b) (s -> f t)
~~~

And you can create an iso from functions the easy way – just make 2 lenses going in opposite directions:

~~~ haskell
isos :: (s -> a) -> (a -> s)      -- s <-> a
     -> (t -> b) -> (b -> t)      -- t <-> b
     -> Iso s t a b
isos sa as tb bt = isomorphic
  (\afb s -> bt <$> afb (sa s))   -- easy peasy
  (\sft a -> tb <$> sft (as a))   -- lemon squeezy
~~~

That's all. Since `Iso` uses the `Isomorphic` class, it would return a function when we apply `^.` to it, and it would return a nice pair of functions when we want to invert it with `from`. This is [exactly how things were done][iso 3.6] in lens 3.6. You have just learned a piece of ancient Haskell!

[iso 3.6]: http://hackage.haskell.org/package/lens-3.6/docs/Control-Lens-Iso.html

This approach has a flaw, however: if you compose isos with `.`, they'll turn into ordinary functions and the result will be an ordinary function as well (while it *could* still be an iso). We can preserve both directions by writing an instance of [`Category`][] for `Isomorphism` and using the `.` from `Control.Category`, but then we could just use it for lenses themselves and forget about all the pains we took to make them composable with ordinary `.`.

(It also has another flaw: you need to give `isos` 4 functions, but you could do with just 2. Ugh, inelegant.)

## A better approach

If we want to do better, we can't really do anything with `->` between `(a -> f b)` and `(s -> f t)` – it'll be lost when we try to compose isos. We also won't achieve anything by placing constraints on `f`. What to do, what to do?

Well, there's only 1 thing left to meddle with – `->`s in parens. Let's meddle!

-----------------------------------------------------------------------------

> In fact, we could even use `undefined` and `enum` wouldn't care:
>
> ~~~ {.haskell .repl}
> > undefined & enum .~ 'x'
> 120
> ~~~
> 
> `Iso` generalises this notion of *lenses that don't care*.

First I want to explain why exactly it doesn't care. As you know, a lens is isomorphic to this:

~~~ haskell
Lens s t a b ~ (s -> a, s -> b -> t)
~~~

Or:

~~~ haskell
Lens s t a b ~ (s -> a, (s, b) -> t)
~~~

We can be even more precise if you remember the “hole in the type” approach – a lens decomposes `s` into `(s/a, a)` (except that there's no `/` in Haskell, but whatever) (I wrote `s/a` instead of `s−a` because compound types are denoted with “×”, and “+” is for sum types like `Either` – if you want to know more about algebra of types, I recommend [this post][type algebra]):

~~~ haskell
Lens s t a b ~ (s -> a, s/a×b -> t)
~~~

[type algebra]: http://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/

However, if we assume that `a` is isomorphic to `s`, there's *nothing left* for `s/a`, and it's reduced to `1` (or `()` in Haskell):

~~~ haskell
Iso s t a b ~ (s -> a, s/a×b -> t)
Iso s t a b ~ (s -> a,   1×b -> t)
Iso s t a b ~ (s -> a,     b -> t)
~~~

In other words, when you take a setter – `s -> b -> t` – -the part of `s` that it has to look at to produce the result- equals to `()`. Or in other other words, it doesn't have to look at `s` at all – which is why we can set it to `undefined` and nothing would happen (and which is why the reverse holds as well, and lenses which can be fed `undefined` are isomorphisms).

-----------------------------------------------------------------------------

To be able to write safe isomorphisms, we need some way to forbid them to look at `s` (more precisely: we have `(a -> f b) -> (s -> f t)`, and the resulting `s -> f t` function must be unable to look at `s` – if we can't do that, we can't ever be sure that it *won't* look).

There's a pretty standard way to forbid anyone from doing anything in Haskell – just make a class and now *you* decide what it can and can't do. So, if we want to be able to forbid functions from inspecting their inputs, we can simply make a class for that:

~~~ haskell
class F p where
  ...

type Iso s t a b =
  forall p f. (Functor f, F p) =>
  p a (f b) -> p s (f t)
~~~

But what should we allow? Well, let's look at some iso (`enum` again, actually) to figure it out. Previously we would've written it like this:

~~~ haskell
enum :: Enum a => Lens' Int a
enum afb = \s -> fromEnum <$> afb (toEnum s)
~~~

However, this won't do if we want to make `enum` an iso – we're looking at `s` here, and we're not allowed to. *However*, what we know is that if we can't look at `s`, `afb` can't care about its input either – it must be a constant function. Basically, we feed `enum` a constant function, and want a constant function in return.

There's a type for constant functions, called [`Tagged`][] (in the [tagged](@hackage) package):

~~~ haskell
newtype Tagged a b = Tagged {unTagged :: b}

instance Functor (Tagged a) where
  fmap f (Tagged b) = Tagged (f b)

...
~~~

> A `Tagged a b` value is a value `b` with an attached phantom type `a`. This can be used in place of the more traditional but less safe idiom of passing in an undefined value with the type, because unlike an `(a -> b)`, a `Tagged a b` can't try to use the argument `a` as a real value.

(Well, actually it's useful for more things than constant functions – I guess – but whatever.)

Now we can write an `enum` for `Tagged`:

~~~ haskell
enum :: Enum a => Tagged a a -> Tagged Int Int
enum (Tagged a) = Tagged (fromEnum a)
~~~

Or like this:

~~~ haskell
enum a = retag (fromEnum <$> a)
~~~

[`retag`][] here is needed to change `Tagged a Int` to `Tagged Int Int`.

So, we already know 2 things that we should be able to do and which don't require us to look at the value which might not even exist:

* `p a (f b) -> p a (f t)`, to be able to apply `fromEnum` to the result – we might even change it to `p a b -> p a t` because then we can just apply `fmap fromEnum` instead.

* `p a (f b) -> p s (f b)`, to be able to `retag` – but we can't do it if `p a (f b)` is *not* a constant function! The solution is to make something like `(s -> a) -> p a (f b) -> p s (f b)`, because then it can work for both ordinary functions and constant functions.

These 2 things – put together – already let us get from `p a (f b)` to `p s (f t)`, and it means that nothing else is needed:

~~~ haskell
class F p where
  changeInput  :: (s -> a) -> p a b -> p s b
  changeOutput :: (b -> t) -> p a b -> p a t

instance F (->) where
  changeInput sa ab = \s -> ab (sa s)
  changeOutput bt ab = \a -> bt (ab a)

instance F Tagged where
  changeInput _ ab = retag ab
  changeOutput bt b = bt <$> b
~~~

Now we can define `enum`:

~~~ haskell
enum = changeInput toEnum . changeOutput (fmap fromEnum)
~~~

And a generic `iso` function which would create an iso:

~~~ haskell
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = changeInput sa . changeOutput (fmap bt)
~~~

And since we know how to extract both `s -> a` and `b -> t` from an iso, we can define `from` (which inverts an iso):

~~~ haskell
-- e.g. from :: Enum a => Iso' Int a -> Iso' a Int
from :: Iso s t a b -> Iso b a t s
from i = iso bt sa
  where
    -- This uses the (->) instance:
    --   Сonst   :: a -> Const a b   or  (->) a (Const a b)
    --   i Const :: s -> Const a t   or  (->) s (Const a t)
    sa s = getConst ((i Const) s)
                                       
    -- This uses the Tagged instance:
    --   Tagged (Identity b)     :: Tagged a (Identity b)
    --   i (Tagged (Identity b)) :: Tagged s (Identity t)
    bt b = runIdentity . unTagged $ i (Tagged (Identity b))
~~~

# Profunctors and pure profunctor lenses

Now guess what? The `F` class is actually called [`Profunctor`][], and its methods `changeInput` and `changeOutput` are actually called [`lmap`][] and [`rmap`][], and there's also a [`dimap`][] method which combines `lmap` and `rmap` and which I'm going to use from now on:

~~~ haskell
lmap :: Profunctor p => (a -> b) -> p b c -> p a c
rmap :: Profunctor p => (b -> c) -> p a b -> p a c

dimap :: Profunctor p => (a -> b) -> (c -> d) -> p b c -> p a d
~~~

The point of profunctors is that if you're given a `p a b`, you can treat it as an opaque “black box”, some kind of relationship between `a` and `b` – you can add a filter to the black box which would modify its output, and you can add another filter which would modify its input, but you can't modify the black box itself in any way and you can't inspect the input in any way (because, after all, there might not even be any) or get any information from one filter to another (this bit might not be clear, but it'll be clear when I explain [`Choice`][] in the next post).

If you want examples of profunctors in the wild, [this 24 Days Of Hackage post][24 profunctors] and [this School Of Haskell post][SOH profunctors] give some – but (as with pretty much all abstractions!) what's useful about profunctors isn't that you can use `dimap` to operate on something that happens to be a profunctor, but that you can write functions which work on several profunctors. For instance, this is what lets functions in lens operate on both ordinary and indexed traversals— okay, okay, maybe there'll be indexed traversals in the post after the next post.

[24 profunctors]: https://ocharles.org.uk/blog/guest-posts/2013-12-22-24-days-of-hackage-profunctors.html
[SOH profunctors]: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors

-----------------------------------------------------------------------------

You could've noticed a bit of asymmetry in the previous section – we used `Tagged a b` as a function which ignores its input, but we used `a -> Const a b` as a function that remembers its input and doesn't do anything else. Isn't there some type specifically created for that?

How might such a type look?

~~~ haskell
newtype Input a b = ...
~~~

Okay, maybe just take `a -> Const a b` itself?

~~~ haskell
newtype Input a b = Input (a -> Const a b)
~~~

No, it won't work – when we go from `Input a b` to `Input s b`, the inner type will change to `s -> Const s b`, but we want it to be `s -> Const a b`. In other words, we want to create a black box which would store its *immediate* input and not the input of the `s -> a` filter that would be put in front of the black box.

The next attempt is to have an [existential type][] – something like this:

~~~ haskell
newtype Input a b = Input (exists x. a -> x)
~~~

[existential type]: https://wiki.haskell.org/Existential_type

This way *we* choose `x`, and nobody else can do anything else with `x` since they don't know what `x` actually stands for.

Unfortunately, there's no `exists` in Haskell, and we can't fake it with `forall` like this

~~~ haskell
newtype Input a b = forall x. Input (a -> x)
~~~

because once we embed anything into `Input`, we'll lose information about what `x` was and we won't be able to get it back. So, we can resort to a simpler trick – making `x` a separate parameter:

~~~ haskell
newtype Input x a b = Input (a -> x)
~~~

Or, if we stick to standard terminology, the [`Forget`][] type:

~~~ haskell
newtype Forget r a b = Forget { runForget :: a -> r }

instance Profunctor (Forget r) where
  dimap f _ (Forget k) = Forget (k . f)
~~~

Now we can replace `Const` with `Forget id`:

~~~ haskell
from :: Iso s t a b -> Iso b a t s
from i = iso bt sa
  where
    sa s = runForget (i (Forget id :: Forget a a (Identity b))) s
    bt b = runIdentity . unTagged $ i (Tagged (Identity b))
~~~

(I had to give `Forget id` a type – involving `Identity` – because otherwise GHC doesn't know what functor to use and gives an “ambiguous type” error.)

-----------------------------------------------------------------------------

And now, enter the pure profunctor lenses territory: if we were able to use `Identity` in both cases, we don't really need a functor there *at all!*

~~~ haskell
type Iso s t a b = forall p. Profunctor p => p a b -> p s t
~~~

Everything suddenly becomes much simpler when cruft stripped away:

~~~ haskell
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap

from :: Iso s t a b -> Iso b a t s
from i = iso bt sa
  where
    sa s = runForget (i (Forget id)) s
    bt b = unTagged (i (Tagged b))
~~~

Of course, this was a pure profunctor iso, not a pure profunctor lens, and I don't want to touch pure profunctor lenses yet because I'm afraid of them:

> ~~~ haskell
> lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
> lens f g = dimap (f &&& id) (uncurry $ flip g) . first'
> ~~~

Maybe later.

# Another derivation of `Iso`

I wrote this section before learning about `Tagged`, and I think it's interesting if only because it's another path getting to the same solution – “well, even if I never thought up *that* approach, I still wouldn't be entirely lost”.

-----------------------------------------------------------------------------

Since an iso is defined by a pair of functions – `s -> a` and `b -> t` – we could try to hide those functions somewhere in the arrow. It's pretty much the same trick we used in the section about isos-as-bidirectional-functions – create a class, make functions an instance, make \<something else\> an instance:

* if we end up doing lens-y things with the iso, well, we won't need \<something else\> at all

* if we end up doing something iso-y, we'll demand \<something else\> from the iso (or the composition of isos)

The simplest thing I can think of is this:

~~~ haskell
data Hide x a b = Hide x
~~~

(`a` and `b` are phantom parameters – we need them to unify with the `->` type.)

Next we can make a class:

~~~ haskell
class Hiding x p where
  hide :: x -> (a -> b) -> p a b
~~~

We can't hide anything in `->`, so we just leave it intact:

~~~ haskell
instance Hiding x (->) where
  hide _ f = f
~~~

We can hide things in `Hide`, tho:

~~~ haskell
instance Hiding x (Hide x) where
  hide x _ = Hide x
~~~

(It's important to realise that neither `Hide` nor `->` contain both the function and \<whatever we're hiding\> – it's one or the other. The trick is that since the caller gets to choose *which* one it wants, in reality it's both.)

Finally, that's our new type for `Iso`:

~~~ haskell
type Iso s t a b =
  forall p f. (Hiding (s -> a, b -> t) p, Functor f) =>
  (a -> f b) -> p s (f t)
~~~

We naively store the representation of the iso in the returned value:

~~~ haskell
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt afb = hide (sa, bt) (\s -> bt <$> afb (sa s))
~~~

And we can get it back by passing a dummy `a -> f b`. How can you get one? For instance, you could just always return `Const ()` as `f b`, but let's use [`Proxy`][] from [`Data.Proxy`][] instead because learning about new things is nice and because it's sort of standard. Here's the definition of `Proxy`:

~~~ haskell
data Proxy t = Proxy
~~~

(It's often used to “pass a type” to a class method without having to write something like `undefined :: Int` – you can write `Proxy :: Proxy Int` instead.)

So, to reverse an iso, we extract a pair of functions from it, swap them around, and create a new iso:

~~~ haskell
from i = iso bt sa
  where Hide (sa, bt) = i (const Proxy)
~~~

Let's create a couple isos, too:

~~~ haskell
bool_int :: Iso Bool Bool Int Int
bool_int = iso fromEnum toEnum

int_char :: Iso Int Int Char Char
int_char = iso toEnum fromEnum
~~~

-----------------------------------------------------------------------------

What's wrong with this approach? Well, nothing, apart from the fact that now our isos don't compose *at all:*

~~~ haskell
bool_char :: Iso Bool Bool Char Char
bool_char = bool_int.int_char
~~~

~~~ {.haskell .repl}
    Could not deduce (Hiding (Bool -> Int, Int -> Bool) p) …
      arising from a use of ‘bool_int’
    from the context (Hiding (Bool -> Char, Char -> Bool) p, Functor f)
      bound by the type signature for
        bool_char :: (Hiding (Bool -> Char, Char -> Bool) p, Functor f) =>
                     (Char -> f Char) -> p Bool (f Bool)
    In the first argument of ‘(.)’, namely ‘bool_int’
    In the expression: bool_int . int_char
    In an equation for ‘bool_char’: bool_char = bool_int . int_char
~~~

It's not that easy to understand the error message, so I'll just explain what went wrong:

* We are using `iso` to put iso's representation into, well, the iso itself.

* We use `iso` twice, so there are 4 functions that end up embedded into our 2 isos. Those functions are of types `Bool -> Int`, `Int -> Bool`, `Int -> Char`, and `Char -> Int`.

* The composition of our isos should have the type `Iso Bool Bool Char Char`. It means that it should have functions of type `Bool -> Char` and `Char -> Bool` embedded into it.

* At no point we are actually doing *anything* with functions of such types. There's no way they could appear!

In other words, we can't hold on indefinitely to once-embedded `s -> a` and `b -> t`, because they need to be updated when we're combining things – but currently our isos don't ever update anything.

-----------------------------------------------------------------------------

Let's take 2 isos: `isoSTAB` and `isoABXY`. (Letters are nicer to work with than `Int` and `Bool` and `Char`.)

When you compose them, you would get `isoSTXY` (just like with lenses).

`isoABXY` can return:

  * `a -> f b`
  * `Hide (a -> x, y -> b)`

`isoSTAB` would have to turn:

  * `a -> f b` into `s -> f t`
  * `Hide (a -> x, y -> b)` into `Hide (s -> x, y -> t)` (because we need to get `isoSTXY` in the end)

So, we need to unify this:

~~~ haskell
(a -> f b)            -> (s -> f t)
Hide (a -> x, y -> b) -> Hide (s -> x, y -> t)
~~~

First of all, we can give `Hide` a different type to make unification easier:

~~~ haskell
data Hide x y a b = Hide (a -> x) (y -> b)  -- same as Hide (a -> x, y -> b)
~~~

Now it looks like this:

~~~ haskell
(a -> f b)    ->  (s -> f t)
Hide x y a b  ->  Hide x y s t
~~~

Or like this, if we make `->` prefix:

~~~ haskell
(->) a (f b)  ->  (->) s (f t)
Hide x y a b  ->  Hide x y s t
~~~

Now we have `f` getting in the way, so let's add it to `Hide`. It won't break anything, because if `f` is a functor *we* would be able to choose (and it is), we can always go like this:

* `Hide x y a (f b)`
* `Hide x y a (Identity b)`
* `Hide x y a b`

So, what we have now is this:

~~~ haskell
(->) a (f b)      ->  (->) s (f t)
Hide x y a (f b)  ->  Hide x y s (f t)
~~~

Or:

~~~ haskell
(->)       a (f b)  ->  (->)       s (f t)
(Hide x y) a (f b)  ->  (Hide x y) s (f t)
~~~

[rubs hands]

Excellent.

-----------------------------------------------------------------------------

Next, let's think what we want to do with both these things— no, wait, we already know the answer:

> `isoSTAB` would have to turn:
> 
>   * `a -> f b` into `s -> f t`
>   * `Hide (a -> x, y -> b)` into `Hide (s -> x, y -> t)` (because we need to get `isoSTXY` in the end)

Except that with our new types...

~~~ haskell
data Hide x y a b = Hide (a -> x) (y -> b)
~~~

...it's a bit different. `isoSTAB` would have to turn:

* `a -> f b` into `s -> f t`
* `Hide x y a (f b)` into `Hide x y s (f t)`

Okay, at this point we have all pieces of the puzzle. We know exactly which 2 types `iso` has to be, and all that is left is writing a typeclass:

~~~ haskell
class MakeIso p where
  iso :: Functor f => (s -> a) -> (b -> t) -> p a (f b) -> p s (f t)
~~~

(Exercise: write instances by yourself!)

-----------------------------------------------------------------------------

The instance for `->` is straightforward:

~~~ haskell
instance MakeIso (->) where
  iso sa bt afb = \s -> bt <$> afb (sa s)
~~~

I don't like how cryptic it looks, but since writing it amounts to Just Following The Types, if you don't understand it just write it by yourself.

The instance for `Hide`, too, is straightforward (and \<same advice applies\>):

~~~ haskell
instance MakeIso (Hide x y) where
  iso sa bt (Hide ax yfb) = Hide sx yft
    where sx  = \s -> ax $ sa s
          yft = \y -> bt <$> yfb y
~~~

For the sake of completeness, here's the new `Iso` type:

~~~ haskell
type Iso s t a b =
  forall p f. (MakeIso p, Functor f) =>
  p a (f b) -> p s (f t)
~~~

-----------------------------------------------------------------------------

We also need to write `from`, and to do that, we need to learn how to extract functions from an iso. Let's use reasoning to do it! (I mean, more reasoning reasoning than usual. Kinda. Whatever.)

We start with an `Iso`:

~~~ haskell
i :: Iso s t a b
~~~

Expand it using the definition of `Iso`:

~~~ haskell
type Iso s t a b =
  forall p f. (MakeIso p, Functor f) =>
  p a (f b) -> p s (f t)

i :: (MakeIso p, Functor f) => p a (f b) -> p s (f t)
~~~

(I removed `forall p f` because it's not really needed there.)

Now, we actually know what we want to get – `Hide a b s (f t)`:

~~~
p s (f t) = Hide a b s (f t)
~~~

From it we can deduce:

~~~
p = Hide a b
~~~

And therefore:

~~~ haskell
i :: (Functor f) => Hide a b a (f b) -> Hide a b s (f t)
~~~

Also, since we can choose `f`, let's choose `Identity` because we can always strip it away:

~~~ haskell
i :: Hide a b a (Identity b) -> Hide a b s (Identity t)
~~~

We're going to get our `Hide a b s (Identity t)` if we can get `Hide a b a (Identity b)` from somewhere. Can we get it?

~~~ haskell
data Hide a b a (Identity b) = Hide (a -> a) (b -> Identity b)
~~~

Yep, seems easy enough – `Hide id Identity`. And once we have `s -> Identity t`, we can combine it with [`runIdentity`][] to get `s -> t`:

~~~ haskell
from i = iso (runIdentity . bt) sa
  where Hide sa bt = i (Hide id Identity)
~~~

-----------------------------------------------------------------------------

It's only left to notice that `MakeIso` is pretty much just like `Profunctor`:

~~~ haskell
class MakeIso p where
  iso :: Functor f => (s -> a) -> (b -> t) -> p a (f b) -> p s (f t)

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
~~~

`Hide` can be easily made an instance:

~~~ haskell
instance Profunctor (Hide x y) where
  dimap sa bt (Hide ax yb) = Hide sx yt
    where sx = ax . sa
          yt = bt . yb
~~~

Then we can change the definitions of `Iso` and `iso`:

~~~ haskell
type Iso s t a b =
  forall p f. (Profunctor p, Functor f) =>
  p a (f b) -> p s (f t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt) afb
~~~

Rename `Hide` to [`Exchange`][] (because that's how it's called in lens):

~~~ haskell
data Exchange a b s t = Exchange (s -> a) (b -> t)
~~~

Write an explicit type – [`AnIso`][] – for \<whatever `from` accepts\>:

~~~ haskell
type AnIso s t a b = Exchange a b a (Identity b) ->
                     Exchange a b s (Identity t) 
~~~

This type can be used whenever we want to write a function which takes an `Iso`, because it's the “smallest” type that fully describes an iso. (By the way, `Lens` and `Traversal` have similar types associated with them – [`ALens`][] and [`ATraversal`][] – but they're implemented using weird-sounding things like `Bazaar` and `Pretext` with very helpful descriptions like “a.k.a. indexed Cartesian store comonad, indexed Kleene store comonad, or an indexed `FunList`” and I don't want to touch them with a 10-foot pole. [sighs] I guess I'll have to sooner or later...)

-----------------------------------------------------------------------------

This approach has actually led us slightly further than the previous one – previously we needed `->` and `Tagged` to implement `from`, but now we can extract both the “getter” and the “setter” in 1 pass (using `Exchange`), and we also can write `AnIso` (which we couldn't do before because we needed the iso to be polymorphic to use it with both `->` and `Tagged`). However, we definitely could've invented `Exchange` without all the mess in this section, by noticing that:

  * We can try to combine `Forget` and `Tagged` to get both thing we need in 1 pass:

    ~~~ haskell
    data FT r a b = FT (a -> r) b
    ~~~

    and make it an instance of `Profunctor` and everything.

  * We can't, however, use `FT` to get both `s -> a` and `b -> t`, because currently we get `b -> t` basically by feeding `b` to the iso and taking `t` from the result. So, with `FT` we can have this:

    ~~~ haskell
    bsat :: b -> (s -> a, t)
    bsat b = let ... = i (FT id (Identity b)) in ...
    ~~~

    but we can't get `b -> t` and `s -> a` separately.

  * The trick is in using `exists x. x -> b` instead of `b` as our type for “constant function” – they're equivalent:

    ~~~ haskell
    data FT r x a b = FT (a -> r) (x -> b)
    ~~~

    (and again we replace `exists` with the type-variable-outside trick).

  * And now `FT` is actually `Exchange` with a different name and type variables. Ha.

# Some useful isos

[`strict`][] converts a lazy `Text` or `ByteString` to a strict one (and back):

~~~ {.haskell .repl}
> lazyTexts & each.strict %~ doSomethingWithStrictText
~~~

(You can use `from strict` to go in the opposite direction, or you can use [`lazy`][].)

[`reversed`][] is an isomorphism between things and... well, reversed things:

~~~ {.haskell .repl}
> "live" ^. reversed
"evil"

> "live" & reversed %~ ('d':)
"lived"
~~~

(It has instances for many various containers, as well as `Text` and so on.)

[`swapped`][] swaps/unswaps sides of tuples or `Either`:

~~~ {.haskell .repl}
> (1,2) ^. swapped
(2,1)

> Left "hi" ^. swapped
Right "hi"
~~~

[`non`][] is an isomorphism for `Maybe a` that lets you assign some arbitrary value to `Nothing`:

~~~ haskell
non :: Eq a => a -> Iso' (Maybe a) a
~~~

~~~ {.haskell .repl}
> Just 185 ^. non 0
185

> Nothing ^. non 0
0
~~~

It's more useful, however, when you're using it for setting/modifying – it combines the “modifying” step and the “checking we don't have any values we don't like” step:

~~~ {.haskell .repl}
> Just 185 & non 0 -~ 185
Nothing
~~~

It's even more useful when you're working with maps and [`at`][] and also nested maps and stuff, but I haven't talked about `at` yet and I don't want to do it *now*, so just read the examples in the docs for [`non`][] if you're interested.

[`enum`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#v:enum
[`empty`]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html#v:empty
[`Alternative`]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative
[`Category`]: http://hackage.haskell.org/package/base/docs/Control-Category.html#t:Category
[`Op`]: http://hackage.haskell.org/package/contravariant/docs/Data-Functor-Contravariant.html#v:Op
[`runIdentity`]: http://hackage.haskell.org/package/base/docs/Data-Functor-Identity.html#v:runIdentity
[`Profunctor`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#t:Profunctor
[`dimap`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#v:dimap
[`Exchange`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Internal-Iso.html#t:Exchange
[`ALens`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:ALens
[`ATraversal`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Traversal.html#t:ATraversal
[`AnIso`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#t:AnIso
[`Control.Lens.Iso`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html
[`strict`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#v:strict
[`lazy`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#v:lazy
[`reversed`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#v:reversed
[`non`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#v:non
[`swapped`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#v:swapped
[`at`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-At.html#v:at
[`Tagged`]: http://hackage.haskell.org/package/tagged/docs/Data-Tagged.html#t:Tagged
[`retag`]: http://hackage.haskell.org/package/tagged/docs/Data-Tagged.html#v:retag
[`Choice`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#t:Choice
[`lmap`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#v:lmap
[`rmap`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#v:rmap
[`Forget`]: http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#t:Forget
[`Proxy`]: http://hackage.haskell.org/package/base/docs/Data-Proxy.html#t:Proxy
[`Data.Proxy`]: http://hackage.haskell.org/package/base/docs/Data-Proxy.html
[`united`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#v:united

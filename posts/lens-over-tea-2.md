% lens over tea #2: composition, laws, getters/actions/setters

---
series:
  top: “lens over tea”
  toplink: /#lens-over-tea
  prev: /lens-over-tea-1
  next: /lens-over-tea-3
---

2 obligatory topics of every lens tutorial seem to be “wow, look how lenses
compose, it's just like OOP” and “wow, look how lenses make working with
records easy”. Until these 2 are covered, then, this tutorial can't be
considered a proper lens tutorial and so should probably be called “a text
which for whatever reason mentions the word “lens” often”.

Template Haskell is unwieldy, so let's start with composition.

# Lens composition

First of all, why had somebody thought in the first place that the words
“lens” and “composition” should occur in a sentence together? (It's an
important question – without at least trying to address it, lens composition
would just look like something pulled out of thin air.)

I don't know. This [Luke Palmer's post][LP accessors] from 2007 suggests that
at least some people were motivated by OOP:

> And then given the Accessor abstraction you could do stuff like define a
> `:=` operator for readability and whatnot. I was proud that you could write
> accessors that accessed things other than the top level of the data
> structure.
>
> But something still wasn't right. I wanted to be able to do something like
> `a.b.c` in OO languages, where `a`, `b`, and `c` were accessors. So here is
> my new Accessor abstraction [...]

[LP accessors]: lukepalmer.wordpress.com/2007/08/05/haskell-state-accessors-second-attempt-composability/

A bit of history:

  * The idea of lenses, or accessors, or “functional references”, had existed
    (and was blogged about) for several years. In those times lenses were
    implemented as a getter–setter pair and didn't allow type changing
    (i.e. using a lens to go from `(a, Int)` to `(a, String)` or something).

  * Then Twan van Laarhoven [proposed][Laarhoven lens] the `Functor`
    formulation (the one we're using). Still nothing about type changing (or
    “polymorphic updates”).

  * Then Russel O'Connor [noticed][O'Connor lens] that van Laarhoven lenses
    could be easily generalised by changing the type from `(a -> f a) -> s ->
    f s` to `(a -> f b) -> s -> f t`, and Edward Kmett developed the concept
    in his [Mirrored Lenses][] post (introducing `Getting` and `Setting`
    along the way).

[Laarhoven lens]: http://www.twanvl.nl/blog/haskell/cps-functional-references
[O'Connor lens]: http://r6.ca/blog/20120623T104901Z.html
[Mirrored Lenses]: http://comonad.com/reader/2012/mirrored-lenses/

## Old lenses

When lenses were still looking like this:

~~~ haskell
data OldLens s a = OldLens
  { get    :: s -> a
  , modify :: (a -> a) -> s -> s }
~~~

the idea of composing them probably felt more natural than it does now with
`type Lens s t a b`. I mean, it's an easy enough step from

> If you have a function from `a` to `b`, and from `b` to `c`, then you can
> get a function from `a` to `c`.

to

> If you can access `b` in `a`, and `c` in `b`, then you can access `c` in
> `a`.

Let's write a composition function for `OldLens`, then.

~~~ haskell
(@.) :: OldLens b c -> OldLens a b -> OldLens a c
(@.) _c _b = OldLens get' modify'
  where
~~~

`get'` is simple. To get `c` from `a`, first get `b` from `a`, and then `c`
from `b`.

~~~ haskell
    get' a = let b = get _b a
             in  get _c b
~~~

`modify'` is pretty simple as well. To modify `c` in `a`, first write a
function which modifies `c` in `b`, and then use it to modify `b` in `a`.

~~~ haskell
    modify' f a = let modifyB :: b -> b
                      modifyB = modify _c f
                  in  modify _b modifyB a
~~~

(I added the type signature for extra clarity.)

If you try to compile it, tho, you'll get a weird-looking error message
saying `Couldn't match type ‘b’ with ‘b1’`. This is because in `modifyB :: b
-> b`, `b` could've been just as well replaced by `a` or `x` or `pony` –
there's no relation to the `b` in `(@.)`'s type signature. So, we'll have to
enable the [`ScopedTypeVariables`](@ghc-ext) extension, which does exactly
what we want – it allows type variables to be *brought into scope* so that
they can be used later. Finally, just enabling the extension doesn't do
anything by itself, only *allows* variables to be brought into scope – you
still have to do it yourself by specifying `forall b` in the signature:

~~~ haskell
{-# LANGUAGE ScopedTypeVariables #-}

...

-- “forall a b c” and not “forall b” because it's all-or-none for “forall”s.
(@.) :: forall a b c . OldLens b c -> OldLens a b -> OldLens a c
(@.) _c _b = OldLens get' modify'
  where
    get' a = let b = get _b a
             in  get _c b
    modify' f a = let modifyB :: b -> b
                      modifyB = modify _c f
                  in  modify _b modifyB a
~~~

If you understand everything, let's shorten things a bit.

~~~ haskell
get' a = let b = get _b a
         in  get _c b

-- Substitute “b”:

get' a = get _c (get _b a)

-- Get rid of “a”:

get' = get _c . get _b
~~~

Same with `modify'`:

~~~ haskell
modify' f a = let modifyB :: b -> b
                  modifyB = modify _c f
              in  modify _b modifyB a

-- Substitute “modifyB”:

modify' f a = modify _b (modify _c f) a

-- Get rid of “a”:

modify' f = modify _b (modify _c f)

-- Get rid of “f”:

modify' = modify _b . modify _c
~~~

Final definition of `(@.)`:

~~~ haskell
(@.) :: OldLens b c -> OldLens a b -> OldLens a c
(@.) _c _b = OldLens
  { get    = get    _c . get    _b
  , modify = modify _b . modify _c }
~~~

And here's an example of how to use lens composition to work with the first
element of a pair in a nested list (row x, column y) [darn, so that's
probably why tutorial writers start with records – to be able to use more
interesting lenses in examples]. Assume that `^.` (reminding: that's
`flip view`), `*~` (like `set` which multiplies instead of setting), `_1`,
`_2`, `ix` were already defined for `OldLens` – there's nothing interesting
or surprising about their definitions anyway.

~~~ {.haskell .repl}
> let field = [[(x, y) | x <- [0..3]] | y <- [0..3]]

> field
[ [ (0,0), (1,0), (2,0), (3,0) ]
, [ (0,1), (1,1), (2,1), (3,1) ]
, [ (0,2), (1,2), (2,2), (3,2) ]
, [ (0,3), (1,3), (2,3), (3,3) ] ]

> -- Getting.

> field ^. (ix 3 @. ix 1)  -- 1st row, 3rd column.
(3, 1)

> field ^. (_1 @. ix 3 @. ix 1)
3

> -- Modifying.

> field & ((_1 @. ix 3 @. ix 1) *~ 100)
[ [ (0,0), (1,0), (2,0), (3  ,0) ]
, [ (0,1), (1,1), (2,1), (300,1) ]
, [ (0,2), (1,2), (2,2), (3  ,2) ]
, [ (0,3), (1,3), (2,3), (3  ,3) ] ]
~~~

## Categories

If you have been programming in Haskell long enough, you might've noticed
that there's quite a number of things that can be composed.

Functions:

~~~ haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
~~~

Monad functions (this one isn't used often, probably for reasons like “we
have do-notation”, but I don't really know):

~~~ haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
~~~

[Conduits][conduit tutorial] (in case you haven't heard about these before,
they are an abstraction for streams – a `Conduit i m o` consumes some items
of type `i` and produces items of type `o`. They are very useful for doing IO
efficiently):

[conduit tutorial]: https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview

~~~ haskell
(=$=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
~~~

[Pipes][pipes tutorial] (same as conduits, but *don't* ask me which one is
better):

[pipes tutorial]: https://hackage.haskell.org/package/pipes/docs/Pipes-Tutorial.html

~~~ haskell
(<-<) :: Monad m => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
~~~

Can this pattern be generalised? Meet [`Category`][]:

~~~ haskell
class Category cat where
  -- An “identity element” – doesn't change anything when composed.
  id :: cat a a
  -- Composition – if you replace “cat” with e.g. “~>”, its type becomes
  -- “(b ~> c) -> (a ~> b) -> (a ~> c)”.
  (.) :: cat b c -> cat a b -> cat a c
~~~

Now, I haven't actually ever used *this* instead of usual `.` and `id`, and I
suspect that you won't either. At least until people become more accepting of
categories (and maybe then we all will be able to stop inventing yet another
way to draw an arrow with ASCII symbols, for heavens' sake). However, since
other lens libraries (such as [fclabels][fclabels category] or
[data-lens][data-lens category]) have `Category` instances for their lenses,
we should at least know what all this means and how to use it and whether
it's the best thing since sliced bread and so on and so forth.

[fclabels category]: https://hackage.haskell.org/package/fclabels/docs/Data-Label.html
[data-lens category]: http://hackage.haskell.org/package/data-lens/docs/Data-Lens-Common.html

Here's the trivial instance of `Category` for `OldLens`:

~~~ haskell
instance Category OldLens where
  id = OldLens id id
  (.) = (@.)
~~~

With this instance at hand we can achieve our Ultimate Goal, namely,
composing lenses without annoying operators such as `@.`. Observe (um, just
in case, reminding you again that it won't work in GHCi unless you've
bothered to rewrite all the lens functions):

~~~ {.haskell .repl}
> field ^. _1 . ix 3 . ix 1
3

> field & _1 . ix 3 . ix 1 *~ 100
[ [ (0,0), (1,0), (2,0), (3  ,0) ]
, [ (0,1), (1,1), (2,1), (300,1) ]
, [ (0,2), (1,2), (2,2), (3  ,2) ]
, [ (0,3), (1,3), (2,3), (3  ,3) ] ]
~~~

You might notice that it's hard to read these examples – they mess with the
concept of “flow” in your head. When you write `f . g . h $ x`, you see the
flow – “here's `x`, it goes to `h`, then `g` does something, then `f`”. It
doesn't even matter whether it's left-to-right or right-to-left, as long as
there's continuity. For instance, (at least for me) this is easier to read:

~~~ {.haskell .repl}
> view (_1 . ix 3 . ix 1) field
3

> 100 ~* _1 . ix 3 . ix 1 $ field
[ [ (0,0), (1,0), (2,0), (3  ,0) ]
, [ (0,1), (1,1), (2,1), (300,1) ]
, [ (0,2), (1,2), (2,2), (3  ,2) ]
, [ (0,3), (1,3), (2,3), (3  ,3) ] ]
~~~

(`~*` doesn't exist in lens library – I made it up. If it did exist, it
would've been `flip (*~)`.)

Okay, enough of those “old lenses”. Can we get composition for *our* lenses?

## New lenses

Before making a `Category` instance, let's work out how to actually do
composition. I'm going to reuse the `@.` name for our operator:

~~~ haskell
(@.) :: Lens' b c -> Lens' a b -> Lens' a c
~~~

Here goes the process:

  * We're given 2 lenses and we're constructing a lens. A lens is a function
    which takes -something which acts on a part- and produces -something
    which acts on the whole-:

    ~~~ haskell
    (@.) _c _b = \f -> ...
    ~~~

  * Here `f` acts on `c`, and we want to get something which acts on `a`,
    because `Lens' a c`.

  * We can use `_c` to get something which acts on `b`:

    ~~~ haskell
    (@.) _c _b = \f -> ... (_c f)
    ~~~

  * And then we can use `_b` to get something which acts on `a`, which is
    exactly what we want!

    ~~~ haskell
    (@.) _c _b = \f -> _b (_c f)
    ~~~

  * A-ha, and then we can get rid of `f`...

    ~~~ haskell
    (@.) _c _b = _b . _c
    ~~~

  * ...a-and of `_c` and `_b` as well...

    ~~~ haskell
    (@.) = flip (.)
    ~~~

  * ...aw *darn*.

“Hey, why darn?” – because it's almost, almost function composition, but it's
flipped function composition, so we'd still have to wrap it in a `newtype` to
make a `Category` instance— you know, screw `Category`, I'm satisfied with
the outcome anyway – after all, we can compose our lenses with a mere `.`
from `Prelude` and *they* all can't. Ha.

But still... why?

## “Backward composition”

Let's rewrite our examples with `OldLens` to lens (by just reversing the
order of composed lenses):

~~~ {.haskell .repl}
> field ^. ix 1 . ix 3 . _1
3

> field & ix 1 . ix 3 . _1 *~ 100
[ [ (0,0), (1,0), (2,0), (3  ,0) ]
, [ (0,1), (1,1), (2,1), (300,1) ]
, [ (0,2), (1,2), (2,2), (3  ,2) ]
, [ (0,3), (1,3), (2,3), (3  ,3) ] ]
~~~

Now the flow is right – in the first example `field` “goes” to `ix 1`, then to
`ix 3`, then to `_1` and we get the result. In the second example `* 100`
“passes” thru a chain of lenses which turn it into a more and more complex
modifier, until it finally gets applied to `field`.

In case of `view`, I clearly prefer the “normal” order. `view (_1 . ix 3 . ix
1) field` reads just like `fst . (!! 3) . (!! 1) $ field`, which is what I'm
used to when I'm working with getters. However, when I'm modifying, the
“backward” order is better – `field & ix 1 . ix 3 . _1 *~ 100` (or,
alternatively, `(ix 1 . ix 3 . _1 *~ 100) field`) reads like `mapElem 1
(mapElem 3 (mapFst (* 100))) field`, which, again, reads like what I'm used
to when I'm working with setters.

So, what's the answer to “why”? It's simple: a `Lens' s a` is a *setter*
first and foremost, and a getter only by a clever (and not very obvious)
`Const` hack. If the order of type parameters was changed appropriately – if
it was `Lens' a s` instead of `Lens' s a` – `.` would've been a “proper”
composition operator for lenses.

## Okay, but can we fix it if we *really* want to?

Again: it's only a “bug” if you view lenses as getters. When you're using
them as setters, all other libraries are buggy. That said... yeah,
[we can][Tekmo flipped lens], and we don't even need newtypes or anything.

[Tekmo flipped lens]: http://www.reddit.com/r/haskell/comments/1oa9qx/beginners_cheatsheet_to_the_lens_library/ccq9mre

The trick is slightly reminiscent of the one we used with difference lists –
if `x` is a lens, then we'll define `x'` to be a function which “appends” `x`
to the end of a “lens chain”:

~~~ haskell
_1' l = l . _1
_2' l = l . _2
ix' i l = l . ix i
~~~

or, in pointfree style:

~~~ haskell
_1' = (. _1)
_2' = (. _2)
ix' i = (. ix i)
~~~

Here's how it lets us reverse our “backward” lens chain:

~~~ haskell
view (ix 1 . ix 3 . _1)

-- Add “id” to the chain (we can do it since it's the “constant lens”).

view (id . ix 1 . ix 3 . _1)

-- Move “_1” to the beginning.

view (_1' (id . ix 1 . ix 3))

-- Move “ix 3” to the beginning.

view (_1' (ix' 3 (id . ix 1)))

-- Move “ix 1” to the beginning.

view (_1' (ix' 3 (ix' 1 (id))))

-- Use “.” and “$” instead of brackets.

view (_1' . ix' 3 . ix' 1 $ id)

-- Move “$ id” to “view”.

(view . ($ id)) (_1' . ix' 3 . ix' 1)

-- And call it «view'».

view' = view . ($ id)

view' (_1' . ix' 3 . ix' 1)
~~~

In the same way we can change `lens`, `over`, and every other lens or
function using lenses. By the way, what's the type of *these* lenses?

~~~ haskell
_1  :: Functor f => (a -> f a) -> (a, x) -> f (a, x)
_1' :: Functor f => (((a, x) -> f (a, x)) -> c) -> (a -> f a) -> c
~~~

Not very illuminating. I'll add a couple of type synonyms to make it easier
(warning: it won't compile due to type unification errors):

~~~ haskell
type Modifier = Functor f => a -> f a
type TupleModifier = Functor f => (a, x) -> f (a, x)

_1  :: Modifier -> TupleModifier

_1' :: (TupleModifier -> c) -> (Modifier -> c)
~~~

Or in English:

> `_1` takes a simple modifier and makes a modifier which acts on tuples from
> it.
>
> `_1'` takes a function which needs a *tuple* modifier to produce some `c`,
> and then it takes a *simple* modifier and produces a `c`.

Is it clear how/why they compose? [Just in case it's not obvious already:
when I say “is it clear”/“do you understand”, it almost always means that
either I don't understand, or I did 5min ago but not anymore.] Here's
another step-by-step explanation:

~~~ haskell
-- These are the types for ordinary lenses.

_1   :: Modifer -> TupleModifier
ix 3 :: Modifier -> ListModifier

-- These are the types for new lenses.

_1'   :: (TupleModifier -> c) -> (Modifier -> c)
ix' 3 :: (ListModifier  -> c) -> (Modifier -> c)

-- In order for «_1' . ix' 3» to work, the output type of «ix' 3» has to
-- match the input type of «_1». So, here's the more specialised type:

ix' 3 :: (ListTupleModifier -> c) -> (TupleModifier -> c)

-- So, the composition has this type:

_1' . ix' 3 :: (ListTupleModifier -> c) -> (Modifier -> c)
~~~

## Various stuff

There are a few things left to mention:

  * So far we've been assuming simple lenses – `Lens' s a`, and not `Lens s t
    a b`. However, the latter ones compose just as well, provided that the
    types match.

  * Traversals can be composed with lenses, as they have the same type
    (modulo `Functor`/`Applicative`). When you're composing 2 things of same
    types but with different constraints, the result is something which has
    both constraints – but since `Functor` is already a constraint of
    `Applicative`, the resulting constraint is just `Applicative`.

## Recap

  * Function-like stuff which can be composed forms a `Category`.

  * Lenses can be composed using ordinary function composition. If `_b` is a
    lens for accessing `b` in `a`, and `_c` is a lens for accessing `c` in
    `b`, then `_b . _c` is a lens for accessing `c` in `a`:

    ~~~ haskell
    (.) :: Lens' a b -> Lens' b c -> Lens' a c
    ~~~

    This allows for OOP-like views and updates:

    ~~~ {.haskell .repl}
    > [(1, 2), (3, 4)] ^. ix 0 . _1  -- Getting.
    1

    > [(1, 2), (3, 4)] & ix 0 . _1 .~ 8  -- Setting.
    [(8, 2), (3, 4)]

    > [(1, 2), (3, 4)] & ix 0 . _1 %~ (+ 7)  -- Modifying in general.
    [(8, 2), (3, 4)]

    > [(1, 2), (3, 4)] & ix 0 . _1 +~ 7  -- Adding.
    [(8, 2), (3, 4)]
    ~~~

  * When you've `view`ing, the order of composition seems reversed.

    ~~~ haskell
    -- To get field C of field B of field A, you can use this:
    view fieldC . view fieldB . view fieldA

    -- Or this:
    view (fieldA . fieldB . fieldC)
    ~~~

  * When you're modifying, the order of composition seems normal.

    ~~~ haskell
    -- To apply “f” to field C of field B of field A, you can use this:
    over fieldA (over fieldB (over fieldC f))

    -- Or this:
    over (fieldA . fieldB . fieldC) f
    ~~~

  * Lenses can be modified to reverse the composition order – it only
    requires changing every lens from `x` to `(. x)`, and then applying `($
    id)` to the “inverted” lens to get a normal lens from it. We won't
    actually do all this, because the types become kinda scary and also
    because we don't really need it.

  * Everything above applies to polymorphic lenses (`Lens s t a b`) as well
    as to simple lenses (`Lens' s a`):

    ~~~ haskell
    (.) :: Lens s t a b -> Lens a b x y -> Lens s t x y
    ~~~

  * Everything above applies to traversals (`Applicative f => (a -> f b) -> s
    -> f t`) as well as to lenses (`Functor f => (a -> f b) -> s -> f t`) –
    type constraints don't harm composing. When a lens and a traversal are
    composed, the result is a traversal.

# A bit about laws

Traversals and lenses have some associated laws, pretty much like
[monads][monad laws] or [functors][functor laws]. I'll describe these laws in
a moment.

[monad laws]: https://www.haskell.org/haskellwiki/Monad_laws
[functor laws]: http://en.wikibooks.org/wiki/Haskell/The_Functor_class#The_functor_laws

Disclaimer: I won't preach about Sticking To The Laws in this section (yada
yada “or else [lens police][] will come after you!”), but it doesn't mean
that I think that you can go and do whatever you want and make all kind of
weird improper lenses and put them on Hackage and it would be morally
acceptable. On the other hand, I don't -think that you shouldn't ever use
improper lenses- either. And there's place for `head` and `fromJust` in this
world, too.

[lens police]: http://www.reddit.com/r/haskell/comments/1dpk2b/haskell_for_all_program_imperatively_using/c9szhxa

## Traversal laws

(Why start with traversals? Because every lens is a traversal, but not every
traversal is a lens, which means that lenses have some additional laws
-traversals don't have-, which means that starting with traversals is
easier.)

### Identity

If `t` is some `Traversal`, then:

~~~ haskell
t pure ≡ pure
~~~

And since nobody can forbid me to quote the definition of `Traversal` as many
times as I please:

~~~ haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
~~~

Now, what this law implies is that you can't change the shape of the
structure. Omitting every second element of a list isn't okay; duplicating
elements isn't okay either; even things which technically don't change the
shape are wrong – for instance, a traversal of a list can't reverse it (but
it can process the list in reversed order or even random order).

An interesting question is whether this law implies that you can't leave some
elements unprocessed. For polymorphic traversals, it does – but only because
you *can't* leave an element unprocessed without omitting it entirely (how
can you leave an `Int` unprocessed and yet include it when you're converting
`[Int]` into, say, `Maybe [String]`?). However, for simple traversals which
don't *claim* to be able to perform polymorphic updates – i.e. `Traversal'`s
– it doesn't mean any such thing. A `Traversal'` which alternates between
applying the given function and applying `pure` is a valid traversal (even
`const pure` is a valid traversal), and in fact there are several such
traversals in lens.

[`ignored`][] is exactly `const pure` – just an empty traversal:

~~~ {.haskell .repl}
> 1 ^.. ignored
[]

> "hi" & ignored +~ 2
"hi"
~~~

[`taking`][] takes an existing traversal and makes a traversal which visits
only the first `n` elements:

~~~ {.haskell .repl}
> [1..10] & taking 5 each %~ negate
[-1,-2,-3,-4,-5,6,7,8,9,10]
~~~

[`dropping`][] does... well, guess what:

~~~ {.haskell .repl}
> [1..10] & dropping 5 each %~ negate
[1,2,3,4,5,-6,-7,-8,-9,-10]
~~~

(Don't try to implement them yet, it's hard.)

[`elementsOf`][] generalises `taking` and `dropping` to arbitrary index
predicates:

~~~ {.haskell .repl}
> [[1],[3,5],[7,9,11],[13,15,17,19]] & elementsOf (each.each) even .~ 0
[[0],[3,0],[7,0,11],[0,15,0,19]]
~~~

(The lists only have odd numbers on purpose – otherwise someone might think
`elementsOf` applies `even` to elements and not to indexes.)

Finally, this law also means that it should be possible to restrict every `t :: Traversal s t a b` to `t :: Traversal s s a a` – otherwise `t pure` won't even typecheck. In general, there are restrictions on what `s`, `t`, `a` and `b` can be for lenses and traversals – see Edward's [Mirrored Lenses][] post for details.

### Composition

This one looks kinda scary at first. If `t` is some `Traversal`, then:

~~~ haskell
fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)
~~~

For the sake of simplicity, I'll make some assumptions. Let's say `t` is a
list traversal:

~~~ haskell
t :: forall f . Applicative f => (a -> f b) -> [a] -> f [b]
~~~

`f` and `g` are just some functions:

~~~ haskell
g :: a -> G b
f :: b -> F c
~~~

`t f` and `t g` would be the same functions, “lifted” to work on lists:

~~~ haskell
t g :: [a] -> G [b]
t f :: [b] -> F [c]
~~~

The left part is therefore:

~~~ haskell
fmap (t f) . t g :: [a] -> G (F [c])
~~~

The right part is built as follows:

~~~ haskell
fmap f . g :: a -> G (F c)
~~~

We already used [`Compose`][] in the previous part to combine 2 functors, but I haven't defined it, and so I'll do it here:

~~~ haskell
newtype Compose f g a = Compose {getCompose :: f (g a)}

-- And some easy-to-write instances:
(Functor     f, Functor     g) => Functor     (Compose f g)	where ...
(Applicative f, Applicative g) => Applicative (Compose f g)	where ...
(Foldable    f, Foldable    g) => Foldable    (Compose f g)	where ...
(Traversable f, Traversable g) => Traversable (Compose f g)	where ...
(Alternative f, Applicative g) => Alternative (Compose f g)	where ...
~~~

Armed with this knowledge, we can infer the type of `Compose . fmap f . g`:

~~~ haskell
Compose . fmap f . g :: a -> Compose G F c
~~~

When using `t` with this, `Compose G F` would be our `Applicative`:

~~~ haskell
t (Compose . fmap f . g) :: [a] -> Compose G F [c]
~~~

Finally, `getCompose` brings us `G (F [c])`.

As an example of a traversal ruled out by this law, let's take

-----------------------------------------------------------------------------

[huffs]

Why did I think several months ago that going to sleep instead of finishing
this sentence was a good idea.

Why.

Okay, I remember vaguely that I had asked a question on Stackoverflow about
it... Aha! [Here it is.][SO failing question]

[SO failing question]: http://stackoverflow.com/questions/27138856/why-does-failing-from-lens-produce-invalid-traversals

I'll retell it here for you; all credit goes to— do I link to Twitter, or SO profile, or Github? and do I use their real name or just nick? decisions, decisions— whatever, [bennofs][].

[bennofs]: http://stackoverflow.com/users/2494803/bennofs

We take a data type:

~~~ haskell
data Present = Wrapped Present | Toy
~~~

Now we write a traversal to unwrap the present:

~~~ haskell
unwrapped :: Traversal' Present Present
unwrapped f (Wrapped x) = Wrapped <$> f x
unwrapped f Toy         = f Toy             -- Instead of “pure Toy”.
~~~

Instead of not doing anything when there's nothing to unwrap, it just
pretends the toy *was* wrapped and does something to it. Too darn smart for
its own good.

The intuitive law this traversal doesn't follow is “you get what you put in”
– specifically, `(whatever & t .~ x) ^.. t` should give you a list consisting
of `x`s back. Which it won't, as you'll see now.

Let's take `Toy`:

~~~ {.haskell .repl}
> Toy
Toy
~~~

And set the “present inside” (which is just `Toy` itself, because there is no
wrapping at all) to be `Wrapped Toy`:

~~~ {.haskell .repl}
> Toy & unwrapped .~ Wrapped Toy
Wrapped Toy
~~~

And now unwrap it:

~~~ {.haskell .repl}
> (Toy & unwrapped .~ Wrapped Toy) ^.. unwrapped
[Toy]
~~~

We should've gotten `[Wrapped Toy]` back, but got just `[Toy]`. Ouch.

Now, what does it all have to do with the second traversal law? Well, let's
rewrite the example using definitions of `.~` and `^..`:

~~~ haskell
t .~ b = runIdentity . t (\_ -> Identity b)

v ^.. t = getConst $ t (\x -> Const [x]) v
~~~

Which brings us to:

~~~ haskell
Toy & runIdentity . unwrapped (\_ -> Identity (Wrapped Toy))
    & getConst . unwrapped (\x -> Const [x])
~~~

I'll also get rid of `runIdentity` and `getConst` to make lines shorter, and
change the order to the one I'm more used to. And rename some pieces:

~~~ {.haskell .repl}
> let set' = \_ -> Identity (Wrapped Toy)

> let get' = \x -> Const [x]

> fmap (unwrapped get') . unwrapped set' $ Toy
Identity (Const [Toy] Toy)
~~~

I had to use `fmap` because we didn't unwrap `Identity` after doing
`unwrapped set'`. Also, there's no `Show` instance for `Const`, so I just
made the output up.

Anyway, it's exactly the left part of the law: `fmap (t f) . t g`. What about
the right part? Let's try to write it without looking at it. (I looked
accidentally before the idea to write it without looking came to my head, but
I have bad memory when it comes to things I saw a minute ago, so it's
alright.) The *point* of the right part is that we somehow apply the
traversal only once. Okay, what have we got?

~~~ haskell
get' :: t -> Const [t] b
set' :: t -> Identity Present
~~~

Or, specialised:

~~~ haskell
get' :: Present -> Const [Present] b
set' :: Present -> Identity Present
~~~

And we need to give `unwrapped` something of the type `Present -> f
Present`. Hm.

We could just compose `get'` and `set'`:

~~~ haskell
get' . set' :: Present -> Const [Identity Present] b
~~~

It would give us:

~~~ haskell
unwrapped (get' . set') Toy
  :: Const [Identity Present] Present
~~~

But the left part had a different type:

~~~ haskell
fmap (unwrapped get') . unwrapped set' $ Toy
  :: Identity (Const [Present] Present)
~~~

Can't we get the same type?

We can, but it needs some shenanigans. First, instead of just applying `get'`
let's `fmap` it:

~~~ haskell
fmap get' . set' :: Present -> Identity (Const [Present] b)
~~~

Now if we give this to `unwrapped`, it will only see `Identity` as the
functor part, which is not what we want. We'd like to have both `Identity`
and `Const [Present]` handled together.

That's exactly what `Compose` does – it lets us join functors!

~~~ haskell
Compose :: f (g a) -> (Compose f g) a
~~~

So, the whole thing looks like this:

~~~ haskell
Compose . fmap get' . set'
  :: Present -> Compose Identity (Const [Present]) b
~~~

And here's what would happen after `unwrapped`:

~~~ haskell
unwrapped (Compose . fmap get' . set') Toy
  :: Compose Identity (Const [Present]) Present
~~~

~~~ {.haskell .repl}
> unwrapped (Compose . fmap get' . set') Toy
Compose (Identity (Const [Wrapped Toy]))
~~~

Now it's only left to strip `Compose` off:

~~~ {.haskell .repl}
> getCompose $ unwrapped (Compose . fmap get' . set') Toy
Identity (Const [Wrapped Toy])
~~~

Finally, the same shape as the left part of the equation – but not the same
output! It happened because in the left part `get'` operated on the thing
`unwrapped` gave to it, and in the right part `get'` operated *directly* on
the result of `set'`. Here's exactly what I said right now but in list form
because lists are cool:

* left part

    * `unwrapped` sees `Toy`
    * `unwrapped` gives `set'` `Toy`
    * `set'` replaces it with `Wrapped Toy`
    * the next `unwrapped` gives `get'` another `Toy`, and not `Wrapped Toy`
    * `get'` carries `Toy` out in its magical hidden list

* right part

    * `unwrapped` sees `Toy`
    * `unwrapped` gives `set'` `Toy`
    * `set'` replaces it with `Wrapped Toy` and gives to `get'`
    * `get'` carries `Wrapped Toy` out in its magical hidden list

### No duplicates

Another consequence of the composition law is that you can't visit the same
element twice – at least, it says so in the lens documentation:

> Another testament to the strength of these laws is that the caveat
> expressed in section 5.5 of the “Essence of the Iterator Pattern” about
> exotic `Traversable` instances that traverse the same entry multiple times
> was actually already ruled out by the second law in that same paper!

So, something like

~~~ haskell
traverseTwice :: Traversal [a] [b] a b
traverseTwice f = T.traverse (\x -> f x *> f x)
~~~

is disallowed.

This remark was added in version 2.4, and previously “no visiting the same
entry twice” was a separate law. [The commit][lens third law] introducing the
remark is kinda concise:

[lens third law]: https://github.com/ekmett/lens/commit/3a2e053ea78e6d45d32da3101fc693b0ae0fd1e0

> removed the third traversal law. roconnor proved it can't be infringed

The proofs are here:

> Richard Bird's proof: <http://www.cs.ox.ac.uk/jeremy.gibbons/publications/uitbaf.pdf>
>
> Russell O'Connor's proof: <http://r6.ca/blog/20121209T182914Z.html> in coq: <https://raw.githubusercontent.com/oconnorr/traversable-fincontainer/master/decompose_traversal.v>
>
> Both of these show you can separate "shape" from data in every legal
> traversal, ruling out the multiple visit scenario Gibbons feared in the
> Essence of the Iterator Pattern.

-----------------------------------------------------------------------------

Months ago I left myself a cryptic remark here:

> real ix, real at, all'

I don't know what it means. I only remember vaguely that `ix`—
[looks at the previous part] ah, right, I defined `ix` as a lens, and it
really should be a traversal. Well, then I guess “rewriting `ix`” shall be
left as homework or something. Or just don't bother.

I don't even know what `all'` could be doi— [curiosity wins, looks at the
previous part again] turns out `all'` just traverses all elements which are
equal to some value. And it's not a proper traversal because it violates the
composition law – 2 `all'`s in a row might not be looking at the same
elements (for instance, if you traverse all `0`s and change them to `2`s,
then the next time you'll look at `all' 0` you won't see *anything*).

Still could be pretty useful, y'know.

## Lens laws

There are 3 lens laws. I am going to copy them from lens documentation. I'm
so lazy. I hate long sentences. I've read the [Harry the Hufflepuff][] fanfic
recently and I think I got infected with laziness or something. Or maybe it's
sleep deprivation. Whatever. Let's go.

[Harry the Hufflepuff]: https://www.fanfiction.net/s/6466185/2/Harry-the-Hufflepuff

>   1. You get back what you put in:
>
>     ~~~ haskell
>     view l (set l v s) ≡ v
>     ~~~
>
>   2. Putting back what you got doesn't change anything:
>
>     ~~~ haskell
>     set l (view l s) s ≡ s
>     ~~~
>
>   3. Setting twice is the same as setting once:
>
>     ~~~ haskell
>     set l v' (set l v s) ≡ set l v' s
>     ~~~

The third law seems to follow from the second traversal law. I don't see why it
doesn't. Tell me if it doesn't, so that I could be properly embarrassed.

The second law is not even applicable to traversals, and the first law is almost
not applicable.

Study the 3 laws and be wise in your ways.

## Recap

  * Don't write traversals which remove elements or duplicate elements or
    change shape of the structure or whatever. That's not what traversals are
    for.

  * Not traversing some elements is fine.

  * A traversal should compose with itself – i.e. doing `over t f . over t g`
    should be the same as `over t (f . g)`. If you're violating this
    intentionally, maybe mark the traversal as “law-violating under such and
    such conditions, take care”.

  * [`Compose`][] from `Data.Functor.Compose` composes functors. For
    instance, here's how you can write `fmap2` to look 2 functors deep:

    ~~~ {.haskell .repl}
    > let fmap2 = fmap.fmap

    > fmap2 negate [[1],[2]]
    [[-1],[-2]]
    ~~~

    Or how you could do it with `Compose`:

    ~~~ {.haskell .repl}
    > let fmap2 f = getCompose . fmap f . Compose

    > fmap2 negate [[1],[2]]
    [[-1],[-2]]
    ~~~

  * A traversal shouldn't traverse the same element twice.

  * Lens laws are as follows: you get what you put in, if you put what you
    get the whole thing is still the same, and the second put overwrites the first
    one.

# Getters

You can regard `view` as something which makes a simple function from a
lens. It's useful if you want to employ an occasional lens when writing
“ordinary” code:

~~~ haskell
-- This is a lens which lets you access a deeply concealed value in a record.
-- I don't want to actually write it merely for the sake of an example, but
-- just believe me that it's *very* deep inside and everything is complicated
-- and so you really, really like the idea of not having to write *both* a
-- getter and a setter for it.
deepValue :: Lens' AmazinglyComplicatedStructure Int
deepValue = ...

main = do
  putStrLn "Looking for it..."
  ws <- filter ((== 42) . view deepValue) <$> getWorlds
  forM_ ws $ \world -> do
    new <- randomRIO (1, 100)
    modifyWorld world (set deepValue new)
~~~

However, when the code is fully (or mostly) lens-based, we would want to
inject *functions* into the flow of lenses, rather than the other way
round. For instance, lenses are more convenient than ordinary functions when
it comes to various maps:

~~~ haskell
import qualified Data.Map as M

bindings :: M.Map String String
bindings = M.fromList
  [ ("cancel" , "C-g")
  , ("search" , "C-s")
  , ("refill" , "M-q")
  , ("save"   , "C-x C-s")
  , ("occur"  , "C-s o")
  ]
~~~

You want to get a keybinding for “save”:

~~~ {.haskell .repl}
> bindings ^?! ix "save"
"C-x C-s"
~~~

By the way, `^?!` is an unsafe version of `^?` – it doesn't wrap its result
in `Maybe`, but it also throws an exception when there aren't any elements:

~~~ {.haskell .repl}
> bindings ^? ix "bark"
Nothing

> bindings ^?! ix "bark"
"*** Exception: (^?!): empty Fold
~~~

Okay, now you want to take the first key group of the binding:

~~~ {.haskell .repl}
> takeWhile (not . isSpace) $ bindings ^?! ix "save"
"C-x"
~~~

Unfortunately, the flow is broken – first goes `bindings`, then `ix`, then
`takeWhile`, and the result is slightly messy. You can fix the situation with
`&`:

~~~ {.haskell .repl}
> bindings ^?! ix "save" & takeWhile (not . isSpace)
"C-x"
~~~

But now you lose the ability to easily get back to safety:

~~~ {.haskell .repl}
> bindings ^? ix "save" & takeWhile (not . isSpace)

<interactive>:
    Couldn't match type ‘[Char]’ with ‘Maybe [Char]’
    Expected type: Maybe [Char] -> [Char]
      Actual type: [Char] -> [Char]
    In the second argument of ‘(&)’, namely
      ‘takeWhile (not . isSpace)’
    In the expression:
      bindings ^? ix "save" & takeWhile (not . isSpace)
~~~

Aw darn. Okay, `&` can be replaced with `<&>` (which is defined in lens as
the flipped version of `<$>`):

~~~ {.haskell .repl}
> bindings ^? ix "save" <&> takeWhile (not . isSpace)
Just "C-x"
~~~

Even so, trying to add an additional lens after `takeWhile` would finally
break our solution. There must be a simpler way... A-ha! Remember about
`lens`?

~~~ haskell
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
~~~

Well, since all lenses can be composed, and we don't intend to use the lens
for setting anyway – only for convenient getting – it should be possible to
use `lens` to create a “getting lens” out of an `s -> a` function.

~~~ haskell
to :: (s -> a) -> Lens' s a
to getter = lens getter setter
  where setter = error "to: tried to set a getting-only lens"
~~~

Will it work now? Will it crash for some unexpected reason? Let's check:

~~~ {.haskell .repl}
> bindings ^? ix "save" . to (takeWhile (not . isSpace))
Just "C-x"

> bindings ^? ix "save" . to (takeWhile (not . isSpace)) . _last
Just 'x'
~~~

Hooray, it doesn't crash. Our quest is complete... or is it?

## A note regarding readability

Let's compare the lens version and the version which doesn't use lenses:

~~~ haskell
-- Lens version.
bindings ^? ix "save" . to (takeWhile (not . isSpace)) . _last

-- Usual version.
last . takeWhile (not . isSpace) <$> M.lookup "save" bindings
~~~

The “usual version” seems to be shorter and cleaner. So, is it true that the
lens library should only be used for setting, and getting is easier done with
already existing functions?

Not quite. Getting tends to become clumsy mainly because everybody has a
different idea about how to write getting functions! If you want to get
something from a structure, you have to consider this:

  * What do you want to do upon failure? If “throw an exception” is a
    satisfactory answer, you'll have to use `fromJust` on all those functions
    which return `Maybe` (e.g. [`M.lookup`][]).

  * If you'd rather get a `Maybe` instead, be prepared to deal with functions
    which *don't* return `Maybe`. This is `head`, `tail`, `!!` and so on.

      * For some there are equivalent functions – e.g. a safe variant of
        `head` lives in base as [`listToMaybe`][].

      * For some, there's a “sibling function” in some package –
        e.g. [safe](@hackage) provides a lot of functions such as
        [`maximumMay`][].

      * For some, you can check whether the getter will fail or not before
        actually using it – e.g. `inRange (0, length list - 1)` for `!!`.

      * Finally, if all else fails, [`teaspoon`][] comes to rescue:

        ~~~ {.haskell .repl}
        > teaspoon $ head [1]
        Just 1

        > teaspoon $ head []
        Nothing
        ~~~

        Note that `teaspoon` doesn't distinguish between exceptions and
        `undefined`, so you have to wrap everything into some data type
        (`Identity` won't do because it's a `newtype`) and unwrap afterwards
        if you want to preserve semantics:

        ~~~ {.haskell .repl}
        > head [undefined]
        *** Exception: Prelude.undefined

        > teaspoon $ head [undefined]
        Nothing

        > data X a = X {getX :: a}
        > fmap getX . teaspoon $ head $ fmap X [undefined]
        Just *** Exception: Prelude.undefined
        ~~~

  * Getters which never fail (e.g. `sum`) have to be used with `fmap` or
    `<$>` in order to be composed with `Maybe` getters.

  * Composing 2 `Maybe` getters requires [`<=<`][]:

    ~~~ haskell
    (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
    -- Or, specialised for Maybe:
    --     (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
    ~~~

    ~~~ {.haskell .repl}
    > maximumMay <=< headMay $ [[8,5,3,9]]
    Just 9

    > maximumMay <=< headMay $ [[],[8,5,3,9]]
    Nothing

    > maximumMay <=< headMay $ []
    Nothing
    ~~~

  * If you want to replace `Maybe` with a default value, you have to either
    use [`fromMaybe`][] or rely on yet another bunch of functions such as
    [`M.findWithDefault`][] or [`lastDef`][].

It's not even that `findWithDefault` is a long name (it is, tho). The real
problem is that too many different functions do the same thing – you can't
really write code without having to constantly consult Hoogle or
documentation. Not only that, but people can't *read* your code that easily,
because they might not have been trained to recognise `findWithDefault`,
`lastDef`, `fromMaybe [] (spoonify tail)`, etc. as conceptually similar
things.

Lens solves this problem by providing a (mostly) uniform vocabulary. Getting
can fail? Use `^?`. Can't fail? `^.`. Possibly can, but you don't care?
`^?!`. Want all elements and not only the first one? `^..`. After that, it's
just a combination of predefined traversals, lenses, prisms, simple functions
used with `to`, and -functions ending in `Of`- (no idea how to call
them). The “what happens in case of a failure” and “how do I get the value”
parts are separated.

## Back to `to`

If you remember, just one section ago we wrote `to` – it “lifts” a function
into a getter, which can be composed with other lenses.

~~~ haskell
to :: (s -> a) -> Lens' s a
to getter = lens getter setter
  where setter = error "to: tried to set a getting-only lens"
~~~

It's not very nice – `Lens'` usually indicates, well, lenses, and not
getters. Liar liar. There should be some type which would indicate that the
resulting “lens” can't be used for setti— a-ha, but we *have* such a type:

~~~ haskell
type Getting r s a = (a -> Const r a) -> s -> Const r s
~~~

And functions which work with `Getting` have types like this:

~~~ haskell
view     :: Getting  a        s a -> s -> a
toListOf :: Getting [a]       s a -> s -> [a]
preview  :: Getting (First a) s a -> s -> Maybe a
has      :: Getting Any       s a -> s -> Bool
~~~

If we want all these functions to work with our `to`-created getter, it has
to have a type which would match all of these:

  * `Getting a s a`
  * `Getting [a] s a`
  * `Getting (First a) s a`
  * `Getting Any s a`

Can some type match all of these? Sure!

~~~ haskell
type Getter s a = forall r . Getting r s a
~~~

So, our better `to` would have this type signature:

~~~ haskell
to :: (s -> a) -> Getter s a
~~~

Implementing `to` is simple:

  * We've got our `getter :: s -> a` and `f :: a -> Const r a`, and `s :: s`:

    ~~~ haskell
    to getter f s = _
    ~~~

  * We use `getter` to extract the `a` from `s`:

    ~~~ haskell
    to getter f s = _ (getter s)
    ~~~

  * Then `f` acts on the extracted `a`:

    ~~~ haskell
    to getter f s = _ (f (getter s))
    ~~~

  * At this point we've got a `Const r a`, but we need `Const r s`. To
    convert, we can just unwrap it and wrap the value back into `Const`:

    ~~~ haskell
    to getter f s = Const (getConst (f (getter s)))
    ~~~

Great, now instead of a runtime exception there'll be a more-or-less nice
compile-time error:

~~~ {.haskell .repl}
> False & to id .~ True

<interactive>:
    Couldn't match type ‘Const r0 Bool’ with ‘Identity Bool’
    Expected type: Setting Bool c Bool Bool
      Actual type: Getting r0 Bool Bool
    In the first argument of ‘(.~)’, namely ‘to id’
    In the second argument of ‘(&)’, namely ‘to id .~ True’
~~~

## Recap

  * `view` makes an “ordinary getting function” out of a lens.

  * `to` makes a getter out of an “ordinary getting function”. A getter is
    something which fits any `Getting r s a`:

    ~~~ haskell
    type Getter s a = forall r . Getting r s a
    ~~~

    In other words, it's a lens-like thing where the functor can be any
    `Const r`:

    ~~~ haskell
    type Getter s a = forall r .   (a -> Const r a) -> s -> Const r s
    type Lens'  s a = Functor f => (a ->    f    a) -> s ->    f    s
    ~~~

  * A getter can be used instead of a lens or a traversal in many places:

    ~~~ {.haskell .repl}
    > -- As an argument to «view» or «^.».
    > [1..3] ^. to reverse
    [3, 2, 1]

    > -- Or as an argument to «toListOf» or «^..».
    > (1, 2, 3) ^.. each . to show
    ["1", "2", "3"]

    > -- Every other function from Control.Lens.Fold works too.
    ~~~

  * `^?!` is like `^?`, but unsafe (it unwraps a `Maybe` and can throw an
    exception):

    ~~~ {.haskell .repl}
    > [] ^? _head
    Nothing

    > [] ^?! _head
    *** Exception: (^?!): empty Fold
    ~~~

# Reader, Writer, State, related stuff

There isn't much left to say about getters (without touching indexed getters,
of course). However, if you browse the [`Control.Lens.Getter`][] module
[like I did when I had thought I was done writing this section], you can
notice that there are 5 functions left unexplained: [`views`][], [`use`][],
[`uses`][], [`listening`][] and [`listenings`][]. What do they do?

To find out, look closely at the function which *isn't* among these 5 –
`view`. Its type signature is:

~~~ haskell
view :: MonadReader s m => Getting a s a -> m a
~~~

However, back in part 1 we defined it with a different signature:

~~~ haskell
view :: Getting a s a -> s -> a
~~~

(by the way, please take a second right now and reimplement `view` without
looking anywhere or querying any types or anything of the sort). What does
[`MonadReader`][] do here? In fact, how come lens's `view` works at all,
after all, we aren't using any monads..?

-----------------------------------------------------------------------------

~~I promised that nothing beyond `Functor`, `Applicative`, `Monoid` and
`Monad` is “required knowledge” for these posts, and “reader” isn't any of
those 4 things. So, I'll take a moment to explain it.~~

The previous paragraph was written more than a month ago, and I guess that
constantly thinking “and now I have to explain reader/writer/state, da-arn”
somehow took all my motivation away. I still don't think there's a good
explanation of reader/writer/state anywhere, but apparently I can't write my
own. Sorry.

Have some links instead!

  * An [explanation of Reader/Writer/State with pictures][adit RWS]. It
    doesn't actually explain much and I don't like it.

  * A [confusing explanation in LYAH][LYAH RWS]. I don't like it either.

  * An [explanation of Reader][maz R] which I'm too lazy to
    evaluate but I think it might be potentially better than the others.

  * [Something which isn't even an explanation][wish RWS] but just code which
    you're supposed to look at and become enlightened. It *does* work
    sometimes, but I can't guarantee anything.

  * [Examples][SO R] of not-exactly-standard Reader usage.

  * An [explanation which is long enough][AAB RWS] to have a chance to be
    useful.

  * I've just noticed that [You Could Have Invented Monads][invented WS]
    deals with Writer and State, so, y'know, maybe you should start with it
    'cause everyone loves this one.

  * This tutorial ([part 1][Mike S1], [part 2][Mike S2]) explains the
    implementation of State monad well.

  * And [this one][Brandon S] is short but to the point.

  * [This one][wiki S] has examples.

[adit RWS]: http://adit.io/posts/2013-06-10-three-useful-monads.html
[LYAH RWS]: http://learnyouahaskell.com/for-a-few-monads-more
[maz R]: http://www.maztravel.com/haskell/readerMonad.html
[wish RWS]: http://www.stephendiehl.com/what/#reader-monad
[AAB RWS]: https://www.haskell.org/haskellwiki/All_About_Monads#The_State_monad
[invented WS]: http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
[Mike S1]: http://mvanier.livejournal.com/5406.html
[Mike S2]: http://mvanier.livejournal.com/5846.html
[Brandon S]: http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
[wiki S]: http://en.wikibooks.org/wiki/Haskell/Understanding_monads/State
[SO R]: http://stackoverflow.com/a/14179721/615030

## `view` (and `preview`)

Remember that `view` makes a function out of a lens/traversal/getter – which
means that with its help we can use lenses and getters when reading just as
we use ordinary functions. Here's a contrived example showing how [`swap`][]
can be written Reader-style:

~~~ haskell
ex3 :: (a, b) -> (b, a)
ex3 = do
  a <- fst
  b <- snd
  return (b, a)
~~~

Or with `view` (because `view _1` is the same as `fst`):

~~~ haskell
ex3' :: (a, b) -> (b, a)
ex3' = do
  a <- view _1
  b <- view _2
  return (b, a)
~~~

By the way, you can use `preview` in the same way (it'll just return a
`Maybe`). I won't give an example because, honestly, I dislike giving
completely contrived examples and I can't think of a good one (“let's define
a huge thing which depends on some configuration which is a record with lots
of fields which have other fields” doesn't count as a particularly good
example).

Bonus task: figure out what [`pre`][] does, why `preview = view . pre`, and
write it.

## So, why `MonadReader`?

Because `x -> a` isn't the only reader monad. There's also [`Reader`][],
which is simply a `newtype` over `x -> a`:

~~~ haskell
newtype Reader r a = Reader
  { runReader :: r -> a }
~~~

And then there's [`ReaderT`][], which is a monad transformer – it can be used
to turn other monads into reader monads.

So, we make `view` work in any reader monad so that you wouldn't have to use
`asks (view ...)`. At this point I could rant about how maybe *every* function
should be made work in any reader monad – like, why not `length` too? if you
want to abstract over such a thing as Getting A Function Argument, go on,
abstract everywhere! – but I'm willing to accept that it's just for
convenience. Ahem.

Old implementation:

~~~ haskell
view :: Getting a s a -> s -> a
view l = getConst . l Const
~~~

New implementation:

~~~ haskell
import Control.Monad.Reader

view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst . l Const)
~~~

## `views`

There's also [`views`][], which is to `view` what [`asks`][] is to
[`ask`][]. That is, it takes a getter *and* a function to apply to the value
returned by the getter. So, if there's a config like `Config`, which contains
field `name` (well, let's say the field is called `_name` and `name` is the
name of the corresponding lens), and you want to get length of said field –
in some reader monad – you have at least 6 slightly different ways:

  1. Straightforward – `length . view name <$> ask`.
  2. With `asks` – `asks (length . view name)`.
  3. Moving `length` into getter – `asks (^. name . to length)`.
  4. Using `view` as `asks` – `length <$> view name`.
  5. Again, moving `length` into getter – `view $ name . to length`
  6. With `views` – `views name length`.

I think #4 looks the best *in this particular case*, but choose for yourself.

By the way, what is the type of `views`? You might think it's

~~~ haskell
views :: MonadReader s m => Getter s a -> (a -> r) -> m r
~~~

but it's not – remember that `Getter s a` is the same as `Getting x s a`
which has to work for any `x`, and we surely don't need *any* `x`, right? We
only need some specific `x`.

What about `Getting a s a`? It would've made sense – with `Getting a s a` we
can get `a` out of an `s`, and then simply apply the `a -> r` function to
it. Like this:

~~~ haskell
views :: MonadReader s m => Getting a s a -> (a -> r) -> m r
views l f = f <$> view l
~~~

However, it's not the right answer either.

The right answer is this:

~~~ haskell
views :: MonadReader s m => Getting r s a -> (a -> r) -> m r
views l f = view (l . to f)
-- Or “asks (getConst . l (Const . f))”.
~~~

Can you answer – without thinking much – why `l` is required to have type
`Getting r s a` in the final definition of `views`? I couldn't – so here we
go, yet another bullet list:

  * `Getting r s a` is the same as `(a -> Const r a) -> s -> Const r s`. It
    should be read not as “gets `a` from `s`”, or “gets `r` from `s`”, but
    “gets `r` from `s`, provided that you know how to get `r` from `a`”. (And
    since `views` is given exactly that – a function from `a` to `r` – the
    question is already answered.)

  * In other words, in `Getting r s a`, `a` isn't *result* type. It's
    *intermediate* type.

  * Now it should (probably) be crystal clear to you that if `f` has type
    `Getting r abc ab` and `g` has type `Getting r ab a`, then `f . g` has
    type `Getting r abc a`. If one `r` was different from another `r`, it
    would've been impossible to compose 2 `Getting`s.

  * Update from a month later: I'm rereading it now and it wasn't “crystal
    clear” to me until I reminded myself that `Getting` is a -function which
    modifies a getter- and these can be naturally chained. Lesson: never,
    never stop writing in the middle of a post for a month.

  * In `views`, our final goal is `r`. Since `to f` can be getting anything

    ~~~ haskell
    to   :: (a -> r) -> (forall x . Getting x a r)
    to f :: forall x . Getting x a r
    ~~~

    it's enough for `l` to be getting `r`, which is indeed what we require by
    saying `Getting r s a`.

**Open question:** it's impossible to write a function of the type `(x -> y)
-> Getting x s a -> Getting y s a`, but it's possible to write a function of
the type `(r -> r) -> Getting r s a -> Getting r s a`, which would do pretty
much what you expect. Can it be of any use?

**Open question:** are there non-contrived situations where it would matter
whether `views` takes `Getting r s a` or `Getting a s a`?

## Bonus

A reader-style version of ~~quicksort~~ [deforested tree sort][Reddit qs]
left from my attempted explanation of reader:

~~~ haskell
sort' = do
  empty <- null
  if empty then id else do
    pivot <- head
    local tail $ do
      left  <- local (filter (<  pivot)) sort'
      right <- local (filter (>= pivot)) sort'
      return (left ++ pivot : right)
~~~

[Reddit qs]: http://www.reddit.com/r/programming/comments/2h0j2/real_quicksort_in_haskell/c2h196

## [`listening`][] and [`listenings`][]

I haven't ever seen anybody use them, so I guess I'll just skip them. It's not that hard to figure out what they do by looking at the signatures and implementation, anyway:

~~~ haskell
-- | This is a generalized form of 'listen' that only extracts the portion
-- of the log that is focused on by a 'Getter'.
listening :: MonadWriter w m => Getting u w u -> m a -> m (a, u)
listening l m = do
  (a, w) <- listen m
  return (a, view l w)

listenings :: MonadWriter w m => Getting v w u -> (u -> v) -> m a -> m (a, v)
listenings l uv m = do
  (a, w) <- listen m
  return (a, views l uv w)
~~~

# Actions 101

And now for something completely different! I'm tired of sounding like I know
what I'm talking about, which means that right now I'll tell you what I
*don't* know, and then we'll try to unravel everything toge— well, I'll be
unravelling things and imagining that you're sitting with me in this room and
asking questions or participating or something.

[Ye-eah, it was another I'm-bored-at-night-so-why-not-be-a-bit-awkward
paragraph.]

Take a look at the [`Control.Lens.Action`][] module (that's what I'm doing
right now) (note that you need the [lens-action](@hackage) package for it). What questions could you ask?

-----------------------------------------------------------------------------

> ~~~ haskell
> type Action m s a = forall f r. Effective m r f => (a -> f a) -> s -> f s
> ~~~

What's `Effective`?

-----------------------------------------------------------------------------

> An `Action` is a `Getter` enriched with access to a `Monad` for
> side-effects.

Should `putStrLn` (which prints given value but returns `()`) be made into an
`Action`? What are canonical uses of actions? What interesting actions can
there be using other monads? Are all monads allowed, or maybe not?

-----------------------------------------------------------------------------

> Every `Getter` can be used as an `Action`.

Remember that

~~~ haskell
type Getter s a = forall r . (a -> Const r a) -> s -> Const r a
~~~

How does this unify with `Effective m r f => (a -> f a) -> s -> f s`? Is
there an instance of `Effective` for `Const` or something?

-----------------------------------------------------------------------------

> ~~~ haskell
> act :: Monad m => (s -> m a) -> Action m s a
> act sma afb a = effective (sma a >>= ineffective . afb)
> ~~~

What do `effective` and `ineffective` do?

-----------------------------------------------------------------------------

> `acts`: a self-running `Action`, analogous to `join`.
>
> ~~~ haskell
> acts ≡ act id
> ~~~

What can this be useful for?

-----------------------------------------------------------------------------

> `liftAct`: apply a `Monad` transformer to an `Action`.

Again, need examples of usage.

-----------------------------------------------------------------------------

*[Skipping indexed stuff and monadic folds.]*

> ~~~ haskell
> type Acting m r s a = LensLike (Effect m r) s s a a
> ~~~
>
> Used to evaluate an `Action`.

`Acting` to `Action` is the same as `Getting` to `Getter`, right?

-----------------------------------------------------------------------------

> ~~~ haskell
> class (Monad m, Functor f, Contravariant f) => Effective m r f | f -> m r
> ~~~
>
> An `Effective` `Functor` ignores its argument and is isomorphic to a
> `Monad` wrapped around a value.

Ignores its argument... Um, like `Const`? Is `Const (m r)` an `Effective`?

-----------------------------------------------------------------------------

> Instances:
>
>   * `Effective Identity r (Const r)`
>   * `Monad m => Effective m r (Effect m r)`

What's `Effect`? What if `Effect m r` is the same as `Const (m r)`? And
what's the `Identity` instance for?

## `Action` and `Effect`

Let's say we're only given the definition:

> An `Action` is a `Getter` enriched with access to a `Monad` for
> side-effects.

Can we deduce everything else from it? I think we can.

A getter is conceptually the same as `s -> a`:

~~~ haskell
type Getter s a = s -> a
~~~

In this spirit, an action is, logically:

~~~ haskell
type Action m s a = s -> m a
-- Where “m” is a Monad.
~~~

However, the actual `Getter` is turned around, like this:

~~~ haskell
type Getter s a = forall r . (a -> r) -> s -> r
~~~

We can turn around `Action` in the same way:

~~~ haskell
type Action m s a = forall r . (a -> m r) -> s -> m r
~~~

And since `Getter` uses `Const r` instead of `r` to be compatible with
lenses...

~~~ haskell
type Getter s a = forall r . (a -> Const r a) -> s -> Const r s
~~~

...`Action` should too.

~~~ haskell
type Action m s a = forall r . (a -> Const (m r) a) -> s -> Const (m r) s
~~~

The final step is giving a name for `Const (m r)` (guess what it's going to
be):

~~~ haskell
type Effect m r = Const (m r)

type Action m s a = forall r . (a -> Effect m r a) -> s -> Effect m r s
~~~

No, wait, this is not the final step. The final step would be adding
`Acting`, to achieve the symmetry with `Getter`/`Getting`.

~~~ haskell
type Getting r s a = (a -> Const r a) -> s -> Const r s

type Getter s a = forall r . Getting r s a
~~~

(Just reminding you that `Getter` is to show that a function *is* a getter,
and `Getting` is for functions which need to *accept* a getter as an
argument.)

~~~ haskell
type Acting m r s a = (a -> Effect m r a) -> s -> Effect m r s
-- Or “LensLike (Effect m r) s s a a”.

type Action m s a = forall r . Acting m r s a
~~~

## Making actions

Since `Effect m r` is just `Const (m r)`, we can actually represent `Acting`
as `Getting`:

~~~ haskell
type Getting  r  s a = (a -> Const   r   a) -> s -> Const   r   s
type Acting  m r s a = (a -> Const (m r) a) -> s -> Const (m r) s

-- From which follows...

type Acting m r s a = Getting (m r) s a
~~~

However, we can't represent `Action` as `Getter`, and thus can't make actions
with `to`. We need a separate combinator for this (which I'll call `act`
because in lens it's called `act`):

~~~ haskell
act :: Monad m => (s -> m a) -> Action m s a
~~~

...“implementing `act` is left as an exercise for the reader”, yeah.

## Using actions

What is to `^.` as `Acting` is to `Getting`? Let's call it `^!` and implement
it.

~~~ haskell
(^.) :: s -> Getting a s a -> a

(^!) :: s -> Acting m a s a -> m a
--   :: s -> Getting (m a) s a -> m a

infixr 8 ^!
~~~

Again, left as an exercise or something.

Finally, let's define `perform` just because `^.` has `view` and `^!` wants
to have something too (and also because it's sometimes better to use a
function than an operator):

~~~ haskell
perform = flip (^!)
~~~

With `^!` and `act`, we can write *extremely cool stuff*, like hello world:

~~~ {.haskell .repl}
> "Hello, world!" ^! act putStrLn
Hello, world!
~~~

## Actions and traversals

We probably can do something like `each . act putStrLn`. Well, we *should* be
able to, at least, because otherwise actions are just useless. So, let's try:

~~~ {.haskell .repl}
> ["a", "b", "c"] ^! each . act putStrLn

<interactive>:
    No instance for (Monoid (IO ())) arising from a use of ‘each’
    In the first argument of ‘(.)’, namely ‘each’
    In the second argument of ‘(^!)’, namely ‘each . act putStrLn’
    In the expression: ["a", "b", "c"] ^! each . act putStrLn
~~~

Ouch. What's going o— a-ha, right, the `Monoid` trouble from the past strikes
again – `each` is a traversal, `each` uses `Applicative`, and the
`Applicative` instance for `Const` (which we're using) requires a
`Monoid`. There's no general instance like

~~~ haskell
instance (Monad m, Monoid a) => Monoid (m a)
~~~

for [various reasons][Monoid Monad]; if we want `Effect`s to combine, we'll
have to resort to newtypes. Behold, the *actual* definition of `Effect`:

[Monoid Monad]: https://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg24485.html

~~~ haskell
-- | Wrap a monadic effect with a phantom type argument.
newtype Effect m r a = Effect { getEffect :: m r }

instance Functor (Effect m r) where
  fmap _ (Effect m) = Effect m

instance (Monad m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (return mempty)
  Effect ma <*> Effect mb = Effect (liftM2 mappend ma mb)
~~~

(Now you'll have to make corresponding changes to `^!` and `act`.)

Just in case, I'll spell out what's going on here:

  * Traversals need an `Applicative` instance to combine effects.

  * For `Const`, the `Applicative` instance requires `Monoid`:

    ~~~ haskell
    instance Monoid r => Applicative (Const r)
    ~~~

  * If `Effect` was the same as `Const`, a `Monoid` instance for `m r`
    would've been needed.

  * However, we don't want `Effect`s to be combined in the same way as
    `Const`s are combined. With additional knowledge of `m` being a `Monad`,
    we want monadic composition instead of pure monoidal composition.

  * So, we make `Effect` a newtype, with a different `Applicative` instance –
     previously `m r` was required to be a `Monoid`, but now only `r` has to
     be a `Monoid` (given that `m` is a `Monad`).

With this new definition, will the original `each` thing work? Sure:

~~~ {.haskell .repl}
> ["a", "b", "c"] ^! each . act putStrLn
a
b
c
~~~

The `Monoid` used here is `()`, but any other could be used as well:

~~~ {.haskell .repl}
> -- A helper for generating a random string.
> let rand n = (++ ".") <$> sample (replicateM n (uniform 'a' 'z'))
> forM [1..5] rand
["t.","dz.","ouq.","uzzz.","aimwn."]

> -- Let's create 10 files containing random strings:
> forM_ [1..10] $ \i -> writeFile ("/tmp/rand" ++ show i) =<< rand i

> -- Just in case, let's also check they exist:
> :! du -b /tmp/rand*
2	/tmp/rand1
11	/tmp/rand10
3	/tmp/rand2
4	/tmp/rand3
5	/tmp/rand4
6	/tmp/rand5
7	/tmp/rand6
8	/tmp/rand7
9	/tmp/rand8
10	/tmp/rand9

> -- Okay, now we'll read them all and concat their contents.
> let files = ["/tmp/rand" ++ show i | i <- [1..10]]
> files ^! each . act readFile
"c.rq.vsy.ikia.hmsjs.emjqoh.mpmyuse.gqpxriey.rqxtbqdng.yegadcnfqj."
~~~

(The instance in this case is `Monoid [a]`, since `readFile` returns a
`String`.)

## Actions and getters

In the last example, you might've noticed that the definition of `files`
could've been combined with the action, like this:

~~~ haskell
[1..10] ^! each . to (\i -> "/tmp/rand" ++ show i) . act readFile
~~~

Well, for one, it's clumsy and you shouldn't do it. However, it also doesn't
typecheck (spoiler: yet!). Observe:

~~~ {.haskell .repl}
> [1..10] ^! each . to (\i -> "/tmp/rand" ++ show i) . act readFile

<interactive>:
    Couldn't match type ‘Const r0 t0’ with ‘Effect IO String t0’
    Expected type: ([Char] -> Const r0 [Char])
                   -> t0 -> Effect IO String t0
      Actual type: Getting r0 t0 [Char]
    In the first argument of ‘(.)’, namely
      ‘to (\ i -> "/tmp/rand" ++ show i)’
    In the second argument of ‘(.)’, namely
      ‘to (\ i -> "/tmp/rand" ++ show i) . act readFile’

<interactive>:
    Couldn't match type ‘Effect IO String [Char]’
                  with ‘Const r0 [Char]’
    Expected type: (String -> Effect IO String String)
                   -> [Char] -> Const r0 [Char]
      Actual type: Acting IO String [Char] String
    In the second argument of ‘(.)’, namely ‘act readFile’
    In the second argument of ‘(.)’, namely
      ‘to (\ i -> "/tmp/rand" ++ show i) . act readFile’
~~~

Um, no, don't observe. Better try something simpler:

~~~ {.haskell .repl}
> :t to show . act putStrLn

<interactive>:
    Couldn't match type ‘Effect IO r1 String’ with ‘Const r String’
    Expected type: (() -> Effect IO r1 ()) -> String -> Const r String
      Actual type: Acting IO r1 String ()
    In the second argument of ‘(.)’, namely ‘act putStrLn’
    In the expression: to show . act putStrLn
~~~

A-ha, now that's tolerable. So, what's the problem?

The problem is actually very simple: if `Effect` and `Const` are no longer
the same thing (and we made sure they aren't), we can't unify them:

~~~ {.haskell .repl}
> -- This is type of “to show”:
> :t to show
to show :: Show s => Getting r s String

> -- And now expanded:
> :kind! Show s => Getting r s String
Show s => Getting r s String :: *
= forall r s.
  Show s =>
  ([Char] -> Const r [Char]) -> s -> Const r s

> -- Type of “act putStrLn”:
> :t act putStrLn
act putStrLn :: Acting IO r String ()

> -- Expanded:
> :kind! forall r . Acting IO r String ()
forall r . Acting IO r String () :: *
= forall r. (() -> Effect IO r ()) -> [Char] -> Effect IO r [Char]
~~~

So, in order for `to show . act putStrLn` to work, the result of `act
putStrLn` must be the same as the argument of `to show`:

~~~ haskell
“act putStrLn” (result)   : [Char] -> Effect IO r [Char]
“to show”      (argument) : [Char] -> Const r [Char]
~~~

## What to do now?

A possible solution is to create a class for `Const`-like functors, and make
getters and actions accept any of those functors instead of `Const` and
`Effect` specifically. Turns out, however, that we don't need a special class
for `Const`-like functors – it exists already as a combination of 2 other
classes, `Functor` and [`Contravariant`][] (originally defined in
[contravariant](@hackage)).

`Contravariant` is a class with a single method:

~~~ haskell
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
~~~

Compare with `Functor`:

~~~ haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
~~~

There aren't many interesting instances of `Contravariant`. Functions
could've been one – you can “reverse-apply” a function to the *argument* of
another function – but due to the lack of type-level lambdas they can't, so
you have to resort to newtypes:

~~~ haskell
newtype Op b a = Op (a -> b)

instance Contravariant (Op b) where
  contramap g (Op f) = Op (f . g)

-- contramap :: (x -> y) -> Op b y -> Op b x

-- Or without newtypes:
-- contramap :: (x -> y) -> (y -> b) -> (x -> b)
~~~

Or you can focus on some specific functions – e.g. on comparisons:

~~~ haskell
newtype Comparison a = Comparison (a -> a -> Ordering)

instance Contravariant Comparison where
  contramap g (Comparison comp) = Comparison (comp `on` g)
~~~

`Const` is a `Contravariant` too:

~~~ haskell
instance Contravariant (Const a) where
  contramap f (Const x) = Const x
~~~

Now, the interesting thing is that you can prove that *any* `Functor` which
is also `Contravariant` doesn't actually contain any values – that is, it's
like `Const`. How to prove it? By writing the `coerce` function:

~~~ haskell
coerce :: (Functor f, Contravariant f) => f a -> f b
~~~

You can't get N values of type `b` out of thin air unless N is 0, so the
existence of such function would prove it.

Don't look ahead before trying to implement `coerce` on your own. Mindless
brute-force search -of a composition of a small number of functions which has
the type you want- is fun!

-----------------------------------------------------------------------------

We-ell, maybe not *that* mindless... anyway,

~~~ haskell
coerce :: (Functor f, Contravariant f) => f a -> f b
coerce = contramap (const ()) . fmap (const ())

-- fmap      (const ()) :: Functor       f => f a  -> f ()
-- contramap (const ()) :: Contravariant f => f () -> f a
~~~

The [definition][`coerce` def] in lens uses [`absurd`][] instead, and I feel
obliged to explain it as well:

  * `Void` is an uninhabited type – it has no values (`()` has 1). With
    recent enough GHC, you can implement `Void` as simply `data Void`, but in
    [void](@hackage) it's implemented as `newtype Void = Void Void` (newtypes
    are strict, so you can't construct `Void` as `let x = Void x in x` or
    something).

  * Since `Void` can't exist, it's safe to have a function like `absurd ::
    Void -> a`.

  * From there it's 1 step to `vacuous :: Functor f => f Void -> f a` (simply
    do `fmap absurd`).

  * And, and then we can use `contramap absurd` to get an “opposite” function
    of type `f a -> f Void` – which can be finally combined with `vacuous` to
    get `f a -> f b`.

-----------------------------------------------------------------------------

Do you still remember what we wanted to do? We wanted to find a way to show
that a `Functor` is `Const`-like – i.e. it doesn't hold any values – and we
found that putting an additional `Contravariant` constraint on it is
enough. Now the `Getter` definition can be updated:

~~~ haskell
type Getter s a =
  forall f . (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- Old:
--   type Getter s a = forall r . (a -> Const r a) -> s -> Const r s
~~~

As well as the definition of `to`:

~~~ haskell
to :: (s -> a) -> Getter s a
to getter f s = coerce (f (getter s))

-- Old:
--   to getter f s = Const (getConst (f (getter s)))
~~~

The next step is adding an instance of `Contravariant` for `Effect`, which is
the same as the `Functor` instance:

~~~ haskell
instance Contravariant (Effect m r) where
  contramap _ (Effect m) = Effect m
~~~

This is enough for `to show . act putStrLn` to work:

~~~ {.haskell .repl}
> True ^! to show . act putStrLn
True
~~~

## What about the rest of the questions?

Umh. What if I just asked Whoever Happens To Read This Post to tell me the
answers? I admit that I haven't ever actually used actions, which makes it
pretty hard for me to invent usecases for them. So, what I'm looking for is
any snippets of code which use actions, `liftAct`, `acts`, etc. (and it'd be
kinda nice if you could send me them if you have any).

As for `Effective`... I suspect that it's needed when you start implementing
“indexed stuff”, but I can't be sure because I'm actively trying not to learn
anything about it before time comes to write about it.

# `Getter` and `Setter` laws

Getters have no laws, because they're just functions in disguise.

> ~~~ haskell
> set l y (set l x a) ≡ set l y a
> ~~~
>
> You can't view a `Setter` in general, so the other two laws are irrelevant.
>
> However, two `Functor` laws apply to a `Setter`:
>
> ~~~ haskell
> over l id ≡ id
> over l f . over l g ≡ over l (f . g)
> ~~~


[`Compose`]: http://hackage.haskell.org/package/transformers/docs/Data-Functor-Compose.html#v:Compose
[`Category`]: http://hackage.haskell.org/package/base/docs/Control-Category.html#t:Category
[`traverse`]: http://hackage.haskell.org/package/base/docs/Data-Traversable.html#v:traverse
[`M.lookup`]: http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html#v:lookup
[`M.findWithDefault`]: http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html#v:findWithDefault
[`listToMaybe`]: http://hackage.haskell.org/package/base/docs/Data-Maybe.html#v:listToMaybe
[`fromMaybe`]: http://hackage.haskell.org/package/base/docs/Data-Maybe.html#v:fromMaybe
[`maximumMay`]: http://hackage.haskell.org/package/safe/docs/Safe.html#v:maximumMay
[`lastDef`]: http://hackage.haskell.org/package/safe/docs/Safe.html#v:lastDef
[`teaspoon`]: http://hackage.haskell.org/package/spoon/docs/Control-Spoon.html#v:teaspoon
[`<=<`]: http://hackage.haskell.org/package/base/docs/Control-Monad.html#v:-60--61--60-
[`Control.Lens.Getter`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html
[`Reader`]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader.html#t:Reader
[`MonadReader`]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader.html#t:MonadReader
[`MonadWriter`]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-Writer.html#t:MonadWriter
[`swap`]: http://hackage.haskell.org/package/base/docs/Data-Tuple.html#v:swap
[`views`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:views
[`listening`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:listening
[`listenings`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:listenings
[`ask`]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader-Class.html#v:ask
[`asks`]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader-Class.html#v:asks
[`listen`]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-Writer-Class.html#v:listen
[`use`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:use
[`uses`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:uses
[`sequence`]: http://hackage.haskell.org/package/base/docs/Control-Monad.html#v:sequence
[`worded`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:worded
[`Writer`]: http://hackage.haskell.org/package/transformers/docs/src/Control-Monad-Trans-Writer-Lazy.html#Writer
[`group`]: http://hackage.haskell.org/package/base/docs/Data-List.html#v:group
[`Control.Lens.Action`]: http://hackage.haskell.org/package/lens-action/docs/Control-Lens-Action.html
[`Effect`]: http://hackage.haskell.org/package/lens-action/docs/Control-Lens-Internal-Action.html#t:Effect
[`Contravariant`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#t:Contravariant
[`coerce` def]: http://hackage.haskell.org/package/lens/docs/src/Control-Lens-Internal-Getter.html#coerce
[`absurd`]: http://hackage.haskell.org/package/void/docs/Data-Void.html#v:absurd
[`ignored`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Traversal.html#v:ignored
[`taking`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Traversal.html#v:taking
[`dropping`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Traversal.html#v:dropping
[`failing`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Traversal.html#v:failing
[`pre`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:pre
[`elementsOf`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Traversal.html#v:elementsOf
[`ReaderT`]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader.html#t:ReaderT

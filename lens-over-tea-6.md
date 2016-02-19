% lens over tea #6: Template Haskell

---
series:
  top: “lens over tea”
  toplink: /#lens-over-tea
  prev: /lens-over-tea-5
---

Let's take a break from weird typeclasses and delve into [Template Haskell](@w) that lens uses fairly extensively; in the end we'll have a simple implementation of `makeLenses`. This post is fairly standalone and you don't have to have read the previous 5 posts – in fact, you don't even need to know any TH. The only thing you need to know that Template Haskell is a way to generate code while your program is compiling.

And before we begin, some funny quotes:

~~~
monochrom: Lens over Tea is really long. this is Lens over A Feast
           Spanning 5 Days
    ReinH: Or "Lens Over Teas"
    ReinH: Speaking of which, I should drink some tea.
monochrom: and each day the fabled Italian dinner which lasts 3 hours
           and 10 courses or something
monochrom: unless you're like the minister of education of Hong Kong.
           he claims he reads 30 books every month
~~~

~~~
Gurkenglas: The masterpiece of an article linked above, "lens over
            tea", whose writing style (sacrificing "ease for some to
            understand what you are talking about" for appeal to those
            who would write the same way if they sacrificed the same)
            quickly rubbed off on me, case in point.
~~~

# Functions in lens that use Template Haskell

Or “know your enemy”. If you already know what `makeLenses` etc do, you can [skip this part][Template Haskell] and start reading about Template Haskell itself.

(I'm not going to mention `makeClasses` and `makeWrapped` because they aren't used often, but you should still be aware that I haven't mentioned everything there was to mention.)

## [`makeLenses`][]

Lens uses TH in order to provide [`makeLenses`][] (and related functions), which you can use to automatically generate lenses for your types. Let's see it in action:

~~~ haskell
{-# LANGUAGE TemplateHaskell #-}

module Test where  -- would be needed later

import Control.Lens

data Person = Person {
  _name :: String,          -- the underscores show for which
  _age :: Double }          -- fields the lenses should be created

makeLenses ''Person
~~~

~~~ {.haskell .repl}
> :t name
name :: Functor f => (String -> f String) -> Person -> f Person

-- Or alternatively,
--   name :: Lens' Person String
~~~

Here's what happened. [`makeLenses`][] is a function that takes a name of a datatype and produces some code.

`''Person` is a special syntax enabled by `{-# LANGUAGE TemplateHaskell #-}`. `''` prepended to any type or class turns it into `Name`:

  * `Maybe` – a type constructor
  * `"Maybe"` – a string
  * `''Maybe` – a name (behaves similarly to a string but is guaranteed to refer to something in the code)

The produced code is “inserted” into the file during complation (it doesn't actually get written into the file, just treated as if it was there). In this case, the code looks like this:

~~~ haskell
age :: Lens' Person Double
age f (Person x1 x2) = fmap (\y -> Person x1 y) (f x2)
{-# INLINE age #-}

name :: Lens' Person String
name f (Person x1 x2) = fmap (\y -> Person y x2) (f x1)
{-# INLINE name #-}
~~~

You can see the produced code by compiling your program with `-ddump-splices` (that's what we needed `module Test` for – otherwise GHC would've tried to compile it as a program and we'd have to add a `main` action):

~~~
> ghc -ddump-splices th.hs

[1 of 1] Compiling Test             ( th.hs, th.o )
th.hs:14:1-19: Splicing declarations
    makeLenses ''Person
  ======>
    age :: Lens' Person Double
    age f_a6gx (Person x_a6gy x_a6gz)
      = fmap (\ y_a6gA -> Person x_a6gy y_a6gA) (f_a6gx x_a6gz)
    {-# INLINE age #-}
    name :: Lens' Person String
    name f_a6gB (Person x_a6gC x_a6gD)
      = fmap (\ y_a6gE -> Person y_a6gE x_a6gD) (f_a6gB x_a6gC)
    {-# INLINE name #-}
~~~

(The names of variables look like this because GHC likes making all names unique.)

## [`makeLensesFor`][]

There are other functions available, too. [`makeLensesFor`][] is like `makeLenses` but lets you name lenses differently:

~~~ haskell
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Lens

data Person = Person {
  name :: String,
  age :: Double }

makeLensesFor [("name", "nameLens")] ''Person
~~~

~~~ haskell
nameLens :: Lens' Person String
nameLens f (Person x1 x2) = fmap (\y -> Person y x2) (f x1)
{-# INLINE nameLens #-}
~~~

## [`makeFields`][]

[`makeFields`][] additionally turns generated lenses into class methods (which means that now you can have records with same-named fields... well, sort of):

~~~ haskell
{-# LANGUAGE
TemplateHaskell,
MultiParamTypeClasses,     -- all these extensions are needed
FunctionalDependencies,    -- for generated instances
FlexibleInstances
  #-}

module Test where

import Control.Lens

-- import Language.Haskell.TH
-- import Language.Haskell.TH.Ppr

data Person = Person {
  _personName :: String,
  _personAge :: Double }

data Animal = Animal {
  _animalSpecies :: String,
  _animalName :: Maybe String,
  _animalAge :: Double }

makeFields ''Person
makeFields ''Animal
~~~

Before showing you generated code, here's an example of usage:

~~~ {.haskell .repl}
> Person "Donald" 11 ^. name
"Donald"

> Animal "lion" Nothing 4 ^. name
Nothing

> :t name
name :: (Functor f, HasName s a) => (a -> f a) -> s -> f s
~~~

And now the code:

~~~ haskell
class HasAge s a | s -> a where
  age :: Lens' s a

instance HasAge Person Double where
  age f (Person x1 x2) = fmap (\y -> Person x1 y) (f x2)
  {-# INLINE age #-}

instance HasAge Animal Double where
  age f (Animal x1 x2 x3) = fmap (\y -> Animal x1 x2 y) (f x3)
  {-# INLINE age #-}

--------------------

class HasName s a | s -> a where
  name :: Lens' s a

instance HasName Person String where
  name f (Person x1 x2) = fmap (\y -> Person y x2) (f x1)
  {-# INLINE name #-}

instance HasName Animal (Maybe String) where
  name f (Animal x1 x2 x3) = fmap (\y -> Animal x1 y x3) (f x2)
  {-# INLINE name #-}

--------------------

class HasSpecies s a | s -> a where
  species :: Lens' s a

instance HasSpecies Animal String where
  species f (Animal x1 x2 x3) = fmap (\y -> Animal y x2 x3) (f x1)
  {-# INLINE species #-}
~~~

## [`makePrisms`][]

Next is [`makePrisms`][], which generates prisms for sum types (while `makeLenses` generates lenses for product types):

~~~ haskell
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Lens

data Foobar a
  = Foo a
  | Bar Int Char
  deriving Show

makePrisms ''Foobar
~~~

Again, an example first:

~~~ {.haskell .repl}
> Bar 3 'a' ^? _Foo
Nothing

> Bar 3 'a' ^? _Bar
Just (3,'a')

> Bar 3 'a' ^? _Bar._1
Just 3

> _Foo # False
Foo False
~~~

And now generated code:

~~~ haskell
-- prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b

_Foo :: Prism (Foobar a) (Foobar b) a b
_Foo = prism (\x -> Foo x)
             (\x -> case x of
                      Foo y -> Right y
                      Bar y1 y2 -> Left (Bar y1 y2))

_Bar :: Prism' (Foobar a) (Int, Char)
_Bar = prism
         (\(x1, x2) -> Bar x1 x2)
         (\x -> case x of
                  Bar y1 y2 -> Right (y1, y2)
                  _ -> Left x)
~~~

## [`makeLensesWith`][]

`makeLenses`, `makeLensesFor`, and `makeFields` all call [`makeLensesWith`][] under the hood. It takes a record with settings, and produces lenses according to those settings. (The settings include “whether or not make classes”, “how to call resulting lenses”, etc.) We'll look at those settings later; you can see the list of them [here][`LensRules`] (scroll down a bit, to the “configuration accessors” section).

## [`declareLenses`][]

[`declareLenses`][] lets you make lenses for a record without creating record accessors (i.e. underscored fields). To do it, you have to pass the whole *declaration* to the function:

~~~ haskell
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Lens

declareLenses [d|
  data Person = Person {
    name :: String,
    age :: Double }
  |]
~~~

Similarly to how `''Person` isn't a type but a `Name`, any code in `[| |]` brackets isn't code, but *representation* of that code. `declareLenses` inspects that representation, generates lenses based on it, changes it, and the result is added to the file. In this case, the result is this:

~~~ haskell
data Person = Person String Double

name :: Lens' Person String
name f (Person x1 x2) = fmap (\y -> Person y x2) (f x1)
{-# INLINE name #-}

age :: Lens' Person Double
age f (Person x1 x2) = fmap (\y -> Person x1 y) (f x2)
{-# INLINE age #-}
~~~

(The drawback of `declareLenses` in comparison to `makeLenses` is that now in order to construct `Person` you have to write `Person x y` instead of more understandable and less error-prone `Person {_name = x, _age = y}`.)

# Template Haskell

Any Haskell code can be represented as a *value* of one of the types from [template-haskell](@hackage) (specifically, from the [`Language.Haskell.TH`][] module). There are separate types for expressions, for patterns, for declarations, for types, and for other things.

For a while this piece of code is going to be our case study:

~~~ haskell
map2 :: (a -> b) -> [a] -> [b]
map2 f (x:xs) = f x : map2 f xs
map2 _ [] = []
~~~

`[]` and `f x : map2 f xs` are expressions, `f`, `_`, `[]`, and `(x:xs)` are
patterns, `(a -> b) -> [a] -> [b]` is a type, and the whole thing is a
declaration (well, actually 2 declarations – the type signature and the
function itself).

## Expressions

Let's start with expressions, because I like them most – they're doing all the work and the rest is just supporting cast. Expressions are represented by the type [`Exp`][]:

~~~ haskell
data Exp
  = VarE Name
  | ConE Name
  | AppE Exp Exp
  | InfixE (Maybe Exp) Exp (Maybe Exp)
  | LitE Lit
  ...
~~~

  * `VarE` is any variable or function name: `f`, `x`, `xs`, and `map2`

  * `ConE` is any constructor: `:` and `[]`

  * `AppE` is application of a function to a value: `f x` and `map2 f xs`

  * `InfixE` is used for operators and operator sections: the middle `Exp` is the operator, and `Maybe Exp`s to the left and right of it are arguments; in case of a section one of them would be `Nothing`. In our example it's `f x : map2 f xs`

  * `LitE` is a literal (string, character, or a number)

To create new `Name`s, you can use [`mkName`][]:

~~~ haskell
mkName :: String -> Name
~~~

To refer to already existing things in scope, use `'`. For instance, `'id` refers to `id` from `Prelude`, and `'(:)` refers to the list constructor.

Okay, now you know everything you need to write `f x : map2 f xs` as an `Exp`. (Yeah, this is an exercise.) If you've done everything correctly, you should be able to use [`pprint`][] (short for “pretty-print”) on your expression to see the code:

~~~ {.haskell .repl}
> pprint $ ...
"f x GHC.Types.: map2 f xs"
~~~

(The reason you're oing to see `GHC.Types.:` instead of `:` is that all names generated by TH (excluding the ones defined in the current module) are qualified, and `:` is exported by `Prelude` but originally it's from [`GHC.Types`][].)

Okay, so, have you done it?

The result should look like this if you hate typing repetitive things over and over again:

~~~ haskell
var = VarE . mkName
($$) = AppE

res = InfixE
  (Just (var "f" $$ var "x"))
  (VarE '(:))
  (Just (var "map2" $$ var "f" $$ var "xs"))
~~~

Or like this if you don't mind:

~~~ haskell
res = InfixE
  (Just (AppE (VarE (mkName "f"))
              (VarE (mkName "x"))))
  (VarE '(:))
  (Just (AppE (AppE (VarE (mkName "map2"))
                    (VarE (mkName "f")))
              (VarE (mkName "xs"))))
~~~

## Patterns

Patterns are represented by the type [`Pat`][]:

~~~ haskell
data Exp
  = VarP Name
  | ConP Name [Pat]
  | InfixP Pat Name Pat
  | WildP
  | LitP Lit
  | TupP [Pat]
  | ListP [Pat]
  ...
~~~

  * `VarP` is a variable

  * `ConP` is a constructor (followed by a list of patterns that serve as its arguments – e.g. in `f (Just x)` the constructor is `Just` and the list of arguments is `[x]`)

  * `InfixP` is used for constructors that are operators

  * `WildP` is a wildcard

  * `LitP` is a literal

  * `TupP` is used for tuples, `ListP` – for lists

## Types

Types are represented by the type [`Type`][]:

~~~ haskell
data Type
  = ForallT [TyVarBndr] Cxt Type
  | AppT Type Type
  | VarT Name
  | ConT Name
  | ArrowT
  | ListT
  ...
~~~

`AppT` and `VarT` should be obvious; `ConT` refers to type constructors (like `Int` or `Maybe`); `ArrowT` is a special name for `->` (but you still have to apply it to types with `AppT`), and `ListT` is a special name for `[]`. `ForallT` is the `forall` in this type signature:

~~~ haskell
id :: forall a. a -> a
id x = x
~~~

You can omit it when you write ordinary Haskell, but you can't omit it when you're creating a `Type` – so, whenever you use a `VarT`, the variable must first be declared by a `ForallT`.

`ForallT` takes 3 arguments – a list of variables, context (e.g. `Ord a`) (so, just a list of constraints), and a type. Variables can be defined like this:

~~~ haskell
data TyVarBndr
  = PlainTV Name
  | KindedTV Name Kind
~~~

Here `PlainTV` is what we need (and `KindedTV` is for variables with kinds but variables almost always don't have explicit kind annotations so it doesn't matter right now).

Ugh, that was a lot of details. To make it a bit easier, I'll write down the type of `id`:

~~~ haskell
idT = ForallT
        [PlainTV (mkName "a")]
        []
        (ArrowT `AppT` VarT (mkName "a") `AppT` VarT (mkName "a"))
~~~

~~~ {.haskell .repl}
> pprint idT
"forall a . a -> a"
~~~

And now that you know all that, try to encode `map2`'s type by yourself.

## Declarations

Declarations are represented by [`Dec`][], and pretty much everything is a declaration:

~~~ haskell
data Dec
  = FunD Name [Clause]                            -- functions
  | ValD Pat Body [Dec]                           -- values
  | DataD Cxt Name [TyVarBndr] [Con] [Name]       -- datatypes
  | NewtypeD Cxt Name [TyVarBndr] Con [Name]      -- newtypes
  | TySynD Name [TyVarBndr] Type                  -- type synonyms
  | ClassD Cxt Name [TyVarBndr] [FunDep] [Dec]    -- classes
  | InstanceD Cxt Type [Dec]                      -- instances
  | SigD Name Type                                -- signatures
  ...
~~~

We're only going to use `FunD` and `SigD` right now, so let's look at [`Clause`][] and [`Body`][]:

~~~ haskell
data Clause = Clause [Pat] Body [Dec]

data Body
  = GuardedB [(Guard, Exp)]
  | NormalB Exp
~~~

A `Clause` is a single equation in a function declaration – it includes function arguments and `where`, but doesn't include the function name. A `Body` is either an expression or a bunch of guards-and-expressions, but we're not going to look at guards because they're another rabbit hole leading to lots of new types that we don't need right now.

## Putting it all together

First you try, then I'll show you the code. Your task is to write `map2TH`:

~~~ haskell
map2TH :: [Dec]
map2TH = ...
~~~

that would, when you pretty-print it (`putStrLn . pprint`), produce the following code:

~~~ haskell
map2 :: forall a b . (a -> b) -> [a] -> [b]
map2 f (x GHC.Types.: xs) = f x GHC.Types.: map2 f xs
map2 _ (GHC.Types.[]) = GHC.Types.[]
~~~

Just in case, without annoying `GHC.Types.` it looks like this:

~~~ haskell
map2 :: (a -> b) -> [a] -> [b]
map2 f (x:xs) = f x : map2 f xs
map2 _ [] = []
~~~

When you're done, you should be able to inject it into GHCi and test it. You'd have to use GHCi, because you can't just write `map2TH` on a separate line and have it work the same way `makeLenses` works – a restriction of TH is that the generated code and the *generating* code have to be in different modules. Here's how to test TH code in GHCi:

~~~ {.haskell .repl}
> data X; return map2TH    -- don't worry about “data X” now

> map2 (+1) [1,2,3]
[2,3,4]
~~~

Finally, here are some utilities that might save you typing:

~~~ haskell
(->>) a b = ArrowT `AppT` a `AppT` b
infixr 9 ->>

($$) f a = AppE f a
infixl 9 $$
~~~

## The answer

~~~ haskell
(->>) a b = ArrowT `AppT` a `AppT` b
infixr 9 ->>

($$) f a = AppE f a
infixl 9 $$

map2TH :: [Dec]
map2TH = [signature, function]
  where
    -- Names
    map2 = mkName "map2"

    -- Type signature
    a = VarT (mkName "a")
    b = VarT (mkName "b")
    type' = ForallT [PlainTV (mkName "a"), PlainTV (mkName "b")] [] $
              (a ->> b) ->> AppT ListT a ->> AppT ListT b
    signature = SigD map2 type'

    -- Variables and patterns
    (fPat, fExp) = (VarP (mkName "f"), VarE (mkName "f"))
    (xPat, xExp) = (VarP (mkName "x"), VarE (mkName "x"))
    (xsPat, xsExp) = (VarP (mkName "xs"), VarE (mkName "xs"))

    -- 1st equation
    eq1 = Clause
            -- arguments
            [fPat, InfixP xPat '(:) xsPat]
            -- result
            (NormalB (InfixE (Just (fExp $$ xExp))
                             (ConE '(:))
                             (Just (VarE map2 $$ fExp $$ xsExp))))
            -- no “where” block
            []

    -- 2nd equation
    eq2 = Clause [WildP, ConP '[] []]
                 (NormalB (ConE '[]))
                 []

    -- Function body
    function = FunD map2 [eq1, eq2]
~~~

## Oxford brackets (or `[| |]`s)

If you're used to Lisp macros, this might seem horrible to you, and it *is* pretty horrible indeed. Why are we writing the [AST](@w:Abstract Syntax Tree) by hand when we have a *compiler* that can parse Haskell into a syntax tree for us?

Well, we don't have to, and I actually have mentioned it earlier in this post:

> any code in `[| |]` brackets isn't code, but *representation* of that code

In other words, that whole thing could've just been replaced with this:

~~~ haskell
map2TH :: Q [Dec]
map2TH = [d|
  map2 :: (a -> b) -> [a] -> [b]
  map2 f (x:xs) = f x : map2 f xs
  map2 _ [] = []
  |]
~~~

Now you should be ~~expecting me to tell you answers to~~ asking yourself several questions:

  1. What does [`Q`][] do?
  2. What kind of a non-informative name is that?
  3. Also, why `[d| |]` instead of `[| |]` as the quote says?

The answer to the 3rd question is easy. There are 5 types of brackets:

  * `[| |]` (and `[e| |]`) parse their contents as an expression, and return `Q Exp`

  * `[t| |]` parses its contents as a type, and returns `Q Type`

  * `[p| |]` parses its contents as a pattern, and returns `Q Pat`

  * `[d| |]` parses its contents as a number of declaration, and returns `Q [Dec]`

  * `[someFunc| |] ` calls `someFunc` to parse the contents and generate code; the contents themselves don't have to be Haskell code, they can be HTML, or C code, or a regular expression, or whatever

The answer to the 2nd question is easy too (I think) – my theory is that TH code is usually messy and awkward and awful, and things with `Q` occur very often there, so the shortest name possible was chosen. In fact, even `Q [Dec]` is too long – we have a type synonym for that, [`DecsQ`][]. (Try to guess the synonyms for `Q Exp` and `Q Pat`.)

Now we come to the 1st question. What's `Q`?

## The `Q` monad

The [`Q`][] monad is a monad for generating code. The difference between it and just writing-an-AST-by-hand is that in the `Q` monad you can:

  * query information about values and types in scope
  * generate new names that are guaranteed to be unique (so that there wouldn't be any name conflicts between TH-generated code and “outside” code)
  * get current -location in the source-
  * do IO
  * report errors

Note that the IO is going to happen *during compilation*, because that's when TH is run). This is pretty useful, but it also should make you a tiny bit nervous because it means that any package from Hackage can remove your home directory when you do `cabal install`. (There are [worse things][beneficence] that others could do to you, tho.)

[beneficence]: http://abstrusegoose.com/479

Anyway, the next task is generating a simple lens for a 2-tuple:

~~~ haskell
-- The name should be configurable
<some name> :: Lens (a, x) (b, x) a b
<some name> f (a, x) = (\b -> (b, x)) <$> f a
~~~

Here are some notes that should help you:

  * To generate unique names, you can use `newName`; names generated by `mkName` would be preserved as-is. For instance, if you're generating a function called `fstLens`, you'd want to use `mkName` to generate the name of the function (because otherwise it'll look like `fstLens_0` or something), but you'd use `newName` for variables used inside of the function.

  * Since everything happens in `Q` now, you can't use `VarE`, `FunD`, etc verbatim – you either have to lift them (`VarE <$> ...`), which is annoying, or you can use lifted variants ([`varE`][], [`funD`][], etc):

    ~~~ haskell
    FunD :: Name -> [Clause] -> Dec
    funD :: Name -> [ClauseQ] -> DecQ
    ~~~

  * `tupP` is going to be useful to generate the `(a, x)` pattern:

    ~~~ haskell
    tupP :: [PatQ] -> PatQ
    ~~~

  * You can mix code generated manually and code generated with `[| |]`. For instance, if you want to generate `negate 1`, you can write any of these:

    ~~~ haskell
    -- fully manual:
    --   appE :: ExpQ -> ExpQ -> ExpQ
    --   varE :: Name -> ExpQ
    --   litE :: Lit -> ExpQ
    --   integerL :: Integer -> Lit
    appE (varE 'negate) (litE (integerL 1))

    -- 'negate' is generated with [| |]
    appE [|negate|] (litE (integerL 1))

    -- the literal is generated with [| |] too
    appE [|negate|] [|1|]

    -- the whole thing is generated
    [|negate 1|]
    ~~~

  * You can also *include* generated code into `[| |]`s by using `$`:

    ~~~ haskell
    -- 'negate' is generated manually
    [|$(varE 'negate) 1|]

    -- same but with a “let”:
    let negateE = varE 'negate in [|$negateE 1|]
    ~~~

    In short, any piece of code that has the required type (`VarQ`, `TypeQ`, `PatQ`) can be put into `$()` and included into the brackets. (This is called splicing.) By the way, you can use splices outside of the brackets, too:

    ~~~ {.haskell .repl}
    > let x = $(litE (integerL 3))

    > x
    3
    ~~~

  * Without the “name should be configurable” condition, the exercise would be trivial – you could just put the whole thing in `[d| |]` and be done with it. However, TH doesn't let you splice *names*:

    ~~~ haskell
    -- All of these are allowed:
    [d|x :: Int; x = 3|]
    [d|x :: $(conT ''Int); x = 3|]
    [d|x :: Int; x = $(litE (integerL 3))|]

    -- But this isn't:
    [d|$(mkName "x") :: Int; x = 3|]
    ~~~

    Thanks to this, you'll have to generate the signature and the body of the lens by hand (you still can use Oxford brackets for the *type* of the function and the right hand of the equation, tho).

  * Code generated in `Q` doesn't get spliced automatically; you can't do something like this:

    ~~~ haskell
    fstLensTH = do
      generateSignature
      generateBody
    ~~~

    You have to explicitly return the definitions you want to be spliced:

    ~~~ haskell
    fstLensTH = do
      sig <- generateSignature
      body <- generateBody
      return [sig, body]
    ~~~

  * You can get generated code out with `runQ`:

    ~~~ {.haskell .repl}
    -- runQ :: Q a -> IO a

    > putStrLn . pprint =<< runQ [|negate 1|]
    GHC.Num.negate 1
    ~~~

    This may be useful when debugging.

When you're done, test it:

~~~ {.haskell .repl}
> data X; fstLensTH "fstL"

> (1,2) ^. fstL
1
~~~

## The answer

~~~ haskell
mkVars :: String -> Q (PatQ, ExpQ)
mkVars name = do
  x <- newName name
  return (varP x, varE x)

fstLensTH :: String -> DecsQ
fstLensTH lensName = do
  -- Generate the signature
  signature <- sigD (mkName lensName)
                    [t|forall a b x. Lens (a, x) (b, x) a b|]

  -- Generate the body of the function
  (f_, f) <- mkVars "f"
  (a_, a) <- mkVars "a"
  (b_, b) <- mkVars "b"
  (x_, x) <- mkVars "x"
  body <- funD (mkName lensName) [
            clause [f_, tupP [a_, x_]]
                   (normalB [|(\ $b_ -> ($b, $x)) <$> $f $a|])
                   []
            ]

  -- Return the signature and the body
  return [signature, body]
~~~

## The explanation of the `data X` thing

When you're compiling a module, you don't have to use any `data X` to splice generated code into it; you can just write this and generated definitions would be spliced into the file:

~~~ haskell
fstLensTH "fstL"
-- or this
$(fstLensTH "fstL)
~~~

However, for whatever reason GHCi can't normally guess when you want to splice something:

~~~ {.haskell .repl}
> fstLensTH "fstL"

<interactive>:
    No instance for (Show DecsQ)
      arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it

> $(fstLensTH "fstL")

<interactive>:513:3:
    Couldn't match type ‘[Dec]’ with ‘Exp’
    Expected type: ExpQ
      Actual type: DecsQ
    In the expression: fstLensTH "fstL"
    In the splice: $(fstLensTH "fstL")
~~~

And `data X` is needed to hint GHCi that it's being given *declarations* and thus should treat the splice as yet another declaration. It's ad-hoc, but [it works][haskelltil TH].

[haskelltil TH]: https://www.reddit.com/r/haskelltil/comments/3ghacj/you_can_use_template_haskell_functions_like/

## Another exercise

Modify `fstLensTH` to generate a lens for a n-tuple:

~~~ haskell
-- >>> fstLensTH "fst4" 4
-- fst4 :: forall a b x1 x2 x3. Lens (a, x1, x2, x3) (b, x1, x2, x3) a b
-- fts4 f (a, x1, x2, x3) = (\b -> (b, x1, x2, x3)) <$> f a

fstLensTH :: String -> Int -> DecsQ
~~~

Notes:

  * There's no `tupT` similar to [`tupE`][] or [`tupP`][]. If you want to generate a tuple type, you have to use [`tupleT`][] together with `appT` (possibly used several times).

  * To generate an empty context (`Cxt`), either use `return []` or (preferred) `cxt []`.

## The answer

~~~ haskell
import Data.Traversable

mkVars :: String -> Q (PatQ, ExpQ)
mkVars name = do
  x <- newName name
  return (varP x, varE x)

-- >>> fstType 4
-- forall a b x1 x2 x3. Lens (a, x1, x2, x3) (b, x1, x2, x3) a b
fstType :: Int -> TypeQ
fstType n = do
  xs <- for [1..n-1] (\i -> newName ("x" ++ show i))
  a <- newName "a"
  b <- newName "b"
  -- foldl appT (tupleT n) :: [TypeQ] -> TypeQ
  let tupleA = foldl appT (tupleT n) (map varT (a:xs))
      tupleB = foldl appT (tupleT n) (map varT (b:xs))
  forallT (map PlainTV (a:b:xs))
          (cxt [])
          [t|Lens $tupleA $tupleB $(varT a) $(varT b)|]

-- >>> fstClause 4
-- ? f (a, x1, x2, x3) = (\b -> (b, x1, x2, x3)) <$> f a
fstClause :: Int -> ClauseQ
fstClause n = do
  (f_, f) <- mkVars "f"
  (a_, a) <- mkVars "a"
  (b_, b) <- mkVars "b"
  -- Generate x1, x2, ..., xn
  (xs_, xs) <- unzip <$> for [1..n-1] (\i -> mkVars ("x" ++ show i))
  clause [f_, tupP (a_:xs_)]
         (normalB [|(\ $b_ -> $(tupE (b:xs))) <$> $f $a|])
         []

fstLensTH :: String -> Int -> DecsQ
fstLensTH lensName n = do
  signature <- sigD (mkName lensName) (fstType n)
  body <- funD (mkName lensName) [fstClause n]
  return [signature, body]
~~~

## Getting information about types

The next step to `makeLenses` is getting information about a type. This is done by [`reify`][]:

~~~ haskell
reify :: Name -> Q Info
~~~

[`Info`][] is a structure that looks like this:

~~~ haskell
data Info
  = ClassI Dec [InstanceDec]              -- Class and its instances
  | TyConI Dec                            -- Type constructor
  | DataConI Name Type ParentName Fixity  -- Data constructor
  | ClassOpI Name Type ParentName Fixity  -- Class method
  | VarI Name Type (Maybe Dec) Fixity     -- Variable/function
  ...
~~~

(Unfortunately, you can't actually get definitions of functions using `VarI`, but we won't need it anyway so it doesn't matter.)

Let's try `reify`:

~~~ {.haskell .repl}
> runQ (reify ''Bool)
Template Haskell error: Can't do `reify' in the IO monad
*** Exception: user error (Template Haskell failure)
~~~

Ouch.

The reason for the error is that GHC only provides information about types/values to splices, so we'd have to run `reify` inside a splice. That's not hard to do:

~~~ {.haskell .repl}
> $(reify ''Bool)

<interactive>:
    Couldn't match type ‘Info’ with ‘Exp’
    Expected type: ExpQ
      Actual type: Q Info
    In the expression: reify ''Bool
    In the splice: $(reify ''Bool)
~~~

No, wait, we need `Exp`, right. Well, let's pretty-print `Info` and then turn it into a string literal:

~~~ {.haskell .repl}
> $(stringE . pprint =<< reify ''Bool)
"data GHC.Types.Bool = GHC.Types.False | GHC.Types.True"
~~~

Hm, pretty-printing is actually not that helpful in this case. Just use `show`, then:

~~~ {.haskell .repl}
> $(stringE . show =<< reify ''Bool)
"TyConI (DataD
           []                              -- no constaints
           GHC.Types.Bool                  -- “Bool =”
           []                              -- no type variables
           [ NormalC GHC.Types.False [],   -- False |
             NormalC GHC.Types.True  [] ]  -- True
           [])"                            -- not deriving anything
                                           --   (TH lies to us in this
                                           --   case, but whatever)
~~~

(Indentation is mine.)

What would happen if we used a record?

~~~ haskell
data Person = Person {
  name :: String,
  age :: Double }
~~~

~~~ {.haskell .repl}
> $(stringE . show =<< reify ''Person)
"TyConI (DataD
           []
           Test.Person
           []
           [RecC Test.Person
              [ (Test.name,NotStrict,ConT GHC.Base.String),
                (Test.age ,NotStrict,ConT GHC.Types.Double) ] ]
           [])"
~~~

Okay, now you should be able to write a function that takes a record name and returns a list of its fields. `nameBase` would be useful (it takes a `Name` and returns just the name without the module).

If you've written it correctly, here's how you can test it:

~~~ {.haskell .repl}
> $(listE . map stringE =<< listFields ''Person)
["Person.name","Person.age"]
~~~

-----------------------------------------------------------------------------

The answer:

~~~ haskell
listFields :: Name -> Q [String]
listFields name = do
  -- A warning: with GHC 8, you'll have to add an extra “_” before “cons”
  TyConI (DataD _ _ _ cons _) <- reify name
  return [nameBase conName ++ "." ++ nameBase fieldName
         | RecC conName fields <- cons
         , (fieldName, _, _) <- fields]
~~~

## Writing a very simple version of `makeLenses`

For now let's only look at records with 1 constructor and no type variables. The goal is to take

~~~ haskell
data Person = Person {
  _name :: String,
  _age :: Double }
~~~

and write something that would generate the following (skip the fields that don't begin with a `_`):

~~~ haskell
age :: Lens' Person Double
age f (Person x1 x2) = fmap (\y -> Person x1 y) (f x2)

name :: Lens' Person String
name f (Person x1 x2) = fmap (\y -> Person y x2) (f x1)
~~~

You might find the following 2 functions slightly useful:

  * [`lam1E`][] creates a lambda:

    ~~~ haskell
    lam1E :: PatQ -> ExpQ -> ExpQ
    lam1E arg res = [|\ $arg -> $res|]
    ~~~

  * [`appsE`][] applies an expression to a list of arguments:

    ~~~ haskell
    appsE :: [ExpQ] -> ExpQ
    ~~~

    ~~~ {.haskell .repl}
    > $(stringE . pprint =<< appsE [ [|max|], [|1|], [|2|] ])
    "GHC.Classes.max 1 2"
    ~~~

Also, if you need a hint, here it is: if you split it into `makeLenses` and `makeLens`, the signature of `makeLens` would look approximately like this:

~~~ haskell
makeLens
  :: Name    -- ^ Type name
  -> Name    -- ^ Constructor name
  -> Name    -- ^ Lens name
  -> Type    -- ^ Field type
  -> Int     -- ^ Field position in the constructor
  -> Int     -- ^ Overall fields amount
  -> DecsQ
~~~

## The answer

~~~ haskell
makeLenses :: Name -> DecsQ
makeLenses typeName = do
  -- Get constructors:
  --
  --   cons :: [Con]
  TyConI (DataD _ _ [] cons _) <- reify typeName

  -- Get the constructor name and its fields:
  --
  --   conName :: Name
  --   fields  :: [VarStrictType] :: [(Name, Strict, Type)]
  [RecC conName fields] <- return cons

  -- Make the lenses (concat is needed because for is going to return Q
  -- [[Dec]], and we need just Q [Dec])
  fmap concat $
    for (zip fields [0..]) $ \((fieldName, _, fieldType), fieldPos) ->
      case nameBase fieldName of
        ('_':rest) -> makeLens typeName conName (mkName rest)
                               fieldType fieldPos (length fields)
        _ -> return []

makeLens
  :: Name    -- ^ Type name
  -> Name    -- ^ Constructor name
  -> Name    -- ^ Lens name
  -> Type    -- ^ Field type
  -> Int     -- ^ Field position in the constructor
  -> Int     -- ^ Overall fields amount
  -> DecsQ
makeLens typeName conName lensName fieldType fieldPos fieldCount = do
  -- The signature
  let type_ = [t|Lens' $(conT typeName) $(return fieldType)|]
  signature <- sigD lensName type_

  -- The lens
  (f_, f) <- mkVars "f"
  (y_, y) <- mkVars "y"
  (xs_, xs) <- unzip <$> for [0..fieldCount-1] (\i -> mkVars ("x" ++ show i))
  -- lam  = (\y -> Con ...)
  -- pats = ? f (Con x1 x2 ...)
  -- rhs  = fmap lam (f xi)
  let lam  = lam1E y_ (appsE (conE conName : (xs & ix fieldPos .~ y)))
      pats = [f_, conP conName xs_]
      rhs  = [|fmap $lam ($f $(xs !! fieldPos))|]
  body <- funD lensName [clause pats (normalB rhs) []]

  -- All together
  return [signature, body]

mkVars :: String -> Q (PatQ, ExpQ)
mkVars name = do
  x <- newName name
  return (varP x, varE x)
~~~

By the way, the `fmap concat` trick is also useful when you want to create lenses for several types – instead of writing

~~~ haskell
makeLenses ''A
makeLenses ''B
makeLenses ''C
~~~

you can write

~~~ haskell
concat <$> mapM makeLenses [''A, ''B, ''C]
~~~

## Type variables

At the moment our `makeLenses` won't work for something like this (in particular, it's going to fail with a pattern match failure):

~~~ haskell
data Person a = Person {
  _name :: a,
  _age :: Double }
  deriving (Show)
~~~

Notes:

  * To make it work, you'll have to take into account type variables when generating the type of the lens. In particular, `conT typeName` here would have to be replaced with something that would generate `Person a`:

    ~~~ haskell
    let type_ = [t|Lens' $(conT typeName) $(return fieldType)|]
    ~~~

    For that, [`conAppsT`][] from [`Control.Lens.Internal.TH`][] would be pretty useful:

    ~~~ haskell
    conAppsT :: Name -> [Type] -> Type
    conAppsT conName = foldl AppT (ConT conName)
    ~~~

    (Yep, it's this thing with `foldl` that we used before. I wanted to show you how to use `foldl` with `AppT` before showing that `conAppsT` exists, especially since it lives in lens and not in template-haskell.)

  * Another useful thing is [`name`][] – a lens from [`Language.Haskell.TH.Lens`][] that extracts a name from anything that has one (including type variables). So, the construction of the full type would look like this:

    ~~~ haskell
    fullType = typeName `conAppsT` [VarT (v ^. name) | v <- vars]
    ~~~

    The reason it's not enough to just do `[VarT v | PlainTV v <- vars]` is that a “plain” type variable is the same as a kinded type variable with kind `*`, so `reify` can return either a list of `PlainTV`s or a list of `KindedTV`s, and it's easier to just use `name` to extract the name of the type variable from whatever you might be getting. (If you want to know more about kinds, read [here](https://wiki.haskell.org/Kind). In a nutshell: concrete types have kind `*`, type constructors like `Maybe` have kinds like `* -> *`. Since `a` is a plain type variable, you can have `Person Int`, but you can't have `Person Maybe`. For an example of type variables that aren't `*`, see e.g. `ReaderT r m a`, where `r` and `a` have kind `*`, but `m` has kind `* -> *`.)

  * Also keep in mind that any variables in the type of the lens have to be mentioned in a `forall`. Instead of adding them manually, you can use `quantifyType` – it's not exported from anywhere but it's used in the internals of lens and you can just steal it:

    ~~~ haskell
    quantifyType :: Type -> Type
    quantifyType t = ForallT vs [] t
      where
        vs = map PlainTV (nub (t ^.. typeVars))
    ~~~

    [`typeVars`][] is another thing from [`Language.Haskell.TH.Lens`][] – a traversal that traverses all free variables in a type.

## The answer

~~~ haskell
makeLenses :: Name -> DecsQ
makeLenses typeName = do
  -- Get constructors and variables:
  --
  --   cons :: [Con]
  --   vars :: [TyVarBndr]
  TyConI (DataD _ _ vars cons _) <- reify typeName

  -- The full type, thus, is:
  let fullType :: Type
      fullType = typeName `conAppsT` [VarT (v ^. name) | v <- vars]

  -- Get the constructor name and its fields:
  --
  --   conName :: Name
  --   fields  :: [VarStrictType] :: [(Name, Strict, Type)]
  [RecC conName fields] <- return cons

  -- Make the lenses (concat is needed because for is going to return Q
  -- [[Dec]], and we need just Q [Dec])
  fmap concat $
    for (zip fields [0..]) $ \((fieldName, _, fieldType), fieldPos) ->
      case nameBase fieldName of
        ('_':rest) -> makeLens fullType conName (mkName rest)
                               fieldType fieldPos (length fields)
        _ -> return []

makeLens
  :: Type    -- ^ Type
  -> Name    -- ^ Constructor name
  -> Name    -- ^ Lens name
  -> Type    -- ^ Field type
  -> Int     -- ^ Field position in the constructor
  -> Int     -- ^ Overall fields amount
  -> DecsQ
makeLens fullType conName lensName fieldType fieldPos fieldCount = do
  -- The signature
  let type_ = quantifyType (conAppsT ''Lens' [fullType, fieldType])
  -- (We could have “type_” in the Q monad and use [| |], but there's no
  -- reason to do it and I also wanted to showcase conAppsT)
  let signature = SigD lensName type_

  -- The lens
  (f_, f) <- mkVars "f"
  (y_, y) <- mkVars "y"
  (xs_, xs) <- unzip <$> for [0..fieldCount-1] (\i -> mkVars ("x" ++ show i))
  -- lam  = (\y -> Con ...)
  -- pats = ? f (Con x1 x2 ...)
  -- rhs  = fmap lam (f xi)
  let lam  = lam1E y_ (appsE (conE conName : (xs & ix fieldPos .~ y)))
      pats = [f_, conP conName xs_]
      rhs  = [|fmap $lam ($f $(xs !! fieldPos))|]
  body <- funD lensName [clause pats (normalB rhs) []]

  -- All together
  return [signature, body]

quantifyType :: Type -> Type
quantifyType t = ForallT vs [] t
  where
    vs = map PlainTV (nub (t ^.. typeVars))

mkVars :: String -> Q (PatQ, ExpQ)
mkVars name = do
  x <- newName name
  return (varP x, varE x)
~~~

## What to do next

Okay, now you can write your own simple `makeLenses`! How is it different from lens's `makeLenses`?

Most of lens's TH code (excluding code that generates prisms) lives in [`Control.Lens.Internal.FieldTH`][`Control.Lens.Internal.FieldTH` src]. If you want your `makeLenses` to be close to lens's `makeLenses`, you should:

  * Allow newtypes (which can be single-field records). In lens it's done by `makeFieldOpticsForDec`.

  * Handle records with several constructors; generate lenses for fields that are present in all constructors, and traversals otherwise. In lens, the decision whether to create a lens or a traversal is made by `buildScaffold`, which is kinda confusing and so I'll comment on some of its local functions/variables.

    This is a list of constructors, together with types of all their fields. `Left` means “not the field we're currently making a lens for”, `Right` means the opposite:

    ~~~ haskell
    consForDef :: [(Name, [Either Type Type])]
    consForDef = over (mapped . _2 . mapped) categorize cons
    ~~~

    This line checks that the field is present in all constructors; if not, we'll generate a traversal:

    ~~~ haskell
    lensCase :: Bool
    lensCase = all (\x -> lengthOf (_2 . folded . _Right) x == 1) consForDef
    ~~~

    It's all more complicated than it could be because lens permits a generated traversal to update several fields of the same constructor, as the documentation for [`makeLensesFor`][] says:

    > If you map multiple names to the same label, and it is present in the same constructor then this will generate a `Traversal`.

    Anyway, the actual lens/traversal is generated by `makeFieldClauses`, which takes a list like `[(Name, Int, [Int])]` (name, amount of fields, field positions) and generates the clauses (or equations). Each clause is generated by `makeFieldOpticClause`, which should look very vaguely familiar to you (at least it does to me).

  * Create an `Iso` when the record has only 1 constructor with 1 field in it.

  * Create `Getter`/`Fold` instead of `Lens`/`Traversal` when the type of the field is existential (has a `forall` in it). This is determined in `buildScaffold`, too.

  * Handle data families (no idea how or what or why, I haven't ever used them).

  * Generate type-changing lenses where possible (to do that, look at `buildStab`).

    Here's the code that determines the type variables (`unfixedTypeVars`) that don't belong to any of the other fields (and therefore can be changed):

    ~~~ haskell
    (fixedFields, targetFields) = partitionEithers categorizedFields
    fixedTypeVars               = setOf typeVars fixedFields
    unfixedTypeVars             = setOf typeVars s Set.\\ fixedTypeVars
    ~~~

    This generates a fresh name for each of the variables:

    ~~~ haskell
    sub <- T.sequenceA (fromSet (newName . nameBase) unfixedTypeVars)
    ~~~

    This substitutes variables with their new counterparts in `s` and `a`, producing `t` and `b`:

    ~~~ haskell
    let (t,b) = over both (substTypeVars sub) (s',a)
    ~~~

    Finally, `buildScaffold` checks whether `s == t` and `a == b` and generates either a `Lens'` or a `Lens`:

    ~~~ haskell
    -- Generate simple Lens and Traversal where possible
    | _simpleLenses rules || s' == t && a == b =
        let optic | isoCase && _allowIsos rules = iso'TypeName
                  | lensCase                    = lens'TypeName
                  | otherwise                   = traversal'TypeName
        in OpticSa [] optic s' a
    ~~~

  * Generate inline pragmas (look at `inlinePragma`, which has 3 ways of generating pragmas for various versions of GHC).

And that's pretty much all (unless you also want to generate classes, in which case it's not).

# P.S.

Here's a [poll](http://strawpoll.de/33gfe25) about possibly maybe turning lens over tea into a book when it's finished. Please fill it; if you don't, the sample would be really really skewed and the poll would be worthless. (Well, it's already moderately worthless since you have to be reading this in order to participate, but if you read this and don't participate it would be *totally* worthless.)

Thanks for caring about statistics.


[`Control.Lens.Internal.FieldTH` src]: http://hackage.haskell.org/package/lens/docs/src/Control-Lens-Internal-FieldTH.html

[`Control.Lens.TH`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html
[`makeLenses`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#v:makeLenses
[`makeLensesFor`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#v:makeLensesFor
[`makeLensesWith`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#v:makeLensesWith
[`makeFields`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#v:makeFields
[`makePrisms`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#v:makePrisms
[`declareLenses`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#v:declareLenses
[`LensRules`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html#t:LensRules

[`Language.Haskell.TH`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html
[`Exp`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Exp
[`Pat`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Pat
[`Type`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Type
[`Dec`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Dec
[`Clause`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Clause
[`Body`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Body
[`TyVarBndr`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:TyVarBndr
[`Cxt`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Cxt
[`mkName`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:mkName
[`pprint`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:pprint
[`Q`]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Q
[`DecsQ`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Lib.html#t:DecsQ
[`varE`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:varE
[`funD`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:funD
[`tupleT`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:tupleT
[`tupP`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:tupP
[`tupE`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:tupE
[`reify`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reify
[`Info`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Info
[`nameBase`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:nameBase
[`lam1E`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:lam1E
[`appsE`]: http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:appsE

[`Control.Lens.Internal.TH`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Internal-TH.html
[`conAppsT`]: http://hackage.haskell.org/package/lens/docs/Control-Lens-Internal-TH.html#v:conAppsT

[`Language.Haskell.TH.Lens`]: http://hackage.haskell.org/package/lens/docs/Language-Haskell-TH-Lens.html
[`typeVars`]: http://hackage.haskell.org/package/lens/docs/Language-Haskell-TH-Lens.html#v:typeVars

[`GHC.Types`]: http://hackage.haskell.org/package/ghc-prim/docs/GHC-Types.html

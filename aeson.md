% Aeson: the tutorial

[Aeson](@hackage:aeson) is the most widely used library for parsing [JSON](@w) (in Haskell, of course, I'm not talking about the whole world). It's also hopelessly magical for people who try to learn it by looking at provided examples, and existing tutorials don't help that much, so I wrote my own. It's supposed to be the most comprehensive Aeson tutorial on the web; if there's anything missing from it, send me an email! (So far the only thing I haven't covered is using lenses with Aeson, but this is coming soon.)

Note that knowing all this is *not* needed if you want to parse JSON in Haskell; if you just want to get it done – fast – then skip to the [generics](#records-and-json-generics) section. However, if you're doing some tricker JSON processing, then read it all.

Here's an incomplete list of things the tutorial explains (excluding basic stuff and “how it all works inside”):

  * [how various types like `Bool`, `Map`, etc are represented by default](#fromjson-instances-for-other-types)
  * encoding/decoding records manually
      * [applicative-style](#records-and-json)
      * [my preferred style](#recordwildcards)
      * [with postprocessing](#postprocessing)
      * [with optional fields](#optional-fields)
  * [choice: dealing with JSON that can come in several varieties](#more-interesting-choices)
  * [representing types with many constructors](#types-with-many-constructors)
  * [dealing with nested datatypes](#nested-records)
  * [dealing with datatypes which are subsets of other datatypes](#extended-records)
  * [dealing with unknown field names](#unknown-field-names)
  * generic encoding/decoding
      * [with generics](#records-and-json-generics)
      * [with Template Haskell](#records-and-json-template-haskell)
      * [customising field names](#generics-customising-field-names)
      * [handling uppercased/prefixed/etc field names in data](#generics-handling-weird-field-names-in-data)
      * [how optional fields are treated](#generics-optional-fields)
  * [parsing without creating extra types](#parsing-without-creating-extra-types)
  * [pretty-printing](#pretty-printing)

You probably should read it from beginning to end, because some questions of the form “how to do something with Aeson” are hard to answer without understanding how Aeson works. Even if you know how Aeson works, you still probably should read it all – there are some idioms and bits of knowledge scattered around that aren't present in other tutorials, like [`RecordWildCards`](@ghc-ext), and [`Alternative`][], and a list of default encodings for various types, and pretty-printing, and customising generic instances, and stuff.

### A note on string types

For following examples to work, you need to enable the [`OverloadedStrings`](@ghc-ext) extension – either by writing `{-# LANGUAGE OverloadedStrings #-}` at the top of the module, or doing `:set -XOverloadedStrings` in GHCi prompt.

The reason for this is that most of the time you have JSON in a file or receive it over network, so JSON decoding/encoding functions work with [`ByteString`][] and not with [`String`][] or [`Text`][]. (If you need to convert JSON to/from `Text`, there's a possibility you're doing something wrong.) And normally string literals like `"foo"` can only mean `String`, so you can't use Aeson's decoding functions on them – but the `OverloadedStrings` extension lifts this restriction and lets string literals be converted to `ByteString` or `Text` automatically (similarly to how `3` can be converted to `Int`, `Integer`, or `Double` automatically).

The `-XOverloadedStrings` trick only applies when you're playing with Aeson in GHCi. If you want to read JSON from a file, for instance, you should read it with [`Data.ByteString.Lazy.readFile`][] and not with Prelude's [`readFile`][].

However, if you *really* need it for some reason, you can also convert a `ByteString` to/from `String` by using [`fromString`][] and [`toString`][] from [`Data.ByteString.Lazy.UTF8`][] in the [utf8-string](@hackage) package, or to/from `Text` by using [`encodeUtf8`][] and [`decodeUtf8`][] from [`Data.Text.Lazy.Encoding`][] – but don't expect good performance.

### Very basic decoding and encoding

There are 2 main classes used in Aeson – [`FromJSON`][] and [`ToJSON`][]. A type which you want to convert to/from JSON should be an instance of these classes. You can think of `FromJSON` as of [`Read`][], and of `ToJSON` as of [`Show`][] – but instead of reading from a string or converting to a string, you read from JSON or convert to JSON.

There are also 2 functions for actually doing “reading” and “showing”, which are called [`decode`][] and [`encode`][]. (`decode` differs from `read` a bit by returning `Nothing` if reading was unsuccessful, instead of throwing an exception – so, it's closer to [`readMaybe`][] in this regard.)

An example of encoding a list of integers to JSON:

~~~ {.haskell .repl}
> encode [1,2,3]
"[1,2,3]"
~~~

An example of decoding a list of integers from JSON:

~~~ {.haskell .repl}
> :set -XOverloadedStrings

> import Data.Aeson

> decode "[1,2,3]" :: Maybe [Integer]
Just [1,2,3]

> decode "foo" :: Maybe [Integer]
Nothing
~~~

If you want to see the error too, use [`eitherDecode`][]:

~~~ {.haskell .repl}
> eitherDecode "[]" :: Either String Integer
Left "Error in $: expected Integral, encountered Array"
~~~

`$` is the location of the error – here it just means “top-level value”, but it can be helpful in more complicated cases:

~~~ {.haskell .repl}
> eitherDecode "[1,2,[3,4]]" :: Either String (Int, Int, (Int, Bool))
Left "Error in $[2][1]: expected Bool, encountered Number"

> eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool))
Right (1,2,(3,True))
~~~

### Working directly with JSON

Aeson has its own datatype for representing JSON, which is called simply [`Value`][`type Value`]. It has got 6 constructors:

~~~ haskell
data Value
  = Object Object	 
  | Array Array	 
  | String Text	 
  | Number Scientific	 
  | Bool Bool	 
  | Null
~~~

So, if you want to construct JSON directly, you can do it by constructing a `Value` and then converting it to JSON with `encode`:

~~~ haskell
import GHC.Exts    -- (fromList)

val :: Value
val = Object $ fromList [
  ("numbers", Array $ fromList [Number 1, Number 2, Number 3]),
  ("boolean", Bool True) ]
~~~

~~~ {.haskell .repl}
> import qualified Data.Text.Lazy.IO as T
> import qualified Data.Text.Lazy.Encoding as T

> T.putStrLn . T.decodeUtf8 . encode $ val
{"boolean":true,"numbers":[1,2,3]}
~~~

We had to use [`fromList`][] (which has to be imported from [`GHC.Exts`][]) 2 times – once to convert a list of pairs to [`Object`][`type Object`], another time to convert a list to [`Array`][`type Array`]. These are just type synonyms – the actual types are [`HashMap`][] (for `Object`) and [`Vector`][] (for `Array`).

~~~ haskell
type Object = HashMap Text Value
type Array = Vector Value
~~~

<div class="note">

If you use GHC 7.8 or older (you can find out the version by running `ghc --version`), `fromList` might not work for you – in this case you'd have to use specialised versions from `Data.Vector` and `Data.HashMap.Strict`.

~~~ haskell
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

{-
    V.fromList :: [a] -> Vector a
    HM.fromList :: [(a, b)] -> HashMap a b

Or, in context of Aeson:

    V.fromList :: [Value] -> Array
    HM.fromList :: [(Text, Value)] -> Object
-}
~~~

</div>

To make constructing JSON a bit easier, there is a function called [`object`][] (which accepts a list of pairs instead of a `HashMap`) and an operator called [`.=`][] (which is the same as `(,)` except that it also converts the value to JSON). So, this example could be rewritten like this:

~~~ haskell
val :: Value
val = object [
  "boolean" .= True,
  "numbers" .= [1,2,3::Int] ]    -- a type annotation is needed because
                                 -- otherwise it's unclear whether it should
                                 -- be Int or, say, Double or Rational
~~~

Working with `Value` is easy as well, just keep in mind that

  * an object is a `HashMap`
  * an array is a `Vector`
  * a string is a `Text`
  * a number is [`Scientific`][] (because JSON doesn't specify precision and so a type which allows arbitrary precision is used)

For instance, here's a function which reverses all strings anywhere in a `Value`; it's useless, but it shows how `Value`s can be transformed:

~~~ haskell
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.Exts

revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array x)  = Array (fmap revStrings x)
revStrings (Object x) = let revPair (k, v) = (T.reverse k, revStrings v)
                        in  Object . fromList . map revPair . HM.toList $ x
revStrings other      = other
~~~

You can combine it with `encode` and `decode`, if you want:

~~~ {.haskell .repl}
> import qualified Data.Text.Lazy.IO as T
> import qualified Data.Text.Lazy.Encoding as T
> import Data.Maybe

> let revJSON = encode . revStrings . fromJust . decode

> T.putStrLn . T.decodeUtf8 . revJSON . T.encodeUtf8 =<< T.getLine
{"numbers":[1,2,3],"names":["Jane","Artyom"]}
{"seman":["enaJ","moytrA"],"srebmun":[1,2,3]}
~~~

### Parsing simple types

Let's say our task is to parse an array of objects, each of which has fields "a" (a string) and "b" (a boolean), into a list of tuples. How shall we do it?

First, let's write a parser for inner objects. But first first, read the following explanation – it's very important and if you don't understand it you *will* get problems later:

  * A parser is something that turns a JSON value into something else that you need (record, list of tuples, etc).

  * Aeson has a type called [`Parser`][], but it's *not* a type for parsers – it's a type for results of parsers. `Parser a` means pretty much the same as `Either String a` – it's either an `a` or an error message. (The actual implementation is different, and uses CPS for speed, but it's not important.)

  * (And there's also a type called [`Result`][] that is for results of parsers as well but doesn't use CPS. Don't worry about it for now.)

  * So, all parsers actually have the type `Value -> Parser a`.

Since we're parsing a list of tuples, in our case the type would be `Value -> Parser (String, Bool)`:

~~~ haskell
import Data.Aeson.Types    -- that's where Parser comes from
~~~

~~~ haskell
parseTuple :: Value -> Parser (String, Bool)
~~~

What now? Okay, we know that the `Value` has to be an `Object`, but what to do about other types? Well, we can just use [`fail`][] to signal an error (similarly to how we'd use `Left` to signal an error if `Parser` actually was `Either String a`):

~~~ haskell
parseTuple (Object o) = ...
parseTuple _          = fail "expected an object"
~~~

<div class="note">

`Parser`, just like `Either`, is a monad (and also [`Applicative`][] and [`Alternative`][]), so we can use all the usual things on it; if you ever used [Parsec](@hackage:parsec), this shouldn't be too unfamiliar for you. If you haven't, here's a crash course.

You can chain `Parser`s using do notation. For instance, this:

~~~ haskell
do x <- parserX
   y <- parserY
   return (x, y)
~~~

is equivalent to the following pseudocode:

~~~
if parserX fails
  then return its error message
  else call its result “x” and proceed
if parserY fails
  then return its error message
  else call its result “y” and proceed
if neither of parsers failed
  then return (x, y)
~~~

You can also apply a function to the result of a parser, while keeping the error message if the parser fails:

~~~ haskell
f <$> parserX
~~~

is equivalent to

~~~
if parserX fails
  then return its error message
  else return its result with “f” applied to it
~~~

If `f` takes several parameters, you can keep applying it with `<*>`:

~~~ haskell
getW :: X -> Y -> Z -> W
~~~

~~~ haskell
parserW :: Parser W
parserW = getW <$> parserX <*> parserY <*> parserZ
~~~

</div>

Now we can already write our function, by doing manual lookups in the object:

~~~ haskell
import qualified Data.HashMap.Strict as HM

parseTuple (Object obj) = do
  -- Look up the "a" field.
  let mbFieldA = HM.lookup "a" obj

  -- Fail if it wasn't found.
  fieldA <- case mbFieldA of
    Just x  -> return x
    Nothing -> fail "no field 'a'"

  -- Extract the value from it, or fail if it's of the wrong type.
  a <- case fieldA of
    String x -> return (T.unpack x)
    _        -> fail "expected a string"

  -- Do all the same for "b" (in a slightly terser way, to save space):
  b <- case HM.lookup "b" obj of
    Just (Bool x) -> return x
    Just _        -> fail "expected a boolean"
    Nothing       -> fail "no field 'b'"

  -- That's all!
  return (a, b)
~~~

<div class="note">

This isn't how you should actually write JSON parsers! In the following several sections we'll simplify this parser drastically (in the end it'll be only 4 lines long) using functions and operators from Aeson. The reason I wrote this version of the parser first is that otherwise you wouldn't understand how Aeson works, and that would've bitten you later.

</div>

Parsing an array, in comparison, is much less messy:

~~~ haskell
-- Vector is the type Aeson uses to represent JSON arrays
import qualified Data.Vector as V

parseArray :: Value -> Parser [(String, Bool)]
parseArray (Array arr) = mapM parseTuple (V.toList arr)
parseArray _           = fail "expected an array"
~~~

<div class="note">

To understand what's going on, look at the type of [`mapM`][]:

~~~ haskell
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
~~~

(In GHC 7.10 and newer it's more general, but nothing changes if you consider this less general version.)

`mapM` applies a function to a list (resulting in `[m b]`) and then “lumps together” all results. What “lumping together” means is different for each type, but in case of `Parser` it's simply “try getting the value out of all parsers, fail if there is any parser that fails, return the list of values otherwise”. So, in our case the type is:

~~~ haskell
mapM :: (Value -> Parser a) -> [Value] -> Parser [a]
~~~

</div>

Finally, when we have a parser for arrays, there's a function called [`parseMaybe`][] in [`Data.Aeson.Types`][], which applies a parser to a value:

~~~ {.haskell .repl}
> import qualified Data.Text.Lazy.IO as T
> import qualified Data.Text.Lazy.Encoding as T

-- Enter some JSON:
> s <- T.encodeUtf8 <$> T.getLine
[{"a":"hello", "b":true}, {"a":"world", "b":false}]

-- Okay, let's look at the JSON we're going to parse:
> decode s :: Maybe Value
Just (Array [Object (fromList [("a",String "hello"),("b",Bool True)]),
             Object (fromList [("a",String "world"),("b",Bool False)])])

-- And finally parse it (using =<< to chain Maybes):
> parseMaybe parseArray =<< decode s
Just [("hello",True),("world",False)]
~~~

### Avoiding manual type checks

One reason `parseTuple` was so big for such a simple task is that we had to check for type mismatch manually. Instead of doing that, we can use the `with*` family of functions.

Look at both parsers again:

~~~ haskell
parseTuple (Object o) = ...
parseTuple _          = fail "expected an object"
~~~

~~~ haskell
parseArray (Array arr) = mapM parseTuple (V.toList arr)
parseArray _           = fail "expected an array"
~~~

They employ the same pattern: check that `Value` has some type; unwrap if yes, fail if not. This pattern is captured by `with*` functions:

~~~ haskell
withArray :: String -> (Array -> Parser a) -> Value -> Parser a
withArray expected f (Array arr) = f arr
withArray expected f value       = fail "expected ..."
~~~

(There also are [`withObject`][], [`withText`][], [`withScientific`][], and [`withBool`][].)

The extra `String` parameter should be the name of thing you're parsing; `withArray` will use it to generate a nicer error message (e.g. “expected an array of tuples” instead of “expected an array”).

With `withArray` our parser will look as follows:

~~~ haskell
parseArray :: Value -> Parser [(String, Bool)]
parseArray = withArray "array of tuples" $ \arr ->
               mapM parseTuple (V.toList arr)
~~~

### Avoiding manual parsing of primitive types

Take the code we used to parse the string field:

~~~ haskell
  a <- case fieldA of
    String x -> return (T.unpack x)
    _        -> fail "expected a string"
~~~

We can rewrite it with `withText`:

~~~ haskell
  a <- withText "string" $ \x -> return (T.unpack x)
~~~

However, it's still annoying that we have to do all this whenever we need to parse a field of type `String` (which can be quite often). We could write a parser for strings and reuse it:

~~~ haskell
parseString :: Value -> Parser String
parseString = withText "string" $ \x -> return (T.unpack x)
~~~

But we don't need to, because Aeson already defines lots of parsers for popular types (strings, lists, numbers, maps, tuples, and so on) with its [`FromJSON`][] typeclass:

~~~ haskell
class FromJSON a where
  parseJSON :: Value -> Parser a
~~~

~~~ haskell
instance FromJSON String where
  parseJSON = withText "String" (\x -> return (T.unpack x))

instance FromJSON Bool where
  parseJSON = withText "Bool" return

instance FromJSON a => FromJSON [a] where
  parseJSON = withArray "[a]" $ mapM parseJSON . V.toList

...
~~~

Now our `parseTuple` can be simplified quite a bit:

~~~ haskell
import qualified Data.HashMap.Strict as HM

parseTuple = withObject "tuple" $ \obj -> do
  -- Parse "a".
  a <- case HM.lookup "a" obj of
    Just x  -> parseJSON x
    Nothing -> fail "no field 'a'"

  -- Parse "b".
  b <- case HM.lookup "b" obj of
    Just x  -> parseJSON x
    Nothing -> fail "no field 'b'"

  -- That's all!
  return (a, b)
~~~

### Avoiding manual lookups

The `lookup`-and-`parseJSON` pattern can be simplified too, by using the [`.:`][] operator, which does exactly what we are doing for each field:

~~~ haskell
(.:) :: (FromJSON a) => Object -> Text -> Parser a
o .: key = case HM.lookup key o of
             Nothing -> fail ("key " ++ show key ++ " not present")
             Just v  -> parseJSON v
~~~

So, the final form of `parseTuple` is:

~~~ haskell
parseTuple = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)
~~~

Or, if you like the applicative style:

~~~ haskell
parseTuple = withObject "tuple" $ \o ->
               (,) <$> o .: "a"
                   <*> o .: "b"
~~~

### `FromJSON` instances for other types

It'd be nice to be able to use `parseJSON` wherever possible. Instances already exist for many types, however, they aren't documented, so I'm going to document them here. If you want to know where to look for such things, it's in the [`Data.Aeson.Types.Instances`][] module.

  * `()` is `[]` (an empty array).

  * `Bool` is either `true` or `false`.

  * `Char` is a string with a single character in it.

  * `String` and `Text` are strings.

  * Various numeric types are all converted to a number, rounded down if you are converting into an integer (so `"-1.2"` gets decoded as −2). Beware that if you are parsing an `Integer`, someone can write `1e1000000000000` in JSON and your program will eat all your memory.

  * `Rational` is an object with keys `numerator` and `denominator`.

  * Lists and arrays are `[]`-arrays.

  * `Set` is an array too.

  * Tuples of various lengths are arrays. (Moreover, when you parse JSON, you won't be able to parse e.g. `[1,2,3]` as a tuple of length 2.)

  * `Maybe a` is the same as `a`, except that `Nothing` is `null` (which means that you can't distinguish `Nothing` from `Just Nothing`).

  * `Either` is an object with a single key called either `Left` or `Right`.

  * `Map String`, `Map Text`, etc. are objects. (Maps with other types of keys aren't instances at all.)

  * `UTCTime` and `ZonedTime` are represented as strings according to the ISO 8601 standard (specifically, the [ECMA-262][] subset), which every other JSON parser should understand as well. You can also wrap a date into [`DotNetTime`][] to get the non-standard format used by Microsoft.

    [ECMA-262]: http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15

  * `NominalDiffTime` is represented as the number of seconds.

### Records and JSON

(Almost all other Aeson tutorials consist of nothing but this section, which is the reason why people think Aeson is magical – and then they come on IRC and start asking questions about how to do *anything* with Aeson that's not straightforward record-parsing. If you ever decide to write a library, please, please try to prevent this from happening.)

Parsing records is absolutely the same as parsing tuples or whatever else. First get all fields, then put them into a record. You can also make your type an instance of `FromJSON`, to be able to use `decode` instead of `parseMaybe`.

~~~ haskell
-- I'm just going to reuse the example from documentation.
data Person = Person {name :: String, age :: Int}

instance FromJSON Person where
  parseJSON = withObject "person" $ \o ->
    Person <$> o .: "name" <*> o .: "age"
~~~

The reverse is just as easy:

~~~ haskell
instance ToJSON Person where
  toJSON p = object [
    "name" .= name p,
    "age"  .= age  p ]
~~~

### [`RecordWildCards`](@ghc-ext)

There is a slightly different style, however, which I think is better. It relies on the [`RecordWildCards`](@ghc-ext) extension, which does 2 transformations:

* instead of deconstructing the record like `f (Person name age) = ...`, you can write `f Person{..} = ...`

* instead of constructing the record like `Person name age`, you can write `Person{..}`

That's very simple, and very useful.

The code from the previous section, rewritten with `RecordWildCards`:

~~~ haskell
{-# LANGUAGE RecordWildCards #-}

data Person = Person {name :: String, age :: Int}

instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    name <- o .: "name"
    age  <- o .: "age"
    return Person{..}

instance ToJSON Person where
  toJSON Person{..} = object [
    "name" .= name,
    "age"  .= age  ]
~~~

### Handling extra fields

This should be obvious, but I'll say again just in case: extra fields would be ignored by your parsing functions (if it's not obvious, reread previous sections, because it *should* be obvious – from the first example, where we were doing everything by hand – that it's not possible for the parser to find out about “extra” fields in principle).

You can, of course, check for extra fields by yourself if you want to. Just use [`keys`][] on the object to get a list of fields it has, and then check (with [`\\`][], for instance) whether there are any fields you don't expect there to be.

### Postprocessing

The style that uses `do` is better because it makes postprocessing easier. For instance, imagine that records have fields `name` and `surname`, but you want to combine them when parsing the record:

~~~ haskell
instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    firstName <- o .: "name"
    lastName  <- o .: "surname"
    let name = firstName ++ " " ++ lastName
    age       <- o .: "age"
    return Person{..}
~~~

You could do it in applicative style as well, but it'd be a bit awkward.

### Optional fields

A common situation is that some field can be optional, and if it's not present you want to provide a default value. I'm not sure whether it's *actually* that common, but Aeson provides some operators specifically for this case anyway: [`.:?`][] and [`.!=`][].

~~~ haskell
(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)

(.!=) :: Parser (Maybe a) -> a -> Parser a
~~~

`.:?` simply returns `Nothing` if the field wasn't found or was a `null`. Keep in mind that if the field *was* found but had a wrong type, it will fail just like `.:`.

`.!=` takes a parser returning `Maybe a` and supplies it with some default value to be returned if the parser returns `Nothing`.

They can be used together like this:

~~~ haskell
instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    name <- o .:  "name"
    age  <- o .:? "age" .!= 18
    return Person{..}
~~~

If there's a field which is optional and you want to put `Nothing` there even if the field *is* present but couldn't be parsed properly (because of type mismatch, for instance), use [`optional`][]:

~~~ haskell
    ...
    age <- optional (o .: "age")
    ...
~~~

### More interesting choices

What if you wanted to take age from the `age` field, and if it's not found – from the `AGE` field (because some other software you're using behaves weirdly), and give up only if both fields aren't present? To do this, we can use [`Alternative`][], which provides the [`<|>`][] operator. `a <|> b` can be read as “if `a` doesn't fail, then `a`, otherwise `b`”.

An example:

~~~ haskell
import Control.Applicative

instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    name <- o .: "name"
    age  <- o .: "age" <|> o .: "AGE"
    return Person{..}
~~~

You could choose from a list of parsers, even, with the [`asum`][] function from [`Data.Foldable`][] (which is generalised `choice` from various parsing libraries). Here's how you could deal with age which could be:

* in the `age` field, as a number
* in the `age` field, as a string
* in the `AGE` field, as a tuple consisting of “years” and “months”
* not present but we know that if the name is “John” then it's 24

~~~ haskell
import Control.Applicative
import Control.Monad (when)
import Text.Read (readMaybe)

instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    name <- o .: "name"
    age  <- asum [
      -- The simple “number” case.
      o .: "age",
      -- The more complicated “string” case.
      do s <- o .: "age"
         case readMaybe s of
           Nothing -> fail "not a number"
           Just x  -> return x,
      -- The “tuple” case.
      fst <$> o .: "AGE",
      -- The “John” case.
      do guard (name == "John")
         return 24 ]
    return Person{..}
~~~

If you don't want to use `fail`, you can use [`empty`][], which is equivalent to `fail "empty"`. There's also [`guard`][] in [`Control.Monad`][], which fails unless some condition is met, and [`when`][], which does something conditionally. The following pieces of code do the same thing:

~~~ haskell
do guard (name == "John")
   return 24
~~~

~~~ haskell
do when (name /= "John") $ fail "mzero"
   return 24
~~~

~~~ haskell
if name == "John"
  then return 24
  else fail "mzero"
~~~

Or, for instance, let's say that you consider a record invalid if the name is “Ann”, because you hate your friend Ann. Then you could simply use `when` to fail whenever Ann appears in your data:

~~~ haskell
instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    name <- o .: "name"
    when (name == "Ann") $
      fail "GO AWAY ANN"
    age  <- o .: "age"
    return Person{..}
~~~

### Types with many constructors

If you have a type with several constructors, such as this one:

~~~ haskell
data Something
  = Person {name :: String, age :: Int}
  | Book {name :: String, author :: String}
~~~

there are several ways to encode it. For instance, you could just use the fact that fields have different names:

~~~ haskell
instance FromJSON Something where
  parseJSON = withObject "book or person" $ \o -> asum [
    Person <$> o .: "name" <*> o .: "age",
    Book <$> o .: "name" <*> o .: "author" ]
~~~

Or you could add a separate field denoting the kind of thing encoded:

~~~ haskell
instance FromJSON Something where
  parseJSON = withObject "book or person" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "person" -> Person <$> o .: "name" <*> o .: "age"
      "book"   -> Book <$> o .: "name" <*> o .: "author"
      _        -> fail ("unknown kind: " ++ kind)
~~~

### Nested records

Say, you have JSON looking like this:

~~~ json
{
    "name":"Nightfall",
    "author":{
        "name":"Isaac Asimov",
        "born":1920
    }
}
~~~

However, you don't like nested records and you want to convert it into this:

~~~ haskell
data Story = Story {
  name       :: String,
  author     :: String,
  authorBorn :: Int }
~~~

Doing this is rather simple; just use the fact that you can use `.:` to get an object out, and then use `.:` on it again. An example:

~~~ haskell
instance FromJSON Story where
  parseJSON = withObject "story" $ \o -> do
    name <- o .: "name"
    -- authorO :: Object
    authorO <- o .: "author"
    -- Now we can deconstruct authorO.
    author     <- authorO .: "name"
    authorBorn <- authorO .: "born"
    -- And finally return the value.
    return Story{..}
~~~

### Extended records

Another interesting case is extended records – that is, when one datatype is a subset of another:

~~~ haskell
data Name = Name {
  name    :: String,
  surname :: String }

data RussianName = RussianName {
  russianName       :: Name,
  russianPatronymic :: String }
~~~

(In case you wonder: [patronymic](@w:Patronymic#Russian).)

So, `RussianName` is just a `Name` with patronymic added to it. However, we also want `RussianName` to turn into this JSON (without nestedness):

~~~ json
{
    "name":"Сергей",
    "patronymic":"Михайлович",
    "surname":"Брин"
}
~~~

How can we do this?

First, obvious instances for `Name` (which can actually be derived generically, as we'll see in one of the following sections):

~~~ haskell
instance FromJSON Name where
  parseJSON = withObject "name" $ \o -> do
    name    <- o .: "name"
    surname <- o .: "surname"
    return Name{..}

instance ToJSON Name where
  toJSON Name{..} = object [
    "name"    .= name,
    "surname" .= surname ]
~~~

And now, the `FromJSON` instance for `RussianName` shall exploit the fact that Aeson parsers ignore extra fields, by parsing the *same* structure simultaneously as `Name` and `RussianName`:

~~~ haskell
instance FromJSON RussianName where
  parseJSON = withObject "name" $ \o -> do
    russianName       <- parseJSON (Object o)
    russianPatronymic <- o .: "patronymic"
    return RussianName{..}
~~~

The `ToJSON` instance is less peculiar – it just concatenates hashmaps:

~~~ haskell
instance ToJSON RussianName where
  toJSON RussianName{..} = Object $
    toObject russianName <>
    fromList ["patronymic" .= russianPatronymic]

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"
~~~

That's where the understanding of Aeson's inner model pays off.

### Unknown field names

Let's say you have the following data:

~~~ json
{
    "website1.com": {
        "/page1": 3,
        "/page2": 4
    },
    "website2.com": {
        "/page": 10
    }
}
~~~

And you want to parse it into a list of `Referer`s, where a `Referer` is something like this:

~~~ haskell
data Referer = Referer {
  domain       :: String,
  pathAccesses :: [(String, Int)] }
  deriving (Show)
~~~

How could you do that? `.:` won't work because you don't know the field names. So, what to do?

One solution would be using `parseJSON` to parse our object as a `HashMap` of `HashMap`s of `Int`s, and then apply a simple function to turn it into a list of `Referer`s:

~~~ haskell
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

parseReferers :: Value -> Parser [Referer]
parseReferers p =
  -- Convert each “accesses” object to a list of pairs, and create a Referrer.
  map (\(domain, accesses) -> Referer domain (HM.toList accesses)) .
  -- Turn the HashMap into a list of (domain, accesses) pairs.
  -- Each “accesses” object looks like {"/page1": 3, ...}.
  HM.toList <$>
  -- Parse our data into a HashMap String (HashMap String Int).
  parseJSON p
~~~

Another solution involves processing the `Object` (i.e. a `HashMap Text Value`) directly – this way we can avoid the intermediate step of converting a `HashMap Text` to a `HashMap String`, which makes the code faster:

~~~ haskell
import Data.Traversable
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

parseReferers :: Value -> Parser [Referer]
parseReferers =
  -- We're expecting an object: {"website1.com": {...}, ...}
  withObject "referers" $ \o ->
    -- Now we have 'o', which is a HashMap. We can use HM.toList to turn it
    -- into a list of pairs (domain, referer) and then parse each referer:
    for (HM.toList o) $ \(domain, referer) -> do
      -- accesses :: [(Text, Int)]
      accesses <- HM.toList <$> parseJSON referer
      -- accesses' :: [(String, Int)]
      let accesses' = map (\(page, n) -> (T.unpack page, n)) accesses
      return $ Referer {
        domain       = T.unpack domain,
        pathAccesses = accesses' }
~~~

### Records and JSON: generics

When you don't care about doing any postprocessing, or handling optional fields, or anything of the sort, you can use generics to make your types instances of `ToJSON` and `FromJSON` without writing *any* boilerplate code, which is really nice. (This trick depends on GHC knowing the internals of your data types and willing to share them with Aeson.)

~~~ haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson

data Person = Person {
  name :: String,
  age  :: Int }
  deriving (Generic)

instance ToJSON Person
instance FromJSON Person
~~~

Starting from GHC 7.10, it's possible to simplify this even further by using the [`DeriveAnyClass`](@ghc-ext) extension (which still requires a `Generic` instance):

~~~ haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics
import Data.Aeson

data Person = Person {
  name :: String,
  age  :: Int }
  deriving (Generic, ToJSON, FromJSON)
~~~

### Generics: customising field names

You may still want to be able to control the way field names are treated. If you're using lenses, for instance, you might have your fields prefixed with “_”, but you don't want those underscores to appear in JSON. Aeson provides functions [`genericParseJSON`][] and [`genericToJSON`][], which use generics as well but also take an [`Options`][] parameter which lets you customise some things. For instance, the [`fieldLabelModifier`][] field stores a function to be applied to field names:

~~~ haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

data Person = Person {
  _name :: String,
  _age  :: Int }
  deriving (Generic)

instance ToJSON Person where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }
~~~

~~~ {.haskell .repl}
> import qualified Data.Text.Lazy.IO as T
> import qualified Data.Text.Lazy.Encoding as T

> T.putStrLn . T.decodeUtf8 . encode $ Person "John" 24
{"age":24,"name":"John"}
~~~

Or you could use the [`camelTo`][] function to turn names like `personName` into `person_name`. Or you could even use [`lookup`][] (together with [`fromJust`][], I guess) to provide your own mapping from field names to JSON fields.

There are other options available, which let you specify e.g. how types with several constructors are encoded, but for that just read the documentation on [`Options`][].

Finally, this approach works just as well on datatypes which aren't records – `data Person = Person String Int`, for instance – but then your types would be encoded as arrays instead of objects.

### Generics: handling weird field names in data

Sometimes you want to do the reverse thing: your field names are alright, but the field names in data aren't (for instance, some come uppercased and some come lowercased) and yet you don't want to switch back to manual parsing from generics. This problem is easy to solve if you know how to manipulate `Value`s; basically, the whole solution is “let's first bring all fields in our data to the same standard and then tell the generic decoder to use that standard”.

As an example, here's a case-insensitive JSON decoder which doesn't care whether it's “name” or “NAME” or “nAMe”:

~~~ haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Char (toLower)
import qualified Data.Text as T

import qualified Data.HashMap.Strict as HM

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

data Person = Person {
  pName :: String,
  pAge  :: Int }
  deriving (Generic)

-- “jsonLower” means that all fields in data would be converted to lowercase.
-- “fieldLabelModifier = map toLower . drop 1” means that the decoder would
-- *expect* fields to correspond to their lowercase counterparts – e.g. it
-- would look for “name” when filling the “pName” field.
instance FromJSON Person where
  parseJSON = genericParseJSON opts . jsonLower
    where
      opts = defaultOptions { fieldLabelModifier = map toLower . drop 1 }

-- | Turn all keys in a JSON object to lowercase.
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x
~~~

### Generics: optional fields

When a field has type `Maybe ...`, the autogenerated instance would use `.:?` for that field – that is, autogenerated instances replace missing fields with `Nothing`. (Telling just in case, since it doesn't seem to be documented in Aeson's docs.)

### Records and JSON: Template Haskell

This does the same thing generics do, but in a different way. The only thing that differs is performance (I ran a benchmark and in some cases TH was twice as fast as generics).

Note: with Template Haskell enabled, GHC suddenly becomes picky about the order of your declarations (anywhere in the module), and in some cases it's rather annoying.

Documentation in [`Data.Aeson.TH`][] is pretty good, so I'll just give an example:

~~~ haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH

data Person = Person {
  name :: String,
  age  :: Int }

deriveJSON defaultOptions ''Person
~~~

You can also generate instances for several types at once:

~~~ haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH

data Person = Person {
  name :: String,
  age  :: Int }

data Book = Book {
  title  :: String,
  author :: String }

concat <$> mapM (deriveJSON defaultOptions) [''Person, ''Book]
~~~

### Parsing without creating extra types

(This was already kinda discussed in one of the past sections, but I want to provide the whole solution so that I can link people to it.)

You might want to parse some data without creating types for *everything*. For instance, you have an array of people, but that array is wrapped for whatever stupid reason in an object:

~~~ json
{
    "data":[
        {
            "name":"Harold",
            "age":20
        },
        {
            "name":"Maude",
            "age":80
        }
    ]
}
~~~

You have an instance of `FromJSON` for `Person`:

~~~ haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson

data Person = Person {
  name :: String,
  age  :: Int }
  deriving (Generic)

instance FromJSON Person
~~~

However, you don't want to create another type for `PersonArray` or something. You just want to decode that thing into a list. How could you do it?

First, we might not have to create a type but we'd still have to write a parser:

~~~ haskell
people :: Value -> Parser [Person]
people = withObject "people" $ \o -> o .: "data"  -- or just (.: "data")
~~~

Then, you can decode your JSON as a `Value`, and use [`parseMaybe`][] to apply the `people` parser to it:

~~~ haskell
parseMaybe people =<< decode bs
~~~

That's kinda all.

### Pretty-printing

(Strictly speaking, this isn't a part of Aeson, but it doesn't matter.)

If you are using Aeson to work with e.g. config files, you might want to make them human-readable. (Well, actually I think you *must* make them human-readable, but maybe you have really good reasons not to... so I won't insist.) Aeson doesn't have a function to print JSON in a human-readable format, but there is a function in [aeson-pretty](@hackage), and it's really easy to use, and that's why I won't spend much time explaining it.

You just import [`Data.Aeson.Encode.Pretty`][] and use [`encodePretty`][] instead of `encode` in your code. That's all. The resulting JSON would be nicely indented and everything.

There are, however, some things you can change about generated JSON. For instance, change indentation to 8 spaces and move the `id` field to the beginning of all objects that have it:

~~~ haskell
encodePretty' (Config 8 (keyOrder ["id"])) ...
~~~

(If you want more examples of [`keyOrder`][] usage, see the docs.)

One last thing: by default your fields in the output won't be sorted as they are in your `data` declaration – instead, they will be sorted somewhat randomly. Unfortunately, there's no way to change it other than hardcoding the field order with `keyOrder`.

### Strictness

Just for completeness, I'll also mention that there is function called [`decode'`][] which performs strict decoding of JSON, and that the documentation claims that you should use `decode` when you don't plan to access all of parsed data, and `decode'` – when you do. I haven't checked, but I believe the author.


Comments
--------

There are some comments on [Reddit][Reddit comments]. In particular, you can find there the author of Aeson bashing this tutorial (and me disagreeing).

[Reddit comments]: http://www.reddit.com/r/haskell/comments/324vrx/aeson_the_tutorial_which_actually_tries_to/

[`.!=`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:.-33--61-
[`.:?`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:.:-63-
[`.:`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:.:
[`.=`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:.-61-
[`<|>`]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html#v:-60--124--62-
[`Alternative`]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative
[`Applicative`]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Applicative
[`ByteString`]: http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Lazy.html#t:ByteString
[`Control.Monad`]: http://hackage.haskell.org/package/base/docs/Control-Monad.html
[`Data.Aeson.Encode.Pretty`]: https://hackage.haskell.org/package/aeson-pretty/docs/Data-Aeson-Encode-Pretty.html
[`Data.Aeson.TH`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-TH.html
[`Data.Aeson.Types`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html
[`Data.Aeson.Types.Instances`]: http://hackage.haskell.org/package/aeson/docs/src/Data-Aeson-Types-Instances.html
[`Data.ByteString.Lazy.UTF8`]: http://hackage.haskell.org/package/utf8-string/docs/Data-ByteString-Lazy-UTF8.html
[`Data.ByteString.Lazy.readFile`]: http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Lazy.html#v:readFile
[`Data.Foldable`]: http://hackage.haskell.org/package/base/docs/Data-Foldable.html
[`Data.Text.Lazy.Encoding`]: http://hackage.haskell.org/package/text/docs/Data-Text-Lazy-Encoding.html
[`DotNetTime`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:DotNetTime
[`FromJSON`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:FromJSON
[`GHC.Exts`]: http://hackage.haskell.org/package/base/docs/GHC-Exts.html
[`HashMap`]: http://hackage.haskell.org/package/unordered-containers/docs/Data-HashMap-Strict.html#t:HashMap
[`Options`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#t:Options
[`Read`]: http://hackage.haskell.org/package/base/docs/Prelude.html#t:Read
[`Scientific`]: http://hackage.haskell.org/package/scientific/docs/Data-Scientific.html#t:Scientific
[`Show`]: http://hackage.haskell.org/package/base/docs/Prelude.html#t:Show
[`String`]: http://hackage.haskell.org/package/base/docs/Prelude.html#t:String
[`Text`]: http://hackage.haskell.org/package/text/docs/Data-Text.html#t:Text
[`ToJSON`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:ToJSON
[`Vector`]: http://hackage.haskell.org/package/vector/docs/Data-Vector.html#t:Vector
[`\\`]: http://hackage.haskell.org/package/base/docs/Data-List.html#v:-92--92-
[`asum`]: http://hackage.haskell.org/package/base/docs/Data-Foldable.html#v:asum
[`camelTo`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#v:camelTo
[`decode'`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:decode-39-
[`decodeUtf8`]: http://hackage.haskell.org/package/text/docs/Data-Text-Lazy-Encoding.html#v:decodeUtf8
[`decode`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:decode
[`eitherDecode`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:eitherDecode
[`empty`]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html#v:empty
[`encodeUtf8`]: http://hackage.haskell.org/package/text/docs/Data-Text-Lazy-Encoding.html#v:encodeUtf8
[`encode`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:encode
[`encodePretty`]: https://hackage.haskell.org/package/aeson-pretty/docs/Data-Aeson-Encode-Pretty.html#v:encodePretty
[`fail`]: http://hackage.haskell.org/package/base/docs/Prelude.html#v:fail
[`fieldLabelModifier`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#v:fieldLabelModifier
[`fromJust`]: http://hackage.haskell.org/package/base/docs/Data-Maybe.html#v:fromJust
[`fromList`]: http://hackage.haskell.org/package/base/docs/GHC-Exts.html#v:fromList
[`fromString`]: http://hackage.haskell.org/package/utf8-string/docs/Data-ByteString-Lazy-UTF8.html#v:fromString
[`genericParseJSON`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:genericParseJSON
[`genericToJSON`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:genericToJSON
[`guard`]: http://hackage.haskell.org/package/base/docs/Control-Monad.html#v:guard
[`keyOrder`]: https://hackage.haskell.org/package/aeson-pretty/docs/Data-Aeson-Encode-Pretty.html#v:keyOrder
[`keys`]: http://hackage.haskell.org/package/unordered-containers/docs/Data-HashMap-Strict.html#v:keys
[`lookup`]: http://hackage.haskell.org/package/base/docs/Prelude.html#v:lookup
[`mapM`]: http://hackage.haskell.org/package/base/docs/Control-Monad.html#v:mapM
[`object`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:object
[`optional`]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html#v:optional
[`parseJSON`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:parseJSON
[`parseMaybe`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#v:parseMaybe
[`readFile`]: http://hackage.haskell.org/package/base/docs/Prelude.html#v:readFile
[`readMaybe`]: http://hackage.haskell.org/package/base/docs/Text-Read.html#v:readMaybe
[`toString`]: http://hackage.haskell.org/package/utf8-string/docs/Data-ByteString-Lazy-UTF8.html#v:toString
[`type Object`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:Object
[`type Array`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:Array
[`type Value`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:Value
[`value`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Parser.html#v:value
[`when`]: http://hackage.haskell.org/package/base/docs/Control-Monad.html#v:when
[`withArray`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:withArray
[`withBool`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:withBool
[`withObject`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:withObject
[`withScientific`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:withScientific
[`withText`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:withText
[`Parser`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#t:Parser
[`Result`]: http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#t:Result

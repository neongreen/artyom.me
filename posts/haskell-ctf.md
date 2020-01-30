% Making a CTF task in Haskell

2 weeks ago I got approached by a person on [oDesk][] who asked me to design
a task in Haskell for some [CTF][] (“capture the flag”) competition (I never
found out, which). The specific requirement was to write a safe Haskell
interpreter (lambdabot on #haskell is an example of such an interpeter – it
disallows IO, forbids any unsafe functions, etc.) which could be broken – for
instance, the attacker should've been able to subvert the restrictions and
read a “flag” file in a specific folder. This type of challenges is commonly
known as a “sandbox” or “jail” – here's an [example in Python][pyjail].

[oDesk]: https://www.odesk.com/jobs/Haskell-programmer-required-develop-sandbox_~0169dd7db924c8f0c5
[CTF]: http://en.wikipedia.org/wiki/Capture_the_flag#Computer_security
[pyjail]: http://nullify-ctf.blogspot.com.au/2014/01/phd-ctf-quals-2014-yet-another-pyjail.html

The challenge wasn't aimed at expert or even amateur Haskell programmers, so
relying on -things which can be googled, guessed or deduced- was preferable,
which was good for me, because I've never participated in any CTFs and hacker
competitions and have no idea how to create hard tasks. By the way, do you
know the 4 properties of a good programming competition task set?

  1. Everybody must have solved at least 1 task.
  2. Nobody should have solved all tasks.
  3. Each task must have been solved by somebody.
  4. No task should have been solved by everybody.

(The 0th – implicit – property is “afterwards, nobody thinks the tasks
sucked”.)

# The sandbox

Before I describe the process behind its creation, I'd like to post the code
of the sandbox itself. Can you break it?

If you don't want to read a couple of screens of code, it's fine – click this
link to [skip it](#the-design-process). You're not losing anything; the code
is here only for those who want to try to solve it first.

~~~ haskell
-- Instructions: you can give the interpreter any code (in Haskell, of
-- course) to be executed; the interpreter will compute the value of
-- top-level declaration `answer :: String` and print it to you. The goal is
-- to learn the contents of file called “flag”, which is contained in the
-- same directory where the interpreter is running from. Good luck!

module Main where

import Control.Monad (when, unless)
import System.IO (openTempFile, hPutStr, hFlush, hClose)
import System.Directory (getTemporaryDirectory, doesFileExist)
import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Language.Haskell.Interpreter  -- from hint package
import Text.Regex                    -- from regex-compat-tdfa package

-- Case-insensitive regex matching.
(~~) :: String -> String -> Bool
regex ~~ s = isJust $ matchRegex (mkRegexWithOpts regex True False) s

main = do
  flagExists <- doesFileExist "flag"
  unless flagExists $
    error "flag file not found!"

  putStrLn "Get the ./flag!"
  putStrLn ""
  putStrLn "Your code here (finish with Ctrl-D):"
  putStrLn ""
  code <- getContents

  tempDir <- getTemporaryDirectory
  (codeFile, codeHandle) <- openTempFile tempDir "code.hs"
  hPutStr codeHandle code
  hFlush codeHandle
  hClose codeHandle

  -- Bye-bye, non-printables.
  when (any (\x -> isControl x && x `notElem` "\n\r\t") code) $
    error "no control characters are allowed. They are scary. Rejected."

  -- Multiline comments are okay, but we forbid then anyway – hard to parse.
  -- Also, this has disallowing scary pragmas as a side-effect.
  when ("{-|-}" ~~ code) $
    error "multiline comments / scary pragmas were found. Rejected."

  -- Strip single-line comments.
  let codeWC = subRegex (mkRegex "--.*$") code ""

  -- Disallow strings, characters and numbers.
  when ("[\"'[:digit:]]" ~~ codeWC) $
    error "fun fact: 100% of bad programs contain text/numbers. Rejected."

  -- Let's keep it short. Notice that operators count as words, too.
  putStr "Counting words... "
  let delim = mkRegex "[][{}()|,;=`[:space:]]"
  let wordCount = length $ filter (not . null) $ splitRegex delim codeWC
  print wordCount

  when (wordCount > 3) $
    error "for Haskell, this is way too long. And scary. Rejected."

  -- Of course, everything unsafe is bad. (Various coercions are bad too, not
  -- to mention immoral.)
  when ("unsafe" ~~ codeWC) $
    error "code mentions “unsafe”... kinda suspicious, y'know. REJECTED."
  when ("coerce" ~~ codeWC) $
    error "don't coerce! Coercions aren't nice. Rejected."

  -- Whitelisting modules is bo-oring... so we'll just ban everything which
  -- isn't “Data”. “qualified” is forbidden merely to make parsing easier.
  when ("qualified" ~~ codeWC) $
    error "qualified imports are disabled for the Greater Good. Rejected."

  let extractImports (x:y:xs)
        | x == "import" = y : extractImports (y:xs)
        | otherwise     =     extractImports (y:xs)
      extractImports _  = []
  let imports = extractImports (words codeWC)

  unless (all ("Data." `isPrefixOf`) imports) $
    error "only Data.* imports are allowed. Others are scary. Rejected."

  -- Okay, time to run the interpreter!
  res <- runInterpreter $ do
    loadModules [codeFile]
    setTopLevelModules ["Main"]
    setImportsQ [("Prelude", Nothing)]
    interpret "answer" (as :: String)
  case res of
    Left  err -> putStrLn $ "Interpretation error: " ++ show err
    Right ans -> putStrLn $ "answer = " ++ ans
~~~

# The design process

(I'm going to write in the present tense, even tho it all happened 2 weeks
ago.)

Okay, sandbox. Hm. Sandbox. There's [mueval][GH mueval], which does exactly
what we want – “unfortunately”, it does it too well. There are no known
vulnerabilities in mueval, and it would be hard to introduce some without
changing its source. But at least I know already what I'm going to use to
interpret Haskell – [hint](https://hackage.haskell.org/package/hint) (a
wrapper over GHC API).

[GH mueval]: https://github.com/gwern/mueval

Other than that, no ideas. Darn. Beginning to panic... No, mustn't panic –
once I start coding, something will come up, so I can tell the client
something vague like

> So, I think I'm going to start writing a simple sandbox of my own now
> (quite more restricted than mueval) based on hint, and after having written
> a prototype and toying with it a bit some non-obvious way to break it
> should emerge.

and set to work. (Just in case: I *did* confess that I had no ideas
immediately after that. I'm not that bad.)

## Vague idea #1: generics?

It should be possible to specify which packages/modules are in scope, right?
What if there are several packages which are safe by themselves but unsafe
when combined somehow?

For instance, imagine there's a package for doing file manipulation, which
uses smart constructors:

~~~ haskell
-- File is an opaque data type.

fileInCurrentDirectory :: String -> Maybe File
fileInCurrentDirectory = ...

openFile :: File -> String
openFile = ...
~~~

Then, if only `fileInCurrentDirectory` and `openFile` are available for the
users of the interpreter, they can't open any files which aren't in the
current directory; but if they also have access to [generics][`Data.Data`],
they can in theory replace the stored file path in abstract `File` datatype,
even if actual constructors aren't exported. For example, this evil function
replaces `Int`s (only the ones on the “first level”) with `666`:

~~~ haskell
import Data.Data
import Data.Maybe (fromJust)

evil :: Data a => a -> a
evil = gmapT f
  where
    f d = case cast d :: Maybe Integer of
            Nothing -> d
            Just _  -> fromJust (cast (666 :: Integer))
~~~

Usage:

~~~ haskell
> evil (Just 0)
Just 666

> evil [1..3]
[666, 2, 3]
~~~

(If you want to learn more about [`Data.Data`] and [`Data.Typeable`], I
recommend [this Chris Done's article][CD Data Typeable].)

[CD Data Typeable]: http://chrisdone.com/posts/data-typeable

## Vague idea #2: `inlinePerformIO`

Everybody knows about [`unsafePerformIO`][], right? The CTF is going to be
aimed at people who aren't Haskell experts, but even those can easily google
`unsafePerformIO`. There is another just as “interesting” (for our purpose)
function – [`unsafeDupablePerformIO`][] – but it's in the same module, so
it's guaranteed to be discovered as well.

Wa-ait... I remember stumbling upon *another* function while reading the
source of [bytestring](https://hackage.haskell.org/package/bytestring) –
[`inlinePerformIO`][]. It might be just perfect!

  * Exported by an `Internal` module (those usually contain functions to
    access internals of a datatype, not dangerous stuff like
    `inlinePerformIO`).

  * Contained in a library which ships with GHC (so I won't have to find any
    excuses to pull it in).

  * Not accessible by browsing documentation (the page for the
    [module exporting it][`Data.ByteString.Internal`] says simply “Sorry,
    it's just not here”).

  * Doesn't have “unsafe” in its name.

  * And yet *is* googleable by a determined person (it's mentioned in the
    [IO Inside][] article).

[IO Inside]: http://www.haskell.org/haskellwiki/IO_inside#inlinePerformIO

Okay, but a task based on merely googling `inlinePerformIO` wouldn't be any
interesting, right? More ideas are needed.

## Vague idea #3: Unicode spaces

A while ago I accidentally found that GHC doesn't mind [non-breaking spaces](https://en.wikipedia.org/wiki/Non-breaking_space) in code. On one hand, it's a rare and slightly weird feature – from TIOBE's index of [top 20 languages][TIOBE top] only C#, Node.js, VB.NET and Dart allow non-breaking spaces instead of normal ones. On the other hand, what use can there possibly be for it in a CTF task?

[TIOBE top]: http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html

Perhaps I can add a restriction to the interpreter – “you can't use more than N words/lexemes”, where N is less than the minimum amount of words actually needed to import `Data.ByteString.Internal` and read the file. If the word counter forgets about non-breaking spaces being valid word separators, it would be possible to use them to circumvent the length limit.

Or perhaps I can write an flawed import list parser which disallows almost all imports but doesn't parse non-breaking spa— no, it would let the attacker import *anything whatsoever*, and this isn't acceptable.

## Vague idea #4: constant generation

This isn't even an “idea” as much as a “wish”, but still... It would be nice
if the attacker had to find a Pretty Clever Way to generate some constant
before they can read the file. Using tricky math or something.

Hm.

Would be *really* nice to have a function for checking code cleverness. Just
think about it – the ultimate cleverness challenge:

    enter your code >

    ...

    *** Exception: Sorry, your code isn't clever enough!

A pity I don't know any ways to estimate cleverness
([Kolmogorov complexity](https://en.wikipedia.org/wiki/Kolmogorov_complexity) doesn't count, as it's not computable).

# Coding

I still haven't got a clear “vision” of what I'm going to write, but...

~~~ haskell
main = do
~~~~

## The boring stuff

Checking for the flag file:

~~~ haskell
  flagExists <- doesFileExist "flag"
  unless flagExists $
    error "flag file not found!"
~~~

Getting the code from standard input (participants were supposed to connect
by SSH to a server with -the interpreter- being set up as shell):

~~~ haskell
  putStrLn "Get the ./flag!"
  putStrLn ""
  putStrLn "Your code here (finish with Ctrl-D):"
  putStrLn ""
  code <- getContents
~~~

Moving received code to a temporary file (hint can only load files, not
strings from memory (or I just haven't found a way to do it)):

~~~ haskell
  tempDir <- getTemporaryDirectory
  (codeFile, codeHandle) <- openTempFile tempDir "code.hs"
  hPutStr codeHandle code
  hFlush codeHandle
  hClose codeHandle
~~~

## Setting up regexes

Usually, regexes aren't used in Haskell – we have [`Data.Char`][] and
[list functions][`Data.List`] for simple cases, and parsing combinator
libraries for more complex ones. However, since CTF participants (my “target
demographics”) use regexes often, I'm going to use them too – especially
since the sandbox code should be concise and regexes are somewhat more
concise than parsers.

There are many libraries for working with regexes on Hackage.
[regex-tdfa](https://hackage.haskell.org/package/regex-tdfa) seems to be the
best one for POSIX regexes, at least judging by [this detailed writeup][HW
regex posix] by its author which describes the ways in which all other
libraries suck. However, I'm going to use
[regex-compat-tdfa](https://hackage.haskell.org/package/regex-compat-tdfa)
instead – it's a simple wrapper over regex-tdfa providing several
easy-to-use functions. If I don't need the full power of regex-tdfa, why
not?

[HW regex posix]: http://www.haskell.org/haskellwiki/Regex_Posix

In particular, these are the functions I'm going to use [hooray for precognition]:

  * `mkRegex :: String -> Regex` – make a case-sensitive regex out of a
    string.

  * `mkRegexWithOpts :: Bool -> Bool -> String -> Regex` – same as `mkRegex`,
    but the second parameter controls case-sensitivity.

  * `matchRegex :: Regex -> String -> Maybe [String]` – matche a regex and
    returns submatches.

  * `subRegex :: Regex -> String -> String -> String` – perform a regex
    replacement.

  * `splitRegex :: Regex -> String -> [String]` – split a string on
    delimiters matched by the regex.

And, for Even More Convenience, I'll add an operator for case-insensitive
matching:

~~~ haskell
(~~) :: String -> String -> Bool
regex ~~ s = isJust $ matchRegex (mkRegexWithOpts regex True False) s
~~~

Okay, we're set.

## Removing control characters

I've no idea whether it can actually influence the solution or not, but
better safe than sorry. What can control characters do? What dangers lurk in
the depths of ASCII? I don't know, and I'm scared.

~~~ haskell
  when (any (\x -> isControl x && x `notElem` "\n\r\t") code) $
    error "no control characters are allowed. They are scary. Rejected."
~~~

## Handling comments

Are you wondering why do anything about comments at all?

  * People should have a chance to comment their solutions adequately (nobody
    is probably going to do this anyway, but still), which means that
    comments shouldn't count towards word totals.

  * Technically, comments are just whitespace for the compiler – it
    complicates code parsing (e.g. if I want to disallow certain imports,
    I'll have to parse them first, and I don't want to deal with comments
    while doing this).

This is enough of a justification for me. `{- ... -}` comments can be nested
and I don't know how to strip them with regexes, so I'm just banning them
(and language pragmas, which can (probably) be used to do something bad, get
automatically banned as well). `--` comments are nicer (the corresponding
regex is `--.*$`, which means “`--` and then any characters until the end of
the line”), so they get to stay.

~~~ haskell
  when ("{-|-}" ~~ code) $
    error "multiline comments / scary pragmas were found. Rejected."

  let codeWC = subRegex (mkRegex "--.*$") code ""
~~~

(All subsequent checks and operations will be done on `codeWC`.)

## Enforcing safety

All functions with “unsafe” in their names shall be considered unsafe:

~~~ haskell
  when ("unsafe" ~~ codeWC) $
    error "code mentions “unsafe”... kinda suspicious, y'know. REJECTED."
~~~

Well, and there's also [`coerce`][]. It's *supposed* to be safe (or so I
heard), but I'm-lazy-to-do-research, so:

~~~ haskell
  when ("coerce" ~~ codeWC) $
    error "don't coerce! Coercions aren't nice. Rejected."
~~~

## Parsing imports

Banning all mentions of “unsafe” still leaves a great deal of
suspicious-looking stuff in `GHC.*` hierarchy. I also don't trust
`Foreign.*`... and `System.*`... you know, it's easier to just say what I
(more-or-less) trust – `Data.*`. So, the plan is to check whether any module
not from the `Data.*` hierarchy is imported, and if so, complain. (Perhaps it
would also serve as a hint as to where to look – or at least discourage the
participants from spending an hour browsing the docs for the whole base
library.)

The easiest way is to gather a list of all words following the keyword
`import` in code (I think it's actually a valid approach, as `import` can't
be a function or variable name) and look at their prefixes. This is somewhat
spoiled by the fact that `import` can be followed by `qualified`, so— no,
I'll just ban `qualified`. (Ye-e-eah, you've probably already noticed that my
attitude can be summarised as “it's a CTF task, it doesn't have to be sane or
sensible”.)

~~~ haskell
  when ("qualified" ~~ codeWC) $
    error "qualified imports are disabled for the Greater Good. Rejected."
~~~

To extract imports, I could use a simple `zip`:

~~~ haskell
  let imports = map snd . filter ((== "import") . fst)
              . (zip <$> id <*> tail) . words
~~~

But... Well, okay, `zip <$> id <*> tail` could be replaced by `\ws -> zip ws
(tail ws)`, yeah, but it's still a bit too dense. Something like this would
be more easily understandable (probably):

~~~ haskell
  let extractImports (x:y:xs)
        | x == "import" = y : extractImports (y:xs)
        | otherwise     =     extractImports (y:xs)
      extractImports _  = []
  let imports = extractImports (words codeWC)
~~~

Phew, I've got a list of imports, now I can use it to Keep Evil Stuff Off:

~~~ haskell
  unless (all ("Data." `isPrefixOf`) imports) $
    error "only Data.* imports are allowed. Others are scary. Rejected."
~~~

## Counting words

It can be easily done using only `elem` and a list of “separator” characters,
but I'm going to use a regex instead. Why? Well, I can't use [`isSpace`][],
because it *does* recognise Unicode spaces, but using something like
`"... \r\n\t"` instead would be suspicious – “why are they using this when
there's `isSpace`? Hm-m...”. Regexes aren't suspicious because I'm already
using them for everything else, and not everybody knows that `[:space:]` and
`isSpace` have different behavior.

First, here's the regex itself: ``mkRegex "[][{}()|,;=`[:space:]]"`` (the
`[][` weirdness in the beginning is due to the fact that `[\[\]` didn't work
for whatever reason). Some lexemes would be considered “whitespace” with this
definition (e.g. `||`), but it doesn't matter much. Now I can use it to split
the string and count words:

~~~ haskell
  putStr "Counting words... "
  let delim = mkRegex "[][{}()|,;=`[:space:]]"
  let wordCount = length $ filter (not . null) $ splitRegex delim codeWC
  print wordCount
~~~

Now, how big should be the word limit? With non-breaking spaces, `import
Data.ByteString.Internal` is one “word”. `answer =` is another. After that
should follow something like `inlinePerformIO $ readFile "flag"`, which (with
spaces removed) would be the third word... Y'know, I think ~~640 kB~~ 3 words
ought to be enough for anybody.

~~~
  when (wordCount > 3) $
    error "for Haskell, this is way too long. And scary. Rejected."
~~~

## Forbidding strings

(Okay, given the 3 words limit, there's no space left for generics. Maybe in
some other task, if I ever get to write another one— ouch, darn, I'm posting
this on the internet, right... No generics in CTF tasks ever, then.)

The obvious candidate for the “constant” to generate is `"flag"`, and it can
be “generated” simply by typing `"flag"`, which seems kinda boring. So,
strings have to go. Characters have to go too, because they can be used to
construct strings. Finally, numbers have to go because they can be converted
to characters which can be used to construct strings.

~~~ haskell
  when ("[\"'[:digit:]]" ~~ codeWC) $
    error "fun fact: 100% of bad programs contain text/numbers. Rejected."
~~~

Um... how to solve it, then? A-ha, one can use `length` on a list to obtain
an `Int`, problem solve— no, wait, `[` is a word separator, so whatever is
in the list would start a new word— *right*, I even mentioned it before:

> Some lexemes would be considered “whitespace” with this definition
> (e.g. `||`), but it doesn't matter much.

Turns out it does matter after all! I can simply make a list consisting of 4
lists of appropriate length full of `(||)`s, then map `length`, then
[`toEnum`][], and voila, I've got the `"flag"` without wasting the word. (In
fact, I can just as well use `()` instead of `(||)`.)

## Running the interpreter

Boring stuff again (read the hint docs yada yada nothing interesting):

~~~ haskell
  res <- runInterpreter $ do
    loadModules [codeFile]
    setTopLevelModules ["Main"]
    setImportsQ [("Prelude", Nothing)]
    interpret "answer" (as :: String)
  case res of
    Left  err -> putStrLn $ "Interpretation error: " ++ show err
    Right ans -> putStrLn $ "answer = " ++ ans
~~~

# A working exploit

All spaces are non-breaking spaces. The lists of `()`s have lengths
`[102, 108, 97, 103]`, which correspond to ASCII codes of “f”, “l”, “a”, “g”.

<style> code {white-space: pre-wrap;} </style>

~~~ haskell
import Data.ByteString.Internal

answer = inlinePerformIO $ readFile $ map toEnum $ map length [[(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()],[(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()],[(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()],[(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()]]
~~~

# Aftermath

Well, it could've been a more interesting task, that's for sure. No tricky
math, no clever combinations leading to unexpected results, just “find this
undocumented function” + “read Haskell report to learn about this behavior” +
“look closely at this regex”. I wonder whether I myself would've enjoyed
solving it...

Oh, and also, I never got paid my 150$.

[`Data.Data`]: http://hackage.haskell.org/package/base/docs/Data-Data.html
[`Data.Typeable`]: http://hackage.haskell.org/package/base/docs/Data-Typeable.html
[`Data.List`]: http://hackage.haskell.org/package/base/docs/Data-List.html
[`Data.Char`]: http://hackage.haskell.org/package/base/docs/Data-Char.html
[`isSpace`]: http://hackage.haskell.org/package/base/docs/Data-Char.html#v:isSpace
[`toEnum`]: http://hackage.haskell.org/package/base/docs/Prelude.html#v:toEnum
[`unsafePerformIO`]: http://hackage.haskell.org/package/base/docs/System-IO-Unsafe.html#v:unsafePerformIO
[`unsafeDupablePerformIO`]: http://hackage.haskell.org/package/base/docs/System-IO-Unsafe.html#v:unsafeDupablePerformIO
[`inlinePerformIO`]: http://hackage.haskell.org/package/text-1.1.1.3/docs/Data-Text-Unsafe.html#v:inlinePerformIO
[`Data.ByteString.Internal`]: http://hackage.haskell.org/package/bytestring-0.10.4.0/docs/Data-ByteString-Internal.html
[`coerce`]: http://hackage.haskell.org/package/base/docs/Data-Coerce.html#v:coerce

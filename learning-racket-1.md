% Learning Racket #1: Introduction

---
series:
  top: “Learning Racket” series
  toplink: /#racket
  next: /learning-racket-2
---

Warning: as of January 2016, I have abandoned this series (the 1st post was written in February <u>2014</u>).

-----------------------------------------------------------------------------

I always wanted to learn myself some Lisp for greater good and what-not, and
I've heard nice things about [Racket](@w:Racket (programming language))
(don't ask when or where, I don't remember), so it's going to be the first
Lisp I learn.

Also I'm tired of “how I spent one day learning [something] and found that it
sucks horribly” posts, so let me state in advance that when something doesn't
work as expected or sets my laptop on fire, I might react with “this is
unfortunate” but nothing beyond that. As long as [Node.js](@w) exists in this
world, I can't truly hate anything else.

<div class="note">

Whenever I think about Javascript – the language originally conceived to
animate jumping monkeys on web pages and now used to power web-servers – my
only reaction is ~~“I don't want to live on this planet anymore”~~ “how did
it all go so wrong”. Oh, and don't tell me “it doesn't matter what the
original purpose was” – the problem with Javascript (and web in general) is
that it's a hack on a hack and supported by a hack, like PHP (which is a
[fractal of bad design][]), except that PHP doesn't try to be fucking
*everywhere* and Javascript does. Fuck Javascript.

[fractal of bad design]: http://me.veekun.com/blog/2012/04/09/php-a-fractal-of-bad-design/

</div>

# Day 1

## Installation

There's a `racket` package in Arch's `extra` repository. 50 MB, quite small
for a modern language full of features and batteries included. Right?

~~~ bash
$ yaourt -S racket
~~~ 

One minute later, Racket is installed. And it takes only 350 MB on my laptop,
vs. 700 of GHC.

Do I need an IDE? Is there an Emacs-mode for Racket? What do I do now?  Aha,
there's a new program on my computer – “DrRacket”. It's probably what I want.

## DrRacket

Fucking Retina-schmetina (just kidding, I love Retina) – font is so small I
can barely read. I'm almost sure it can be fixed, tho. `Edit` → `Preferences`
→ `Font size` = `21`. Hm, it hasn't done anything to button labels, but at
least font is readable.

That's all I can say about DrRacket... for now.

## Looking for a tutorial

> Welcome to DrRacket, version 6.0 [3m].  
> Language: No language chosen; memory limit: 128 MB.  
> DrRacket cannot process programs until you choose a programming language.  
> Either select the “Choose Language...” item in the “Language” menu,
> or get guidance.

Yes, I need guidance!

> Using *How to Design Programs*?  
> Start with Beginning Student.

Fine, beginning student it shall be... No wait, it appears to be a language
chooser, not a built-in tutorial. Whatever, built-in tutorials are for
suckers anyway. I'll open
[The Racket Guide](http://docs.racket-lang.org/guide/intro.html) in browser
and start reading.

## The Racket Guide: 1.1. Interacting with Racket

> You type a Racket expression, hit the Return key, and the answer is
> printed. In the terminology of Racket, this kind of calculator is called a
> read-eval-print loop or REPL.

I love REPLs! And if it's a calculator, surely I can add 2 and 2 with it. If
I recall correctly...

~~~ scheme
> + 2 2
#<procedure:+>
2
2
~~~ 

Not enough parentheses, I guess. Okay, second try.

~~~ scheme
> (+ 2 2)
4
~~~ 

Now that's better. Does it support power operator? What about bignums?

~~~ scheme
> (^ 2 3)
undefined;
 cannot reference an identifier before its definition
 
> (** 2 3)
...same

> (^^ 2 3)
...same

> (pow 2 3)
...same
~~~ 

I wonder if `Tab` can help me. Nope. But there's `Ctrl-/`, let's type `pow`
and press it.

Hang for a minute (on a new MacBook Pro!), then a list pops up (`pow` isn't
on it). Later `Ctrl-/` works flawlessly. It's probably been downloading
autocompletion data or generating it or sending all my data to NSA or
something.

Googling “racket math operators” turns up
[this](http://docs.racket-lang.org/reference/generic-numbers.html#%28part._.Powers_and_.Roots%29). Aha,
it's called `expt`!

~~~ scheme
> (expt 2 3)
8

> (expt (expt 9 100) 10)
1747871251722651609659974619164660570529062487435188517811888011810686
2662272754892914864698646811110756089506961452765887713684358755086475
1441420209363848187291238008997717938152962847832052351931914268150442
4059410890214500500647813935818925701905402605484098137956979368551025
8252394113186439979165236770447696626286464065403356279753296192642450
7975047086246247409110544443735530214615147534809075533015326906793309
1699479889089824650841795567478606396975664557143737657027080403239977
7578652968467400937123779157705360942236880491080232441391830279624844
1107846443951684522796193522126981475341678257645550731607375198537404
6064592546796043150737808314501684679758056905948759246368644416151863
1380852766035958164109451575997420776176189116011851556020807717467859
5935987949019193338996527127540312792543224796326967591264610315634395
4375442792688936047041533537523137941310690833949767764290081333900380
310406154723157882112449991673819054110440001
~~~ 

Awesome.

> The following expression calls the built-in function `substring` with the
> arguments `"the boy out of the country"`, `4`, and `7`:
> 
> ~~~ scheme
> > (substring "the boy out of the country" 4 7)
> "boy"
> ~~~ 

Are strings just lists of characters, like in Haskell, or something else?
Would `substring` work on an ordinary list?

~~~ scheme
> (substring (0 1 2 3) 1 2)
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 0
  arguments...:
   1
   2
   3
~~~ 

Apparently, `(0 1 2 3)` is not a list but application of `0` to
`1 2 3`. IIRC, a little quote should do the trick:

~~~ scheme
> (substring '(0 1 2 3) 1 2)
substring: contract violation
  expected: string?
  given: '(0 1 2 3)
  argument position: 1st
  other arguments...:
   1
   2
~~~ 

So, `substring` really does need a genuine string. Or am I mistaken about the
quote?

~~~ scheme
> (length '(0 1 2 3))
4
~~~ 

Fine, whatever. I'll learn list slicing later.

## What I think so far

  * Racket's default IDE is better than GHCi and probably on par with Emacs
    (you almost certainly *can* configure Emacs to be better than anything,
    but it's not trivial and people don't bother, while DrRacket provides
    autocompletion and documentation out of the box).

  * Racket's default help (installed on computer) is also better than
    Haddocks. I mean, it includes instant search for functions! (You can try
    it [here](http://docs.racket-lang.org/search/index.html).)  There are
    lots of tutorials too, which means I can install `racket` on my laptop,
    turn internet connection off and have a few hours of studying without
    wasting time on Hacker News.

**Off-topic**: generally, it's *very* important that people have goodies out
of the box. Programmers are lazy, and many of them won't lift a finger to
make their own lives better (unless they realise it's a real problem and make
a conscious attempt to improve the situation.  For instance, I had to
[beemind](https://beeminder.com) fixing small problems with my laptop –
otherwise I'm pretty sure I'd *still* live without working hibernation and
sound controls.)

## TRG: 1.2. Definitions and Interactions

> If calling `(extract "the boy")` is part of the main action of your
> program, that would go in the definitions area, too. But if it was just an
> example expression that you were using to explore `extract`, then you’d
> more likely leave the definitions area as above, click Run, and then
> evaluate `(extract "the boy")` in the REPL.

I've just realised that I missed something very important. Very, very
important.

What's the shortcut for `Run`?

Aha, `Ctrl-R`. While I'm at it, killing the program is `Ctrl-K`. These two
are the most important shortcuts if you want to experiment but aren't good
enough (yet) to avoid freezing the interpreter every ten minutes while you
Just Wanted to Calculate Factorial of Billion, What's Wrong with That. Yes,
I'm a bignum junkie.

Back to definitions. Extracting the boy is boring; as I am a Haskell
programmer, my first definition *must* be a factorial.

~~~ scheme
(define (factorial n)
  (if
~~~ 

Er, how do I `if`?

What if I select it, right-click...

> Search in Help Desk for "if"

Yay, Help Desk!

> ~~~ scheme
> (if test-expr then-expr else-expr)
> ~~~ 
> 
> Evaluates `test-expr`. If it produces any value other than `#f`, then
> `then-expr` is evaluated, and its results are the result for the `if`
> form. Otherwise, `else-expr` is evaluated, and its results are the result
> for the `if` form. The `then-expr` and `else-expr` are in tail position
> with respect to the `if` form.
> 
> Examples:
> 
> ~~~ scheme
> > (if (positive? -5) (error "doesn't get here") 2)
> 2
> > (if (positive? 5) 1 (error "doesn't get here"))
> 1
> > (if 'we-have-no-bananas "yes" "no")
> "yes"
> ~~~ 

Okay, let's try.

~~~ scheme
(define (factorial n)
  (if (== n 0) 1 (* n (factorial (- n 1)))))
~~~ 

`Ctrl-R` and...

~~~ scheme
==: this match expander must be used inside match in: (== n 0)
~~~ 

Fi-ine, guessing mode on. It returns `bool` so it probably ends with `?`; can
it be something like `eq?`.

(Wow, when I point on an identifier, lines appear and show me where this
identifier occurs and where it's imported from! (Note to self: I should find
somewhere a huge program, open it in DrRacket and point at `racket` in `#lang
racket`.))

(Wow #2: a small box in the upper right corner shows me type of whatever is
under cursor! That's awesome and also much faster than looking up types in
ghc-mod for Emacs.)

(No, wait, drawback: it doesn't show any information for user-defined
functions. Pfft.)

Reading the documentation for `eq?` now. Apparently, there are lots of
different comparisons and what I actually want is `equal?` (or `=`).

Here's the final version (I wonder if I've indented it properly):

~~~ scheme
(define (factorial n)
  (if (= n 0) 
      1 
      (* n (factorial (- n 1)))))
~~~

~~~ scheme
> (factorial 100)
9332621544394415268169923885626670049071596826438162146859296389521759
9993229915608941463976156518286253697920827223758251185210916864000000
000000000000000000

> (factorial 100000)
~~~ 

Seven minutes later and factorial of 100000 is computed and printed. (Note to
self: `Ctrl-K` doesn't work if a menu is open... and when GUI doesn't
respond, menus can't be closed.)

I'll try to define Quicksort when I know a bit more about lists.

## What I think so far

  * Uniform syntax is harder to read. There should be more syntax
    highlighting (or different fonts – what about [`define`](:sc) in
    [small caps](:sc), for instance?).

  * Four different equality functions isn't a good sign. There's `=` for
    comparing numbers, `eq?` for testing whether two objects are the same
    object (in Haskell such stuff is
    [well-hidden](http://hackage.haskell.org/package/base-4.6.0.1/docs/System-Mem-StableName.html),
    and rightly so), `eqv?` which is the same as `eq?` except for numbers and
    characters, and `equal?` which works for most things. Oh, and `boolean=?`
    specifically for booleans. And `string=?` for strings. And `char=?` for
    characters. C'mon, I understand why all these things exist, but why
    expose them?

## TRG: 1.3. Creating Executables

A hello world program written in Haskell takes 760 kB; I wonder how big is
Racket's hello world going to be, considering that I'll be sure to pack the
entire RTS into it.

~~~ scheme
#lang racket

(print "Hello, world!")
~~~ 

Now `Ctrl-S` and then `Racket` → `Create Executable`. First let's try
“stand-alone”.

~~~ bash
$ du helloworld
4.9M	hw
4.9M	total
~~~ 

Five MB. Now what's about “distribution”?..

~~~ bash
$ du helloworld.tgz 
3.2M	helloworld.tgz
3.2M	total
~~~ 

Even less. But that's packed; what if I unpack it?

~~~ bash
$ du helloworld
0	helloworld/lib/plt/helloworld/exts
0	helloworld/lib/plt/helloworld/collects
0	helloworld/lib/plt/helloworld
40K	helloworld/lib/plt
3.7M	helloworld/lib
4.9M	helloworld/bin
8.5M	helloworld
8.5M	total
~~~ 

Still fine. I doubt I'll be creating more executables any time soon,
but it's good to know anyway.

---

Aha, look what I've found! If I define what I'm using more precisely,
I can further strip the executable:

~~~ scheme
#lang racket/base

(print "Hello, world!")
~~~ 

~~~ bash
$ du helloworld.tgz
1.8M	helloworld.tgz
1.8M	total

$ tar xvf helloworld.tgz 

$ du helloworld
0	helloworld/lib/plt/helloworld/exts
0	helloworld/lib/plt/helloworld/collects
0	helloworld/lib/plt/helloworld
40K	helloworld/lib/plt
4.0M	helloworld/lib
792K	helloworld/bin
4.8M	helloworld
4.8M	total
~~~ 

## TRG: 1.4. A Note to Readers with Lisp/Scheme Experience

I.e. not to me.

> The module system is designed to avoid these problems, so start with
> `#lang`, and you’ll be happier with Racket in the long run.

Hm, is it like starting with `module Main where` in Haskell? Okay, okay, I
solemnly swear to never start a Racket file with anything but `#lang`,
unless, of course, some new circumstances arise blah blah blah earthquakes
blah blah blah too lazy to type `#lang` blah blah blah.

## TRG: 2.1. Simple Values

Whoa, finished the first chapter!

> Numbers are written in the usual way, including fractions and imaginary
> numbers

More tinkering reveals that:

  * Fractionals are not bignum-y (by default):

    ~~~ scheme
	> 6.1e10000
    +inf.0
	~~~ 

  * However, rationals seem to be. Also, DrRacket prints fractions in
    a pretty cool way.

  * `5i+3` isn't a proper number, but `3+5i` and `3+5/8i` are.

  * `(/ 15 6)` is the same as `15/6`.

  * Unlike in Haskell, `.666` is parsed as a number and equals 0.666.
    `42.` is a number too. (But `.` is not.)

  * `3..4` is a valid identifier:

    ~~~ scheme
	> 3..4
    3..4: undefined;
     cannot reference an identifier before its definition
    ~~~ 

---

> Booleans are `#t` for true and `#f` for false. In conditionals, however,
> all non-`#f` values are treated as true.

I wonder why... Hm, it's probably useful for lookup functions: make `lookup`
return `#f` if the element wasn't found, and you can use every `lookup` as
`is-member` if you want to.

---

This chapter was small and boring. Next, please!

## TRG: 2.2. Simple Definitions and Expressions

> A function is just another kind of value, though the printed form is
> necessarily less complete than the printed form of a number or string.

Hey, how come? What about unity-of-code-and-data in Lisps? Why can't I get
the S-expr corresponding to a function?

But at least I can `eval` S-exprs, right?

~~~ scheme
> (eval '(+ 1 2))
3
~~~ 

Phew.

---

> ~~~ scheme
> (define (nobake flavor)
>   string-append flavor "jello")
> 
> > (nobake "green")
> "jello"
> ~~~ 
> 
> Within `nobake`, there are no parentheses around `string-append flavor
> "jello"`, so they are three separate expressions instead of one
> function-call expression. The expressions `string-append` and `flavor` are
> evaluated, but the results are never used. Instead, the result of the
> function is just the result of the final expression, `"jello"`.

I bet I would've made this mistake eventually if not for this warning (and
I'm not sure I won't make it anyway).

---

> The use of square brackets for `cond` clauses is a convention. In Racket,
> parentheses and square brackets are actually interchangeable, as long as
> `(` is matched with `)` and `[` is matched with `]`. Using square brackets
> in a few key places makes Racket code even more readable.

It's a really neat idea. I like Racket more and more.

---

> ~~~ scheme
> > (twice (lambda (s) (string-append s "!"))
>          "hello")
> "hello!!"
> ~~~ 

Aha, lambdas! My little evil functional heart is beating merrily inside my
chest. Let's see if I can write function composition at this point without
cheating.

(Meanwhile: I ran into
[this bug](http://lists.racket-lang.org/users/archive/2014-March/061742.html),
which caused me to restart DrRacket.)

~~~ scheme
(define (. f g)
  (lambda (x) (f (g x))))
~~~

~~~
Module Language: invalid module text
  read: illegal use of `.'
~~~ 

Fine, I don't remember what characters are allowed in identifiers. What about
`<>`?

~~~ scheme
(define (<> f g)
  (lambda (x) (f (g x))))
~~~

~~~ scheme
> ((<> (lambda (x) (+ x 1)) 
       (lambda (x) (* x 2))) 
   7)
15
~~~ 

Clumsy lambdas. Can I use `λ` instead? I can. Cool.

Hm, given that `λ` is just a Greek letter and not part of syntax (like in
Haskell), there's probably some sneaky `define` somewhere which equates `λ`
and `lambda`. Can I define my own alias for `lambda`?

~~~ scheme
(define (l v f)
  (lambda v f))
~~~

~~~ scheme
> ((l (x) (+ 3 x)) 7)
x: undefined;
 cannot reference an identifier before its definition
~~~ 

This is not fai— no, wait, `lambda` is probably not a function at all but
some macro-schmacro, and there's a list of `lambda`-aliases somewhere, and a
parser, and what-not, and even if it's possible to define my own alias for
`lambda`, it's Black Magic and definit— who am I kidding? I won't be able to
go to sleep until I define `l` to be `lambda` and I know it.

[eight minutes after]

It was easy! (Thanks to
[chapter 16 of Racket Guide](http://docs.racket-lang.org/guide/pattern-macros.html).)

~~~ scheme
(define-syntax-rule (l x y) 
  (λ x y))
~~~

~~~ scheme
> ((l (x) (+ 3 x)) 7)
10
~~~ 

---

There are also `let` and `let*`. The difference is that `let` doesn't allow
later definitions reference earlier ones, and `let*` – does.

Example:

~~~ scheme
> (let* ([x (random 100)]
         [y (random (+ x 1))])
    (list (+ x y) x y))

'(115 99 16)
~~~ 

However, we can't swap `x` and `y` lines.

After reading [reference](http://docs.racket-lang.org/reference/let.html) on
`let`-forms, I found that there's `letrec`. Will it help me?

~~~ scheme
> (letrec ([y (random (+ x 1))]
           [x (random 100)])
    (list (+ x y) x y))

+: contract violation
  expected: number?
  given: #<undefined>
  argument position: 1st
  other arguments...:
   1
~~~ 

Nope, even `letrec` doesn't work. But I still can write a factorial with it!

~~~ scheme
> (letrec (
      [fac (λ (n) 
         (if (= 0 n)
             1
             (* n (fac (sub1 n)))))])
    (map fac (range 10)))

'(1 1 2 6 24 120 720 5040 40320 362880)
~~~ 

## What I think so far

  * There should be an alternative to `let` with argument order reversed. I'm
    very used to treating local definitions as clarifying footnotes for “big
    picture” code, and don't want to give up this habit.

  * I like the flexibility given by many `let`-forms (there are `let`,
    `let*`, `letrec`, and `let-values` + `let*-values` + `letrec-values` for
    binding multiple outputs), but I feel that they all should've been just
    one `let` with optional modifiers.

  * Lambdas are clumsy even with `λ` instead of `lambda`. I would've
    preferred something like `(λx. (* x 2))` or even `(* _ 2)`, and I'd be
    quite disappointed if there isn't a macro for that.

## Time to sleep

Plans for tomorrow:

  * honor Haskell by writing a Quicksort

  * honor Tony Hoare by writing a *true* Quicksort (with mutable array)

  * finish ch. 2 and ch. 3

# Day 2

## TRG: 2.3. Lists, Iteration and Recursion

> The `list` function takes any number of values and returns a list
> containing the values

*Such* a useful function! Tho I guess the same could be said about Haskell's
`$` (which applies a function to an argument) and `id` (which returns its
argument)... Okay, I'll see if there are any non-obvious usecases for `list`
later.

## Interlude: list functions

I made myself a reference table for list functions:

                        Haskell Racket                           notes
------------------------------- ---------------------- ------------------------
`null`                          `null?` or `empty?`    not to be confused with `null`/`empty`
`map`, `zipWithN`               `map`                  Haskell's `map` is just `zipWith1`, after all
`length`                        `length`
`length . filter`               `count`
`filter`                        `filter`
`filter . not`                  `filter-not`           also, `negate` can be used to inverse a predicate
`lookup`                        `assoc`
`foldr`                         `foldr`
`foldl`                         `foldl`
`all`                           `andmap`               polyvariadic
`any`                           `ormap`                polyvariadic
`head`                          `car` or `first`
`(!! 1)` ... `(!! 9)`           `second` ... `tenth`   aka `cadr`, `caddr`, `cadddr` and `caddddr`
`!!`                            `list-ref`             clumsy name hints that it isn't needed very often
`tail`                          `cdr` or `rest`
`:`                             `cons`                 since lists are tuples, it's also `,`
`last`                          `last`                 not in `racket/base`
`reverse`                       `reverse`
`intersperse`, `intercalate`    `add-between`
`permutations`                  `permutations`
`++`                            `append`
`concat`                        `append*`              deep version of `concat` is called `flatten`
`sum`                           `apply +`
`product`                       `apply *`
`maximum`                       `apply max`
`minimum`                       `apply min`
`maximumOn`                     `argmax`
`minimumOn`                     `argmin`
`replicate`                     `make-list`
`take`                          `take`                 there's also `take-right`
`drop`                          `drop` or `list-tail`
`takeWhile`                     `takef`
`dropWhile`                     `dropf`
`splitAt`                       `split-at`
`span`                          `splitf-at`
`elem`                          `member`               that's where everything-but-`#f`-is-true proves useful
`find`                          `memf`
`partition`                     `partition`
`nub`[`By`,`On`]                `remove-duplicates`    controlled with optional arguments
`delete`                        `remove`
`\\`                            `remove*`              removes *all* occurences, not only the first ones
`sort`[`By`,`On`]               `sort`
`mapM_`                         `for-each`
list ranges                     `range`
random shuffle                  `shuffle`

Update: originally I had here `apply and` and `apply or` for Haskell's `and`
and `or`, but they don't actually work due to `and` and `or` being macros
and not functions.

While compiling the table I learned a few things.

### Lists are pairs

~~~ scheme
> (reverse '(1 . (2 . ())))
'(2 1)
~~~ 

Now I'm confused about what `'` and `.` mean in general.

Also, this:

~~~ scheme
> (reverse '(1 . 2))
reverse: contract violation
  expected: list?
  given: '(1 . 2)
~~~ 

hints that “type safety” is emulated by pre- and post-conditions (or
*contracts*), and various `list?`, `boolean?`, `number?`, etc.

Update: I'm wrong about type safety here; Racket is safe in the sense that it
won't let you silently coerce two values of different types into
participating in all sorts of abominable things (like taking a `float` and
making an `int` out of it without changing inner representation). However, if
I want to find out that not-a-list (say, `'(1 . (2 . 3))`) has been passed to
`reverse` *earlier* than when `reverse` sees `3` and becomes upset that it's
neither a pair nor an empty list, I still need to use explicit checks or
contracts.

### Optional arguments are common

...and awesome. I mean, what in Haskell is accomplished by `sortBy`,
`sortBy . comparing` (or `GHC.Exts.sortWith`) and `map fst . sortWith
snd . map (\x -> (x, f x))`, in Racket is done with mere `sort`.

### `<` is not overloaded

Why the heck can't `<` compare strings? I don't know. Google doesn't know
either. If you know, please tell me.

### There's such thing as `apply`

Basically it's the ultimate version of `uncurry`: it takes a function with
any number of arguments and a list and feeds elements of list to this
function. (Have I already made clear that lists don't have to be homogeneous
in Racket? Well, now I have.)

## Back to TRG

> It turns out that if you write
> 
> ~~~ scheme
> (define (my-map f lst)
>   (for/list ([i lst])
>     (f i)))
> ~~~ 
> 
> then the `for/list` form in the function is expanded to essentially the
> same code as the `iter` local definition and use. The difference is merely
> syntactic convenience.

Hey, you haven't explained `for/list` yet!..

Ah, it's just a list comprehension, like in Haskell. Let's generate some
Pythagorean triples.

Haskell:

~~~ haskell
[(i, j, k) | i <- [1..10], j <- [i..10], k <- [j..10], i^2 + j^2 == k^2]
~~~ 

Racket:

~~~ scheme
> (for/list ([i (range 1 10)]
             [j (range i 10)]
             [k (range j 10)]
             #:when (= (+ (sqr i) (sqr j))
			           (sqr k)))
    '(i j k))

i: undefined;
 cannot reference an identifier before its definition
~~~ 

Hm. Apparently I'm mistaken about `for/list`; further reading unravels
`for*/list`, maybe it's what I want?

~~~ scheme
> (for*/list ([i (range 1 10)]
              [j (range i 10)]
              [k (range j 10)]
              #:when (= (+ (sqr i) (sqr j))
			            (sqr k)))
    '(i j k))
	
'((i j k))
~~~ 

Not this either, but closer. Fine, I'll use `list` (and increase the range
while I'm at it):

~~~ scheme
> (for*/list ([i (range 1 20)]
              [j (range i 20)]
              [k (range j 20)]
              #:when (= (+ (sqr i) (sqr j))
			            (sqr k)))
    (list i j k))

'((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15))
~~~ 

Clumsy! On the other hand, a) there's probably a macro for Haskell-style
comprehensions, b) that's the price of uniform and predictable syntax, and
c) I don't use list comprehensions that often anyway. (By the by, I love how
Racket's interpreter works with multi-line expressions – `Enter` for newline,
`Ctrl-Enter` to evaluate, indentation is automatic.)

## A fun fact

I forgot to mention that a lot of functions in Racket are polyvariadic (just
like in Wolfram Mathematica). Behold:

~~~ scheme
> (+ 1 2 3)
6

> (+)
0

> (- 1 2 3)
-4

> (* 1 2 3)
6

> (*)
1

> (/ 4)
1/4

> (/ 3 4 5)
3/20

> (max 5 7 3)
7

> (list (= 1 1 1) (= 1 1 2))
'(#t #f)

> (list (< 1 2 3) (< 1 3 2))
'(#t #f)
~~~ 

## Interlude: the mystery of factorial

My thoughts keep returning to factorial of 100000. How come it's seven
minutes to compute? Maybe it was a one-time glitch? Maybe I should've written
it tail-recursively? Gotta check.

First, I'll define two versions of factorial:

~~~ scheme
(define (factorial1 n)
  (define (fac x k)
    (if (= k 0)
        x
        (fac (* x k) (sub1 k))))
  (fac 1 n))

(define (factorial2 n)
  (if (= n 0) 
      1 
      (* n (factorial2 (sub1 n)))))
~~~ 

Now I want to time their execution. There's a `time` function in Racket's
`base` (I wish there was one in Haskell's as well).

~~~ scheme
> (time (factorial1 20000) #t)
cpu time: 440 real time: 440 gc time: 144
#t
~~~ 

I've inserted `#t` there so that the factorial itself wouldn't be printed.

Okay, now to test four different factorials (there's one in `math`). Should
be easy, right?


~~~ scheme
> (require math)

> (for-each time
            '((factorial1 50000) 
              (factorial2 50000) 
              (apply * (range 1 50000)) 
              (factorial 50000)))
			  
time: bad syntax in: time
~~~ 

What?

Apparently – for reasons completely unclear to me – `time` is a special form
and not a procedure, which means I can't use it as a parameter to `for-each`
(or I can, but I don't yet know how). This is weird.

`time-apply`, on the other hand, is a genuine procedure. Let's use it.

~~~ scheme
> (map time-apply
     (list factorial factorial1 factorial2 *)
     (list '(10000) '(10000) '(10000) (range 1 10000)))

result arity mismatch;
 expected number of values not received
  expected: 1
  received: 4
  values...:
   '(284625968091705451890641321211986889014805140170279923...
   1626
   1628
   1528
~~~ 

In other words, `time-apply` returns four values (yeah, multiple return
values) and `map` expects a procedure which returns a single value.

After a ten minutes' search I was unable to find either
`gather-return-values` or `nth-return-value`, so I've given up and defined
`just-time`:

~~~ scheme
(define (just-time f s)
  (let-values ([(res t r g) (time-apply f s)])
    t))
~~~

~~~ scheme
> (just-time factorial1 '(50000))
3800
~~~ 

And finally:

~~~ scheme
> (map just-time
     (list factorial factorial1 factorial2 *)
     (list '(100000) '(100000) '(100000) (range 1 100000)))

'(206 22286 24220 20760)
~~~ 

Only 20 seconds, huh (note how built-in `factorial` is a *hundred* times
faster). Why seven minutes, then?

Turns out that while evaluating the factorial is pretty fast, *printing* it
is terribly slow: `(factorial 20000)` takes 57 ms to calculate and 8 seconds
to print. Moreover, it's not even converting the number to string that is
slow; I used `format "~a"` to explicity convert it to string, and printing
just the evaluated string was still awfully slow. Even compiling it into
executable hasn't made it any faster.

## Back to TRG

> Suppose, for example, that you want to remove consecutive duplicates from a
> list. While such a function can be written as a loop that remembers the
> previous element for each iteration, a Racket programmer would more likely
> just write the following:
>
> ~~~ scheme
> (define (remove-dups l)
>   (cond
>    [(empty? l) empty]
>    [(empty? (rest l)) l]
>    [else
>     (let ([i (first l)])
>       (if (equal? i (first (rest l)))
>           (remove-dups (rest l))
>           (cons i (remove-dups (rest l)))))]))
> ~~~ 

Enough. I demand pattern-matching! And I think I saw it mentioned
[somewhere](http://docs.racket-lang.org/guide/match.html) in the table of
contents...

...wow. Just look at this:

~~~ scheme
(define (rem-dups s)
  (match s
    ['()                '()]
    [(list-rest a a p)  (rem-dups (cons a p))]
    [(list-rest a p)    (cons a (rem-dups p))]))
~~~

~~~ scheme
> (map rem-dups
       '(()
         (1)
         (1 1)
         (1 2 1)
         (1 1 1 2)
         (1 1 2 2 3 1)))
		 
'(() (1) (1) (1 2 1) (1 2) (1 2 3 1))
~~~ 

I mean, this is *not built-in functionality* and it has more features than
Haskell's pattern-matching. Okay, Racket, you're forgiven for your weird
`time` and multiple return values and slow printing and, above all, name
which makes it hard to search for tutorials without also hitting upon sites
selling tennis apparel.

## What I think so far

  * Multiple return values are a cool idea, but there doesn't seem to be
    *good* support for it in standard library. Using `let-values` every time
    I need second-returned-value is both verbose and doesn't allow for
    compositional style (i.e. it doesn't let me say
	`(compose 2nd-value time-apply)`).

  * Pattern-matching rules.

  * Performance (at least by default) kinda sucks (at least when multiplying
    numbers and printing strings; your mileage might vary).

    Update: performance sucks much less when `racket` executable is used to
    execute the program. For instance, printing becomes almost instant, and
    factorials are calculated twice as fast as compared to using GHC's
    `runghc`. However, compiling with `raco exe` doesn't cause any further
    optimizations.

  * Support for functional paradigm is somewhere in the middle. Everything
    *needed* is there, but it's not very convenient to use. (I expect to
    stumble upon `goodies/fp` or `haskell-racket` module one day, but for now
    I won't be looking for it – I need to understand Racket's main paradigm
    before allowing myself to consciously deviate from it.)

## Time to sleep

Plans for tomorrow:

  * Finally write Quicksort (after all, I promised).

  * Solve a couple of tasks from [SPOJ](http://www.spoj.com/).

  * Given that reading every chapter of TRG raises lots of questions and
    provokes endless tinkering with not-quite-related concepts, I'll be happy
    if I manage to read as much as chapter 2.4.

# Day 3

## TRG: 2.4. Pairs, Lists, and Racket Syntax

> The `cons` function actually accepts any two values, not just a list for
> the second argument. When the second argument is not empty and not itself
> produced by `cons`, the result prints in a special way. The two values
> joined with `cons` are printed between parentheses, but with a dot (i.e.,
> a period surrounded by whitespace) in between:
> 
> ~~~ scheme
> > (cons 1 2)
> '(1 . 2)
> 
> > (cons "banana" "split")
> '("banana" . "split")
> ~~~ 

I.e. Racket doesn't distinguish between lists and tuples where the second
part is a list. Tsk, tsk.

> The name `rest` also makes less sense for non-list pairs; the more
> traditional names for `first` and `rest` are `car` and `cdr`,
> respectively. (Granted, the traditional names are also nonsense. Just
> remember that “a” comes before “d”, and `cdr` is pronounced “could-er.”)

Granted, the traditional names in Haskell are also not that great (`fst` and
`snd`), but they're still better than `car` and `could-er`... er, I mean
`cdr`.

> You are perhaps most likely to encounter a non-list pair when making a
> mistake, such as accidentally reversing the arguments to `cons`:
> 
> ~~~ scheme
> > (cons (list 2 3) 1)
> '((2 3) . 1)
> 
> > (cons 1 (list 2 3))
> '(1 2 3)
> ~~~ 

Er, what? Are pairs used so rarely that if I ever encounter one, the most
likely thing is that I made a mistake?

> Non-list pairs are used intentionally, sometimes.

Ah, *sometimes*.

> The only thing more confusing to new Racketeers than non-list pairs is the
> printing convention for pairs where the second element is a pair, but is
> not a list:
> 
> ~~~ scheme
> > (cons 0 (cons 1 2))
> '(0 1 . 2)
> ~~~ 
> 
> In general, the rule for printing a pair is as follows: use the dot
> notation unless the dot is immediately followed by an open parenthesis. In
> that case, remove the dot, the open parenthesis, and the matching close
> parenthesis. Thus, `'(0 . (1 . 2))` becomes `'(0 1 . 2)`, and
> `'(1 . (2 . (3 . ())))` becomes `'(1 2 3)`.

It's not that great – actually, it's pretty stupid – but I don't know what a
better design decision would be, so I guess it could be treated as a
necessary evil. Maybe Racket programmers really don't use pairs which aren't
lists any often, if they are willing to tolerate quirks like this one.

---

By the way, why is `.` not prefix? Discussion on
[c2 wiki](http://c2.com/cgi/wiki?DottedPairNotation) concedes that “it's an
accident of history, as with most notations”. And it would have to be a
special case for parser no matter whether prefix or infix, so there's nothing
gained.

---

> ...the `quote` form lets you write a list as an expression in essentially
> the same way that the list prints:
> 
> ~~~ scheme
> > (quote ("red" "green" "blue"))
> '("red" "green" "blue")
> 
> > (quote ((1) (2 3) (4)))
> '((1) (2 3) (4))
> 
> > (quote ())
> '()
> ~~~ 

Aha. My current understanding is that by default everything is code, and
`quote` brings it in the realm of data.

~~~ scheme
(aba caba)    ; code
'(aba caba)   ; data
~~~ 

Moreover, `quote` is recursive, unlike `list`:

~~~ scheme
> (first '((1 2) (3 4 5) 6))
'(1 2)

> (list (1 2) (3 4 5) 6)
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 1
  arguments...:
   2
~~~ 

What does `''(1 2 3)` mean, then? Well, it's the same as
`'(quote (1 2 3))`:

~~~ scheme
> ''(1 2 3)

''(1 2 3)
> '(quote (1 2 3))
''(1 2 3)

> (first ''(1 2 3))
'quote

> (rest ''(1 2 3))
'((1 2 3))
~~~ 

It stumbled me for a while, before I remembered that `rest` returns the rest
of the list, and not simply its second element.

Hm. Would `quote` try to simplify `(list 1 2 3)` into `'(1 2 3)`?

~~~ scheme
> '(list 1 2 3)
'(list 1 2 3)

> (first '(list 1 2 3))
'list
~~~ 

Nope.

---

> A value that prints like a quoted identifier is a *symbol*. In the same way
> that parenthesized output should not be confused with expressions, a
> printed symbol should not be confused with an identifier. In particular,
> the symbol `(quote map)` has nothing to do with the `map` identifier or the
> predefined function that is bound to `map`, except that the symbol and the
> identifier happen to be made up of the same letters.

I haven't really expected this (but I should've – I already knew that Lisp
had something called “atoms” and could guess it was about quoted
identifiers).

~~~ scheme
> 'gibberish
'gibberish

> (gibberish)
gibberish: undefined;
 cannot reference an identifier before its definition
~~~ 

I can also convert strings to symbols and back:

~~~ scheme
> (string->symbol "str")
'str

> (symbol->string 'str)
"str"
~~~

The guide doesn't go any further, tho. What will happen if I try to convert
other strings?

~~~ scheme
> (map string->symbol
       '("42" "'" "()" "(1 2 3)" "\"str ing\"" ""))

'(|42| |'| |()| |(1 2 3)| |"str ing"| ||)
~~~

Amazing variety, isn't it. Googling “racket vertical bars”
[reveals](http://docs.racket-lang.org/guide/symbols.html) that this is merely
a form of syntax for symbols containing spaces or special characters.

(Meanwhile: I'm tempted to start speculating about symbols, based on what I
heard, but I'll try to refrain from doing so for now.)

---

...Now *this* is a real hack:

> Normally, `.` is allowed by the reader only with a parenthesized sequence,
> and only before the last element of the sequence. However, a pair of `.`s
> can also appear around a single element in a parenthesized sequence, as
> long as the element is not first or last. Such a pair triggers a reader
> conversion that moves the element between `.`s to the front of the
> list. The conversion enables a kind of general infix notation:
>
> ~~~ scheme
> > (1 . < . 2)
> #t
> 
> > '(1 . < . 2)
> '(< 1 2)
> ~~~
> 
> This two-dot convention is non-traditional, and it has essentially nothing
> to do with the dot notation for non-list pairs. Racket programmers use the
> infix convention sparingly—mostly for asymmetric binary operators such as
> `<` and `is-a?`.

On one hand – cool, I can write infix-style if I want to (tho I bet There Is
A Macro For This Somewhere – after all, writing math expressions in prefix
notation must suck horribly).

On the other hand... It's not *that* convenient (dots, spaces, meh) and it
definitely isn't justified enough to be included in language.

---

I did some googling and found a much nicer
[proposal](http://srfi.schemers.org/srfi-105/srfi-105.html) for infix syntax
– just wrap it into curly brackets and that's all. Unfortunately, vanilla
Racket seems to be treating curly brackets just like square brackets – a
substitute for parens and nothing more.

---

*Look at this:*

~~~ scheme
define fibfast(n)
  if {n < 2}
    n
    fibup(n 2 1 0)

define fibup(max count n-1 n-2)
  if {max = count}
    {n-1 + n-2}
    fibup max {count + 1} {n-1 + n-2} n-1

define factorial(n)
  if {n <= 1}
    1
    {n * factorial{n - 1}}
~~~

And to write like this, the only thing I need to do is to import
[one package](http://planet.racket-lang.org/package-source/asumu/sweet.plt/1/4/planet-docs/sweet/index.html).

It must be any language designer's ultimate dream.

(And this is probably Lisp's greatest weakness as well – with this level of
possible diversity, everyone has to use the “common lowest denominator”
simply because nobody can agree on what alternative syntax / library /
etc. is better and should be used.)

**Off-topic**: it's not enough to give everyone opportunity to improve the
language; you have to choose the winners and promote them heavily. The rules
of free market *don't* work here; people won't use the best thing, they'll
use the one which is available out of the box and which is used by their
peers.

## Quicksort

Without further ado...

~~~ scheme
(define (qsort1 s)
  (cond [(or (empty? s) (empty? (rest s)))    s]
        [else (let*-values ([(p)      (first s)]
                            [(split)  (λ (x) (< x p))]
                            [(s< s>)  (partition split (rest s))])
                (append (qsort1 s<) (list p) (qsort1 s>)))]))
~~~

~~~ scheme
> (qsort1 '(3 14 15 92 6 53 58 97 93 23))
'(3 6 14 15 23 53 58 92 93 97)
~~~

There's nothing really to explain, so I won't explain anything.

## Array Quicksort

Arrays are considered an “advanced topic” in Haskell and it's nigh impossible
to write an elegant piece of code dealing with mutable arrays in Haskell
without spending indecent amount of time (at least, on your first try). At
this moment I am *clueless* about how to write imperatively in Racket –
which, of course, means that it would be even more fun.

**Step zero**: google “racket vector”.

**Step one**: read
[the chapter](http://docs.racket-lang.org/reference/vectors.html) of
The Racket Reference concerning vectors.

**Step two**: make a helpful table. Since in Racket most functions operate on
both mutable and immutable vectors, I'm going to use functions `Data.Vector`
for comparison because they're shorter, unless the function in question is
specific to mutable vectors.

<div class="autowidth">

                               Haskell Racket
-------------------------------------- --------------------------------------
`length`                               `vector-length`
`replicate`                            `make-vector`
`fromList [a,b,c]`                     `vector a b c` 
`fromList`                             `list->vector`
`toList`                               `vector->list`
`generate`                             `build-vector`
`!` (or `read` for mutable vectors)    `vector-ref`
`write`                                `vector-set!`
`set`                                  `vector-fill!`
various functions like `map`           various functions like `vector-map`

</div>

**Step two-and-a-half**: mourn the bareness of `Data.Vector.Mutable`.

**Step three**: google “racket assignment” and find out that it's simply
`(set! variable value)`.

**Step four**: write a function to swap vector elements.

~~~ scheme
(define (vector-swap vec i j)
  (let ([t (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j t)))
~~~

**Step five**: write a function to move elements that are less/greater than
pivot to the left/right from said pivot.

~~~ scheme
(define (part vec left right p)
  (define pivot (vector-ref vec p))
  (vector-swap vec p right)            ; move pivot to end
  (define border left)
  (for ([i (range left right)]         ; (range left right) is the same
        #:when (<= (vector-ref vec i)  ;   as left..right-1
                   pivot))
    (vector-swap vec i border)
    (set! border (add1 border)))
  (vector-swap vec border right)       ; move pivot back
  border)                              ; return position of pivot
~~~

**Step six**: write the goddamned Quicksort.

~~~ scheme
(define (qsort2 vec)
  (define (sort left right)             ; subarray sort
    (when (< left right)                ; don't sort arrays of one element
      (let* ([p  (left . + . (random (- right left)))]
             [p* (part vec left right p)])
        (sort left (sub1 p*))           ; sorting lesser elements
        (sort (add1 p*) right))))       ; sorting greater elements
  (sort 0 (sub1 (vector-length vec))))
~~~

~~~ scheme
> (let ([blah (vector 3 14 15 92 6 53 58 97 93 23)])
    (qsort2 blah)
    blah)

'#(3 6 14 15 23 53 58 92 93 97)
~~~

## What I think so far

This day consisted mostly of ranting and awing, and I haven't learned much
new. However:

  * Even if Racket turns out to be not suitable for industry programming, I'm
    still going to use it as a vehicle for language experimentation. It may
    not be a language I *will* program in, but I can probably make out of it
    a language I *love* to program in.

  * The true power of symbols and S-exprs is yet to be unraveled.

  * I don't know how well imperative parts of Racket mix with functional
    parts. I've programmed in Pascal, and I've programmed in Haskell, and I'm
    used to both paradigms, but a mix of them is probably going to bring some
    surprises.

## Time to sleep

Plans for tomorrow:

  * Read chapters 3.1 – 3.5.

  * Apparently, SPOJ doesn't support Racket. (Oh, but it does support
    Node.js.) Therefore, I must either find something which does support it,
    or solve Project Euler tasks instead (but I still will use console
    input/output).

  * Take a look at
    [named `let`](http://docs.racket-lang.org/guide/let.html#%28part._.Named_let%29),
    as per Gökçehan Kara's
    [suggestion](http://www.reddit.com/r/Racket/comments/231sph/a_haskell_programmers_enthusiastic_foray_into/cgsv7yb).

  * I just noticed a “Debug” button in DrRacket. “What time is it?
    ~~Adventure~~ Debug Time!”

Update: this was the last time I included any “plans for tomorrow”
nonsense. It doesn't work.

# Comments

## Philippe Meunier

...has somehow found this draft and sent me a letter, in which ne noticed
that:

  * > If you want “let with argument order reversed”, you can write a macro.

    I can write a macro, yep (and I will). But I'd prefer for it to be a
    *standard* feature.

  * > `time` cannot be a function because Racket is call-by-value and not
    call-by-name.

    Okay, okay, I understand. (Tho I wonder if `time` could instead take a
    quoted expression and somehow evaluate it inside...)

## Guillaume Marceau

...has posted a
[comment](http://www.reddit.com/r/Racket/comments/231sph/a_haskell_programmers_enthusiastic_foray_into/cgst4s2)
on Reddit, in which ne notes that:

  * > Modern Racket style leans on nested `define` instead of the various
    forms of `let`.

    It's good to know. To be honest, up to this moment I was for some reason
    believing the opposite – that I shouldn't use `define` where a `let`-form
    will do.

  * > Contracts in Racket are awesome. See
    [here](http://docs.racket-lang.org/guide/contract-boundaries.html), or
    read the original
    [research paper](http://www.eecs.northwestern.edu/%7Erobby/pubs/papers/ho-contracts-techreport.pdf).

    This would be in my plans for the day after tomorrow. After all,
    following The Racket Guide gets boring, doesn't it?

  * > The lambda shorthand that's available is the
    [`cut` macro](http://docs.racket-lang.org/srfi/srfi-std/srfi-26.html#cut),
    which oddly is not very popular, even if I personally use it a lot. See
    [here](https://github.com/gmarceau/racket-utils/blob/master/cut.rkt) and
    count the number of `<>`s in
    [here](https://github.com/gmarceau/racket-utils/blob/master/table.rkt),
    for example.

    Tomorrow!

  * > “Why the heck can't `<` compare strings?” – Good question. In the
    meantime, use `string<?`.

    Okay. In the similar venue, why does `length` only work on lists?  There
    *is* a
    [generic interface](http://docs.racket-lang.org/reference/sequences.html)
    for various types of sequences in standard library, but as long as
    `sequence-length` is more keystrokes than `vector-length`, nobody is
    going to use it except when specifically designing a function to work on
    every type of sequences – and it doesn't happen often. In short, I need
    something like Snoyman's [classy-prelude](@hackage) (see the
    [original post](http://www.yesodweb.com/blog/2012/07/classy-prelude)),
    but for Racket.

  * > “The true power of symbols and S-exprs is yet to be unraveled.”
    – Indeed. You are yearning for
    [`unquote`](http://docs.racket-lang.org/reference/quasiquote.html#%28form._%28%28quote._%7E23%7E25kernel%29._unquote%29%29)
    and
    [`syntax-quote`](http://docs.racket-lang.org/reference/Syntax_Quoting__quote-syntax.html#%28form._%28%28quote._%7E23%7E25kernel%29._quote-syntax%29%29).

    Looks like something between generics and Template Haskell. I'll save it
    for future as well.

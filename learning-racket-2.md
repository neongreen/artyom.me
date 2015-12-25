% Learning Racket #2: Macros, Macros and a Bit of Modules

---
series:
  top: “Learning Racket” series
  toplink: /#racket
  prev: /learning-racket-1
  next: /learning-racket-3
---

After spending two days doing nothing but sleeping, waking up, watching the
visitor counter, reading
[comments](https://news.ycombinator.com/item?id=7595098) on HN and falling
asleep again... Okay, here we go.

# Day 4

## `let` vs. `define`

As I mentioned before, I'm going to switch to using internal `define`s
wherever possible instead of `let`-forms. However, my plan is a *bit* more
complicated; first I need to learn some things.

## How to make an alias for `define`?

The thing is, “define” is too long. Why not use `def` instead? And the name
doesn't seem to be taken already.

Will the same way that I used to define an alias for `lambda` work
now?

~~~ scheme
> (define-syntax-rule (def id body) (define id body))

> (def name "Artyom")

> name
"Artyom"
~~~

I have a feeling, however, that it won't work if body consists of
*multiple* expressions. Let's check.

~~~ scheme
(def (vector-swap vec i j)
  (def t (vector-ref vec i))
  (vector-set! vec i (vector-ref vec j))
  (vector-set! vec j t))
~~~

And indeed I get an error message :(

~~~
def: use does not match pattern: (def id body) in: (def (vector-swap
vec i j) (def t (vector-ref vec i)) (vector-set! vec i (vector-ref vec
j)) (vector-set! vec j t))
~~~

Okay. `define-syntax-rule` is, as the docs
[say](http://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fmisc..rkt%29._define-syntax-rule%29%29),
a special form, and probably allows more interesting patterns than
`id`. Let's skip some (okay, more than a dozen) chapters and read
about...

## TRG: 16.1. Pattern-Based Macros

> A *pattern-based macro* replaces any code that matches a pattern to
> an expansion that uses parts of the original syntax that match parts
> of the pattern.

I wonder what would happen if I define two macros in terms of each
other. Will it work? In what order are macros applied? (Oh darn, now
I'm curious about how Lisp interpreters work in general. Don't think
about it, don't think about it...)

> The simplest way to create a macro is to use `define-syntax-rule`:
>
> ~~~ scheme
> (define-syntax-rule pattern template)
> ~~~
> 
> As a running example, consider the `swap` macro, which swaps the
> values stored in two variables. It can be implemented using
> `define-syntax-rule` as follows:
> 
> ~~~ scheme
> (define-syntax-rule (swap x y)
>   (let ([tmp x])
>     (set! x y)
>     (set! y tmp)))
> ~~~

So *that's* how to swap two variables in Racket.
[Call-by-value](http://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_value),
right.

~~~ scheme
> (let ([x 'x] [y 'y])
    (swap x y)
    (list x y))

'(y x)
~~~

Cool. What about misuse-resistance?

~~~ scheme
> (let ([x 'x] [y 'y])
    (swap (add1 x) (sub1 y))
    (list x y))

set!: not an identifier in: (add1 x)
~~~

Not cool – this error is as misleading as Haskell's infamous
`*** Exception: Prelude.head: empty list` which pops whenever
*somewhere* somebody tries to extract the first element of
an empty list. Can I at least restrict `swap` to just single
identifiers?

What patterns am I allowed to use?

Where's the big blinking link saying “10 Amazing Racket Macro
Patterns You Didn't Know About”?

What do docs say about
[`define-syntax-rule`](http://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fmisc..rkt%29._define-syntax-rule%29%29)?

> Equivalent to
>
> ~~~ scheme
> (define-syntax id
>   (syntax-rules ()
>    [(id . pattern) template]))
> ~~~

Ni-ice. What about
[`syntax-rules`](http://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fstxcase-scheme..rkt%29._syntax-rules%29%29)?

> Equivalent to
> 
> ~~~ scheme
> (lambda (stx)
>   (syntax-case stx (literal-id ...)
>     [(generated-id . pattern) (syntax-protect #'template)]  ...))
> ~~~

Further down the rabbit hole.
[`syntax-case`](http://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fstxcase-scheme..rkt%29._syntax-case%29%29)?

> ~~~
> (syntax-case stx-expr (literal-id ...)
>   clause ...)
> 
> 
>       clause = [pattern result-expr]
>              | [pattern fender-expr result-expr]
> 
>      pattern = _
>              | id
>              | (pattern ...)
>              | (pattern ...+ . pattern)
>              | (pattern ... pattern ellipsis pattern ...)
>              | (pattern ... pattern ellipsis pattern ... . pattern)
>              | #(pattern ...)
>              | #(pattern ... pattern ellipsis pattern ...)
>              | #&pattern
>              | #s(key-datum pattern ...)
>              | #s(key-datum pattern ... pattern ellipsis pattern ...)
>              | (ellipsis stat-pattern)
>              | const
> 
> stat-pattern = id
>              | (stat-pattern ...)
>              | (stat-pattern ...+ . stat-pattern)
>              | #(stat-pattern ...)
>              | const
> 
>     ellipsis = ...
> ~~~

Finally!

---

[after reading all patterns' descriptions]

None of them seem to match “single identifier” (or “number”, for that
matter, or “string”...). In fact, I glanced over this chapter and
haven't found anything relevant. But this seems like a problem which
should be common enough; maybe it's discussed in the
[next chapter](http://docs.racket-lang.org/guide/proc-macros.html)?

>   1. Syntax Objects
>   2. Macro Transformer Procedures
>   3. Mixing Patterns and Expressions: `syntax-case`
>   4. `with-syntax` and `generate-temporaries`
>   5. Compile and Run-Time Phases
>   6. General Phase Levels
>        1. Phases and Bindings
>        2. Phases and Modules
>   7. Syntax Taints

Hm, which one should I choose... My gut tells me it's
[subchapter 3](http://docs.racket-lang.org/guide/syntax-case.html).
(By “my gut” I mean, of course, the part of brain which is trained
to recognise words it has seen before and ignore everything else.)

Aha!

> ...With this definition, `(swap x 2)` provides a syntax error
> originating from `swap` instead of `set!`.

I won't look at how it's don— no, wait, it's actually more or less
understandable even at this point; perhaps I should just read this
small chapter from the beginning and see how much I can understand.

## TRG: 16.2.3. Mixing Patterns and Expressions: `syntax-case`

> Unlike `syntax-rules`, the `syntax-case` form does not produce a
> procedure. Instead, it starts with a *stx-expr* expression that
> determines the syntax object to match against the *pattern*s. Also,
> each `syntax-case` clause has a *pattern* and *expr*, instead of a
> *pattern* and *template*. Within an *expr*, the `syntax` form –
> usually abbreviated with `#'` – shifts into template-construction
> mode; if the *expr* of a clause starts with `#'`, then we have
> something like a `syntax-rules` form:
> 
> ~~~ scheme
> > (syntax->datum
>    (syntax-case #'(+ 1 2) ()
>     [(op n1 n2) #'(- n1 n2)]))
> 
> '(- 1 2)
> ~~~

As I understand it, `#'` (which is an alias for `syntax`) creates a
representation for expressions which can include patternish stuff like
`...`. Then, `syntax-case` allows matching on such representations and
transforming them; finally, `syntax->datum` reverses the
transformation carried out by `#'`.

What would `syntax->datum` do when it encounters a pattern?

~~~ scheme
> (syntax->datum #'(id ...))
syntax: no pattern variables before ellipsis in template in: ...
~~~

[I see](http://i0.kym-cdn.com/photos/images/original/000/234/739/fa5.jpg).

---

Wait... I don't actually *know* that patterns accepted by
`syntax-case` are the same ones which `#'` wants, right? And
[indeed](http://docs.racket-lang.org/reference/syntax-model.html#%28part._stxobj-model%29):

> A *syntax object* combines a simpler Racket value, such as a symbol
> or pair, with *lexical information* about bindings, source-location
> information, syntax properties, and tamper status. In particular, an
> identifier is represented as a symbol object that combines a symbol
> with lexical and other information.

Sigh. I'm tempted to delete the above paragraphs and pretend that I
understood everything correctly from the first try.
Must... resist... temptation...

...Okay, second try. As I understand it, `#'` (which is an alias for
`syntax`) creates a representation for Racket expressions, *taking
into consideration scoping* – i.e. say, `'map` is not related to `map`
in any way, but `#'map` is. (For Haskellers here, it seems to be the
same thing as `[| map |]`.) Then, `syntax-case` allows matching on
such representations and transforming them; finally, `syntax->datum`
reverses the transformation carried out by `#'`.

---

> We could write the `swap` macro using `syntax-case` instead of
> `define-syntax-rule` or `syntax-rules`:
> 
> ~~~ scheme
> (define-syntax (swap stx)
>   (syntax-case stx ()
>     [(swap x y) #'(let ([tmp x])
>                     (set! x y)
>                     (set! y tmp))]))
> ~~~

I wonder: [gee, am I saying “I wonder” too often...] which of those
`swap`s is responsible for the actual name I use when calling the
`swap` macro?

~~~ scheme
(define-syntax (eyjafjallajökull stx)
  (syntax-case stx ()
    [(swap a b) #'(let ([t a])
                    (set! a b)
                    (set! b t))]))
~~~

~~~ scheme
> (let ([x 1] [y 2])
    (swap x y)
    (list x y))

swap: unbound identifier in module in: swap
~~~

So it's the first one. (In fact, re-reading the docs for `syntax-case`
suggests that I can even replace `(swap a b)` with `(_ a b)` and
nothing will change.)

---

What does `()` mean in `(syntax-case stx () ...`?

The answer, again, is found in the docs. (When reading, keep in mind
that everything that could've been inside of `()` is referenced as
*“literal-id”*.)

> An *id* matches any syntax object when it is not bound to `...` or
> `_` and does not have the same binding as any *literal-id*. The *id*
> is further bound as *pattern variable* for the corresponding
> *fender-expr* (if any) and *result-expr*.

> An *id* that has the same binding as a *literal-id* matches a syntax
> object that is an identifier with the same binding in the sense of
> `free-identifier=?`.

Apparently, it allows me to select which identifiers won't be
interpreted as wildcards in `syntax-case` patterns.

Let me try:

~~~ scheme
(define-syntax (: stx)
  (syntax-case stx (+)
    [(_ l ... + r ...)  #'(+ (: l ...)
                             (: r ...))]))
~~~

`<pattern> ...` is supposed to match any number of tokens which match
the pattern (I wonder whether it's left- or right-associative, by the
way). The exact syntax of `...` is unclear (for instance, how do I
match any-number-of more complex patterns? And how do I refer to
matched tokens later?), but for now I'm satisfied with this partial
understanding.

Ctrl-R and...

~~~ scheme
syntax-case: misplaced ellipsis in pattern (follows other ellipsis)
in: ...
~~~

At first I thought it was just me being stupid, but after some
tinkering around and googling I found
[this](http://osdir.com/ml/general/2014-01/msg04701.html) message on
Racket mailing list, which states that it's impossible to do with
`syntax-case` and advises to use `syntax-parse`.

Why, why must everything be Inferior By Default? But anyway:

~~~ scheme
(define-syntax (: stx)
  (syntax-parse stx #:literals (+)
    [(_ l ... + r ...)  #'(+ (: l ...)
                             (: r ...))]))
~~~

Note how `#:literals (+)` – an optional parameter – is used instead of
a mandatory list parameter.

~~~ scheme
> (: 1 + 2)
:: expected more terms starting with the literal symbol `+' or any
term in: (1)
~~~

Ah right, I forgot about the base case.

~~~ scheme
(define-syntax (: stx)
  (syntax-parse stx #:literals (+)
    [(_ l ... + r ...)  #'(+ (: l ...)
                             (: r ...))]
    [(_ x)              #'x]))
~~~

~~~ scheme
> (: 1 + 2)
3
~~~

Great. Now I have to add more operators!

~~~ scheme
(define-syntax (: stx)
  (syntax-parse stx #:literals (+ - * / ^)
    [(_ l ... + r ...)  #'(+ (: l ...) (: r ...))]
    [(_ l ... - r ...)  #'(- (: l ...) (: r ...))]
    [(_ l ... * r ...)  #'(* (: l ...) (: r ...))]
    [(_ l ... / r ...)  #'(/ (: l ...) (: r ...))]
    [(_ l     ^ r ...)  #'(expt l      (: r ...))]
    [(_ x)              #'x]))
~~~

(This works because when `*` and `/` are processed, we've already
dealt with all `+` and `-`. Also, the case for `^` is different
because it's right-associative.)

~~~
syntax-parse: literal is unbound in phase 0 (phase 0 relative to the
enclosing module) in: ^
~~~

Hey, what? The only difference between `+` and `^` is that the latter
isn't already bound to `expt`. Why would it matter if I'm doing the
interpretation myself anyway...

[Turns out](http://docs.racket-lang.org/syntax/Parsing_Syntax.html#%28form._%28%28lib._syntax%2Fparse..rkt%29._syntax-parse%29%29)
that it matters. The sidenote to `#:literals` section says:

> Unlike `syntax-case`, `syntax-parse` requires all literals to have a
> binding. To match identifiers by their symbolic names, use
> `#:datum-literals` or the `~datum` pattern form instead.

There's probably some good reason for it, but I've no idea what it
could be.

~~~ scheme
(define-syntax (: stx)
  (syntax-parse stx #:datum-literals (+ - * / ^)
    [(_ l ... + r ...)  #'(+ (: l ...) (: r ...))]
    [(_ l ... - r ...)  #'(- (: l ...) (: r ...))]
    [(_ l ... * r ...)  #'(* (: l ...) (: r ...))]
    [(_ l ... / r ...)  #'(/ (: l ...) (: r ...))]
    [(_ l     ^ r ...)  #'(expt l      (: r ...))]
    [(_ x)              #'x]))
~~~

~~~ scheme
> (: 1 + 3 - (+ 1 2 3) * 4 / 5 + 3 * 7 ^ 2 ^ 2 * 5 - 3 / 5 / 8 - 9 / 7)
36012 47/56
~~~

It works, it works!

(And, as you can see, it evals everything correctly (this time I
checked it in GHCi to make sure) and also isn't limited to just
numbers – I can use any Racket expressions as tokens.)

---

**Bonus**:

  1. Paste the long expression into code area (not REPL) and reload
     (it should print the result of its evaluation upon reload).

  2. Press the “Macro Stepper” button in the upper right corner.

  3. Press “Step” repeatedly.

  4. Have fun!

<a name="actual-evaluation"></a>

I wanted to add actual evaluation here as well (so that I'd see how
`(+ 1 2)` gets simplified to `3`), but didn't find any functions named
`reduce-syntax` or `eval-syntax-and-syntaxify-again`. I'll put it off
for a while.

---

Do you still remember where it all started? I wanted to write a `def`
macro and then decided to read one “small chapter”. Ha.

## Night-time rambles

  * hm, I wonder whether people who try to follow this all will find
    it very confusing

  * nah, probably not

  * there's not much to follow actually

  * and hopefully they don't expect to *learn* Racket merely by
    reading how I was learning it

  * they'll have to write their own macros and make their own mistakes

  * because nobody has ever learned a programming language without
    writing a line of code

  * ...right?

# Interlude

I've spent half of this day writing down what I forgot to write down
yesterday and fixing typos / minor inaccuracies. So, no learning today.

These posts are split in three-days-sized chunks. However, a) it takes me
more than three days to write about three days, and b) learning-and-writing
is twice as slow as just learning (but probably also better). If you want to
have an estimate of how long it will take *you* to acquire the same knowledge
I have at some point, divide the time it took me to do it by 2–6 (the actual
coefficient depends on your intelligence and whether you prefer to ask
questions or spend hours googling everything by yourself).

Speaking of questions... I am the type of person who would rather ~~die~~
make a hundred Google queries than ask for help from a human, and I suspect
I'm not the only one who is like that. So, here's an offer: if you have a
question which is related to what I've covered so far, you should submit it
and I'll try to investigate it. You get an answer, and I get more knowledge /
don't have to strain my imagination thinking “hm, what other questions I
*could* have but didn't?”.

# Day 5

## Back to TRG 16.2.3

> One advantage of using `syntax-case` is that we can provide better error
> reporting for `swap`. For example, with the `define-syntax-rule` definition
> of `swap`, then `(swap x 2)` produces a syntax error in terms of `set!`,
> because `2` is not an identifier. We can refine our `syntax-case`
> implementation of `swap` to explicitly check the sub-forms:
> 
> ~~~ scheme
> (define-syntax (swap stx)
>   (syntax-case stx ()
>     [(swap x y)
>      (if (and (identifier? #'x)
>               (identifier? #'y))
>          #'(let ([tmp x])
>              (set! x y)
>              (set! y tmp))
>          (raise-syntax-error #f
>                              "not an identifier"
>                              stx
>                              (if (identifier? #'x)
>                                  #'y
>                                  #'x)))]))
> ~~~
>
> With this definition, `(swap x 2)` provides a syntax error
> originating from `swap` instead of `set!`.

Understood. (Notice how `identifier?` is defined not on things (`x`) but on
syntax objects (`#'x`).)

---

It still annoys me that *this* is how I should be writing
`swap`. However, I have two hopes:

  1. `syntax-parse` turns out to be more advanced and lets me do what
     I want.

  2. Syntax pattern language is in some way extensible and I can
     define my own pattern which only matches identifiers.

---

[Docs](http://docs.racket-lang.org/syntax/Parsing_Syntax.html#%28form._%28%28lib._syntax%2Fparse..rkt%29._syntax-parse%29%29)
for `syntax-parse`:

> Both `syntax-parse` and the specification facility, syntax classes,
> use a common language of syntax patterns, which is described in detail
> in
> [Syntax Patterns](http://docs.racket-lang.org/syntax/stxparse-patterns.html).

Click... As you can see, `syntax-parse`'s pattern language is a bit
richer than `syntax-case`'s. EH-patterns! A-patterns! `~peek`!
`~peek-not`! O, wonder! How many goodly ~~creatures~~ patterns are
there here!..

...ahem. This works:

~~~ scheme
(define-syntax (swap stx)
  (syntax-parse stx
    [(_ (~var x id) (~var y id)) #'(let ([t x])
                                     (set! x y)
                                     (set! y t))]))
~~~

~~~ scheme
> (swap 3 x)
swap: expected identifier in: 3
~~~

`~var x id` means that `x` must belong to the class of identifiers
(the full list of standard classes is
[here](http://docs.racket-lang.org/syntax/Library_Syntax_Classes_and_Literal_Sets.html)).
Moreover, there turns out to be a convenient shortcut:

~~~ scheme
(define-syntax (swap stx)
  (syntax-parse stx
    [(_ x:id y:id) #'(let ([t x])
                       (set! x y)
                       (set! y t))]))
~~~

*Moreover*, I can define my own
[conventions](http://docs.racket-lang.org/syntax/Literal_Sets_and_Conventions.html#%28form._%28%28lib._syntax%2Fparse..rkt%29._define-conventions%29%29)
for literal names!

~~~ scheme
#lang racket

(require syntax/parse)
(require (for-syntax syntax/parse))

(define-conventions xyz-as-ids
  [x id] [y id] [z id])

(define-syntax (swap stx)
  (syntax-parse stx #:conventions (xyz-as-ids)
    [(_ x y) #'(let ([t x])
                 (set! x y)
                 (set! y t))]))
~~~

~~~
syntax-parse: expected identifier defined as a conventions in:
xyz-as-ids
~~~

Oops. What's even more interesting, the example still works:

~~~ scheme
(define-conventions xyz-as-ids
  [x id] [y id] [z id])

(syntax-parse #'(a b c 1 2 3)
    #:conventions (xyz-as-ids)
    [(x ... n ...) (syntax->datum #'(x ...))])
~~~

~~~ scheme
'(a b c)
~~~

Wait... I can remember reading something about having to use
`syntax/parse` and `(for-syntax syntax/parse)` being a bad sign.
Where it was?.. Google says:
[here](http://docs.racket-lang.org/syntax/Phases_and_Reusable_Syntax_Classes.html).

> The phase level mismatch is easily remedied by putting the syntax
> class definition within a `begin-for-syntax` block: ...

Sigh. That's what I get for jumping back and forth instead of
*following* the tutorial.

~~~ scheme
#lang racket

(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-conventions xyz-as-ids
    [x id] [y id] [z id]))

(define-syntax (swap stx)
  (syntax-parse stx #:conventions (xyz-as-ids)
    [(_ x y) #'(let ([t x])
                 (set! x y)
                 (set! y t))]))
~~~

~~~ scheme
> (swap 3 x)
swap: expected identifier in: 3
~~~

## Back to `def`

Skipping the unfinished rest of chapter 16.1 because, because... fuck
it, just because. I'm not in the mood.

(By the by: why doesn't DrRacket indent `syntax-parse` correctly?
This is inconvenient.)

~~~ scheme
(define-syntax (def stx)
  (syntax-parse stx
    [(_ x ...) #'(define x ...)]))
~~~

Seems wrong to spend a day hopping around only to write *this*, but whatever.

## Improvement #1: multiple declarations

This is stupid:

~~~ scheme
(def x 1)
(def y 2)
(def z 3)
~~~

This – isn't:

~~~ scheme
(def [x 1] [y 2] [z 3])
~~~

Okay, hm. To define many things inside of a single expression, I can use
`begin` with multiple `define`s:

~~~ scheme
> (begin
    (define x 1)
    (define y 2))

> (list x y)
'(1 2)
~~~

Ha, the unanswered question from previous day finally came up. How do
I lift syntax expressions?

For instance, how can I write a macro which transforms `1 2 3` into
`'(3 2 1)`? This won't work:

~~~ scheme
(define-syntax (rev stx)
  (syntax-parse stx
    [(_ x ...) #'(reverse (list x ...))]))
~~~

The trouble is – as you can check with Macro Stepper – that `reverse`
is a part of generated code, while I want it to be *applied* to the
syntax object at compile time.

---

And, as usual, I've traded 15 seconds of asking a person on `#racket` for
15 minutes of reading the docs.

**Piece #1**:
[`syntax-e`](http://docs.racket-lang.org/reference/stxops.html#%28def._%28%28quote._~23~25kernel%29._syntax-e%29%29) “unpacks” one layer of syntax-ing:

~~~ scheme
> (syntax-e #'(1 2 (add1 3)))
'(#<syntax:6:15 1> #<syntax:6:17 2> #<syntax:6:19 (add1 3)>)
~~~

**Piece #2**:
[`datum->syntax`](http://docs.racket-lang.org/reference/stxops.html#%28def._%28%28quote._~23~25kernel%29._datum-~3esyntax%29%29)
packs everything back:

~~~ scheme
> (datum->syntax #f (list 1 2 3))
#<syntax (1 2 3)>
~~~

It doesn't do anything on objects which are already packed (unlike
`syntax`):

~~~ scheme
> (datum->syntax #f (list 1 #'2 3))
#<syntax (1 2 3)>

> #'(list 1 #'2 3)
#<syntax:40:4 (list 1 (syntax 2) 3)>
~~~

With these two pieces, reversing a list finally becomes an easy task:

~~~ scheme
(define-syntax (rev stx)
  (syntax-parse stx
    [(_ x ...) (define xs (syntax-e #'(x ...)))
               (define rev-xs (reverse xs))
               (datum->syntax stx (cons list rev-xs))]))

(rev 1 2 3)
~~~

---

I've just remembered about my favorite learning method: whenever you
feel like “there should be a library function for this”, **ditch all
guides and tutorials and read the fucking manual**. In this case,
reading the fucking manual turned up that there is a
`syntax-local-eval` function
[here](http://docs.racket-lang.org/reference/syntax-util.html#%28def._%28%28lib._racket%2Fsyntax..rkt%29._syntax-local-eval%29%29)
and it does exactly what I wanted ([remember?](#actual-evaluation)):

~~~ scheme
> (require (for-syntax racket/syntax))

> (begin-for-syntax (print (syntax-local-eval #'(+ 1 2))))
3
~~~

Now this should be enough to see how things are evaluated:

~~~ scheme
(define-syntax (evaluating stx)
  (syntax-parse stx
    [(_ x ...) (datum->syntax #f 
                 (syntax-local-eval #'(x ...)))]))
~~~

...turns out it isn't:

~~~ scheme
> (evaluating + 1 2)
?: literal data is not allowed;
 no #%datum syntax transformer is bound in: 3
~~~

Hm. Googling the error message turns up a couple of links, but they
aren't helpful. Some guy wrote something about
[context](http://lists.racket-lang.org/users/archive/2011-August/047666.html)...
Maybe it's because I thought that `datum->syntax` doesn't need any
context and wrote `#f` instead of `stx` like I always did before?

~~~ scheme
(define-syntax (evaluating stx)
  (syntax-parse stx
    [(_ x ...) (datum->syntax stx 
                 (syntax-local-eval #'(x ...)))]))
~~~

~~~ scheme
> (evaluating + 1 2 3)
6
~~~

(Now I wonder how it managed to compute `6` at all.)

---

I've spent the last hour trying to make my infix-macro show reduction
steps properly, and I think now it's not possible to do with just a
couple of added lines. Maybe later. G'night!

# Day 6

First, a fun fact: how'd you write a macros which takes many lists,
each with two elements, and reverses the order of those elements
inside each pair?

Answer:

~~~ scheme
(define-syntax (foo stx)
  (syntax-parse stx
    [(_ (a b) ...) #'(list (b a) ...)]))
~~~

~~~ scheme
> (foo (3 +) (5 /))
'(3 1/5)
~~~

Lesson: I underestimated the power of ellipses. Don't be like me.

---

Behold, the `def` macro which handles multiple declarations!

~~~ scheme
(define-syntax (def stx)
  (syntax-parse stx
    [(_ (v:id ex:expr) ...)  #'(begin (define v ex) ...)]))
~~~

(`ex:expr` means that `ex` is an expression.)

~~~ scheme
> (def [x 1] [y 2] [z 3])

> (list z y x)
'(3 2 1)
~~~

## Improvement #2: defining functions together with variables

Should be pretty easy. Here's the syntax I want:

~~~ scheme
(def [(f x y)  (/ (+ x y) 2)]
     [pi       3.141592653589793])
~~~

(Interesting, I just found out I know pi exactly to the precision of
`double`.)

And here's the proposed desugaring:

~~~ scheme
(begin (define f (lambda (x y) (/ (+ x y) 2)))
       (define pi 3.141592653589793))
~~~

Well. First of all, let's define another macro – `def1` – which would
be handling a single definition.

~~~ scheme
(define-syntax (def1 stx)
  (syntax-parse stx
    [(_ v:id ex:expr)
       #'(define v ex)]
    [(_ (f:id p:id ...) body ...+)
       #'(define f (lambda (p ...) body ...))]))
~~~

(`...+` means “many, but at least one”.)

(For whatever reason DrRacket has been barfing at this definition when I
tried it with the example, but accepting it after swapping the clauses.
Ctrl-R didn't help and I had to restart DrRacket.)

Then `def` becomes a bit simpler:

~~~ scheme
(define-syntax (def stx)
  (syntax-parse stx
    [(_ (d ...) ...)  #'(begin (def1 d ...) ...)]))
~~~

(The reason for using `(_ (d ...) ...)` instead of `(_ d ...)` is that
I don't want to pass brackets to `def1`.)

## Non-improvement: recursive declarations

Originally I wanted to implement them too (and even written a couple
of paragraphs about how I found out that the standard way of producing
`undefined` in Racket seems to be `(letrec ([x x]) x)`), but then it
turned out to be working already. So, just see it in action (and
please don't blame me for stealing an example from the `letrec` docs):

~~~ scheme
(def [(is-even? n) (or (zero? n) (is-odd? (sub1 n)))]
     [(is-odd? n)  (and (not (zero? n)) (is-even? (sub1 n)))])
~~~

~~~ scheme
> (list (is-even? 6) (is-odd? 6))
'(#t #f)
~~~

## Improvement #3: multiple return values

The last enemy that shall be destroyed is ~~death~~ `let-values`,
which allows binding multiple return values. Unfortunately, the syntax
`let-values` uses clashes with syntax for function definitions.

What syntax should I choose? Something like `[_ (x y) (values 1 2)]`,
perhaps? But then I lose the ability to define `_`.

What is not definable?

Keywords!

~~~ scheme
> (define #:answer 37)
define: expected 42 in: #:answer 37
~~~

Just kidding, just kidding.

~~~ scheme
> (define #:answer 37)
define: bad syntax in: #:answer
~~~

And, luckily, `#:` also counts as a keyword. So, one more clause...

~~~ scheme
(define-syntax (def1 stx)
  (syntax-parse stx
    [(_ v:id ex:expr)
       #'(define v ex)]
    [(_ #: (v:id ...) ex:expr)
       #'(define-values (v ...) ex)]
    [(_ (f:id p:id ...) body ...+)
       #'(define f (lambda (p ...) body ...))]))
~~~

And here's how to use it:

~~~ scheme
> (def [#: (l r) (split-at (range 10) 5)])

> (list l r)
'((0 1 2 3 4) (5 6 7 8 9))
~~~

## Semi-improvement #4: indentation

For some reason I was thinking that indentation information is somehow
stored with functions/macros themselves. I was wrong.

To make `def` indent correctly, add it to
`Edit` → `Preferences` → `Editing` → `Indenting` → `Begin-like Keywords`.

To fix `syntax-parse`, add it to `Lambda-like Keywords` (I wonder why
it isn't there already).

## Making a module

`def` nicely concludes these three days I've spent learning about
macros. I intend to use it instead of various `let`-forms in the
future – so I'd better make a module exporting it.

Google “racket modules”. Since I'm feeling ~~sleepy~~ lucky, follow
the
[first link](http://docs.racket-lang.org/guide/module-basics.html).

> Each Racket module typically resides in its own file. For example,
> suppose the file `"cake.rkt"` contains the following module:
> 
> ~~~ scheme
> #lang racket
> 
> (provide print-cake)
> 
> ; draws a cake with n candles
> (define (print-cake n)
>   (show "   ~a   " n #\.)
>   (show " .-~a-. " n #\|)
>   (show " | ~a | " n #\space)
>   (show "---~a---" n #\-))
>  
> (define (show fmt n ch)
>   (printf fmt (make-string n ch))
>   (newline))
> ~~~

Okay, I added `(provide def)` to my file.

> The following `"random-cake.rkt"` module imports `"cake.rkt"`:
> 
> ~~~ scheme
> #lang racket
> 
> (require "cake.rkt")
> 
> (print-cake (random 30))
> ~~~

Not nice. I want `artyom/def` or something.

> A *collection* is a hierarchical grouping of installed library
> modules. A module in a collection is referenced through an unquoted,
> suffixless path. For example, the following module refers to the
> `"date.rkt"` library that is part of the `"racket"` collection:
> 
> ~~~ scheme
> #lang racket
> 
> (require racket/date)
> 
> (printf "Today is ~s\n"
>         (date->string (seconds->date (current-seconds))))
> ~~~

An unquoted, suffixless path... Yeah.

> You could add a new collection by placing files in the Racket
> installation or one of the directories reported by
> `(get-collects-search-dirs)`. Alternatively, you could add to the
> list of searched directories by setting the `PLTCOLLECTS`
> environment variable. The best option, however, is to add a package.

I'll check what `get-collects-search-dirs` returns just for the sake
of it, but
[the best option is probably the best one](http://xkcd.com/703/),
right?

~~~ scheme
> (require setup/dirs)

> (get-collects-search-dirs)
'(#<path:/home/yom/.racket/6.0/collects>
  #<path:/usr/share/racket/collects>)
~~~

> Creating a package *does not* mean that you have to register with a
> package server or perform a bundling step that copies your source
> code into an archive format. Creating a package can simply mean
> using the package manager to make your libraries locally accessible
> as a collection from their current source locations.
> 
> For example, suppose you have a directory `"/usr/molly/bakery"` that
> contains the `"cake.rkt"` module (from the beginning of this
> section) and other related modules. To make the modules available as
> a `"bakery"` collection, either
> 
>   * Use the raco pkg command-line tool:
> 
>     ~~~ bash
>     raco pkg install --link /usr/molly/bakery
>     ~~~
> 
>     where the `--link` flag is not actually needed when the provided
>     path includes a directory separator.
> 
>   * Use DrRacket's `Package Manager` item from the `File` menu. In
>     the `Do What I Mean` panel, click `Browse...`, choose the
>     `"/usr/molly/bakery"` directory, and click `Install`.

Okay.

~~~ bash
$ ls code/racket/artyom
def.rkt

$ raco pkg install code/racket/artyom
raco setup: version: 6.0 [3m]
raco setup: installation name: 6.0
raco setup: variants: 3m
...
raco setup: 3 making: <pkgs>/artyom
raco setup: --- creating launchers ---
raco setup: --- installing man pages ---
raco setup: --- building documentation ---
raco setup: --- installing collections ---
raco setup: --- post-installing collections ---
~~~

Wow, so simple. Check:

~~~ scheme
#lang racket

(require artyom/def)

(def [success? #t])

success?
~~~

~~~ scheme
#t
~~~

Does it automatically pick up changes? To test this, I'm going to
sneakily edit `def.rkt` without telling anybody.

~~~ scheme
(provide deaf)

...

(define-syntax (deaf stx)
  ...
~~~

A-a-and... Yes! It behaves just as expected – no source–bytecode
incompatibilities, no cached definitions, nothing, it just picked up
the updated `def.rkt` right after reload. I didn't even have to
restart DrRacket. Awesome. Gotta use `def` everywhere from now on.

G'night.

# P.S.

  * I'm afraid that with the level of flexibility Racket allows, I'll
    never get to writing an actual program – most likely I'll be
    spending my time improving Racket itself (well, or at least
    changing it to suit my tastes). It's a dangerous road, because
    ultimately you can't really *improve* the tool you're not
    *using*. So, as a public promise (?), I want to state that in less
    than a year I'll write and open-source some medium-sized project
    in Racket. (And, of course, I'll be documenting the process.)

  * I need not say anything about macros at this point, do I.

  * This part turned out to be both shorter and more tangled than the
    previous part. Sorry.

# Comments

## gus_massa (from HN)

...has posted a
[comment](https://news.ycombinator.com/item?id=7652680) in which
ne's nitpicking about `define-syntax-rule`; in particular, it seems to
be that `define-syntax-rule` works with patterns just fine.

Hm!

What do the docs say again?

> Equivalent to
> 
> ~~~ scheme
> (define-syntax id
>   (syntax-rules ()
>    [(id . pattern) template]))
> ~~~
> 
> but with syntax errors potentially phrased in terms of *pattern*.

Pattern, template... Okay, that's right.

Hm, “but”... What's it actual definition? (I wish Racket docs included
“source” links like Haskell docs do.)

I'll have to check it with DrRacket. First I need to type some valid
expression involving `define-syntax-rule` (is there a way to get to
the source without jumping thru such hoops, by the by?):

~~~ scheme
(define-syntax-rule (blah) blah)
~~~

Then I can use `Open Defining File` from context menu of
`define-syntax-rule`. It opens `misc.rkt`, which contains a rather
lengthy definition consisting of `pattern-failure`, which, well, seems
to be used for reporting pattern matching failures, and
`define-syntax-rule` itself:

~~~ scheme
(define-syntax define-syntax-rule
  (lambda (stx)
    (let-values ([(err) (lambda (what . xs)
                          (apply raise-syntax-error
                                 'define-syntax-rule what stx xs))])
      (syntax-case stx ()
        [(dr (name . pattern) template)
         (identifier? #'name)
         (syntax/loc stx
           (define-syntax name
             (lambda (user-stx)
               (syntax-case** dr #t user-stx () free-identifier=? #f
                 [(_ . pattern) (syntax-protect (syntax/loc user-stx template))]
                 [_ (pattern-failure user-stx 'pattern)]))))]
        [(_ (name . ptrn) tmpl)         (err "expected an identifier" #'name)]
        [(_ (name . ptrn))              (err "missing template")]
        [(_ (name . ptrn) tmpl etc . _) (err "too many forms" #'etc)]
        [(_ head . _)                   (err "invalid pattern" #'head)]))))
~~~

Hm, okay, what am I protected against?

### `"expected an identifier"`

~~~ scheme
(define-syntax-rule ('not-an-id x ... p)
  (p x ...))
~~~

~~~
define-syntax-rule: expected an identifier in: (quote not-an-id)
~~~

Nice.

~~~ scheme
(define-syntax ('not-an-id stx)
  (syntax-case stx ()
    [(_ x ... p) #'(p x ...)]))
~~~

~~~ scheme
> ('not-an-id 1 2 3 +)
quote: received value from syntax expander was not syntax
  received: #<procedure>
~~~

Not nice – it didn't even get caught at compile-time.

### `"missing template"`

~~~ scheme
(define-syntax-rule (foo x ... p))
~~~

~~~
define-syntax-rule: missing template in:
  (define-syntax-rule (foo x ... p))
~~~

I can guess what `define-syntax` will do...

~~~ scheme
(define-syntax (foo stx)
  (syntax-case stx ()))
~~~

~~~ scheme
> (foo 1 2 3 +)
foo: bad syntax in: (foo 1 2 3 +)
~~~

---

I won't bother checking the rest.

I won't bother using `define-syntax-rule` either, tho. If I ever need
to extend a macro defined with it, it'll be more troublesome to switch
to `syntax-parse` than if I used it right from the start. Not to
mention that `syntax-parse` supports more cool stuff and *who doesn't
like cool stuff?*

(Oh, and if I ever get tired of typing
`define-syntax ... stx ... syntax-case ... stx`,
I'll be sure to write a macro to make it easier.)

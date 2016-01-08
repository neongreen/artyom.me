% A long, long, long FAQ about Haskell

---
unfinished: true
---

# What is Haskell?

It's a programming language, like C++, Java, Python, or Javascript (but it's rather different from all languages I listed).

# Is this going to be Haskell propaganda?

No! I hate Haskell with all my heart.

# Seriously?

Just kidding. I like Haskell, but it's not the primary reason I'm writing this. 1st of all, I hate stupid “Haskell is so awesome” spiels (you can find a lot of them out there), and 2nd, I like writing, especially when it only requires typing and doesn't require much thinking.

# Okay, what can I use Haskell for?

For writing programs! Or games. Or sites, why not. Or you can use it for quickly doing things (like calculating something, or drawing something [like this](http://projects.haskell.org/diagrams/gallery/images/Gray.big.png), or whatever).

# Would I like Haskell?

Hard to say.

Haskell is one of those languages that can be more fun to *learn* than to get stuff done with, since Haskell allows more complexity than most other popular languages. If you like math, or ever tried to invent your own language (programming or not), or generally regard “figuring new stuff out” as a cool way to procrastinate, you would probably enjoy Haskell.

Haskell is pretty concise and lets you get rid of lots of repetition in your code; something that takes 500 lines in C++ might take 50 lines of Haskell and would be just as understandable. If you have a visceral reaction to repetition that goes like “repetition is bad”, you would like that feature of Haskell, but I guess many people actually don't have such a reaction and don't mind copying code when they need to. (Just in case: I'm not implying either that people who copy code are bad *or* that getting rid of repetition is a useless feature! I'm merely saying that some people dislike repetition even in small doses, and for them using Haskell is going to be nicer than using other languages.)

There's more logic/structure in Haskell than in other languages – and again, some people like it and some don't care about it. Imagine a language where all nouns end with “e” and all adjectives end with “o”; does this sound like a potentially nice idea? If yep, you might like Haskell.

On the other hand, I think Haskell is worse for getting-things-done than e.g. Python, at least until you're quite experienced in it. Here's why:

* A lot of libraries for common tasks are either missing or written in vastly different styles (and so you have to figure each new one from scratch).

* It can be hard to understand in what order things happen in Haskell and how it allocates memory, so you might end up with a slow program that uses too much memory and you would spend a lot of time trying to debug it.

* Things that should be simple are implemented in pretty complicated ways under the hood, and so when something goes wrong you sometimes have no idea how to fix it (apart from asking someone).

* There are often several competing ways to do the same thing (some libraries use one type for text, some use another type, you'd have to convert between those types; some libraries use one way to report errors, some use another, and again there's no clear winner; and so on).

* Writing small, one-off programs/scripts is often annoying, because:

    * libraries are optimised too much for “professional” usage and not enough – for messing around
    * mutable variables are inconvenient to use
    * there's no automatic coercion between e.g. numeric types
    * you have to import a dozen modules to write anything remotely complicated

Overall, the answer is “you would like Haskell *a lot* if it happens to correspond to the way you think, and you would like Haskell if you enjoy some challenge, but otherwise for many uses it's inferior to mainstream languages”.

# This list of shortcomings looks pretty damning, to be honest.

Well, it may *look* damning but it isn't – simply because if it was actually damning, there wouldn't be any people who love Haskell. What this list shows is that Haskell is bad for beginners who want to write something useful and aren't attracted to Haskell per se. However, there are also:

  * people (like me) who can spend *years* not writing anything useful and just enjoying using/learning/reading about the language

  * people who aren't beginners and who have different priorities (speed of development, safety, etc.)

  * people who would benefit from learning Haskell even if they don't plan to use it

# How exactly does one benefit from learning Haskell?

Haskell makes people write in a certain style (lots of small composable functions, creating new types for things that shouldn't be mixed, avoiding global state, using abstractions and creating your own abstractions) that is rather different from the style people usually use for Python/C++/Java. The habit of using that style remains even after switching from Haskell to some other language.

Here's a [quote](http://dubhrosa.blogspot.co.uk/2012/12/lessons-learning-haskell.html) illustrating what I mean:

> [...] I must have typed `for (size_t i=0; i<...` just about a million times and I was sick of it. C++ teases with approximations to `map`, `filter`, `fold`, `scan` just enough so that you'll try them for a few months until you eventually give up or your colleagues smack you. When I want to filter items from a container, I don't want to start by saying `for(size_t i=0...`. I want to say `filter f xs` and I want my colleagues to read that too.
>
> It might seem like an overreaction. But even in big classfull C++ projects, where I was senior developer, I spent my days writing functions. Functions consisting of loops and branches, because C++ didn't do a great job of accommodating “operations on containers”. Despite all the guff written about STL separating iterators from algorithms from containers (from allocators... ahem), nobody provided a simple set of primitive container operations that regular programmers would use.
>
> Using Haskell for a while, the effect goes further. It forced me to think of every such problem as a chain of the primitive list operations, maps, folds, filters and scans. Now I always think in these terms. I “see” the transformation of a container as a simple sequence of these operations. Before, I would have thought in terms of munging multiple actions into the body of a single loop.
>
> I see things using higher level concepts, and I write my comments and code with these in mind. I usually still reach for the trusty for loop in C++, but I'll factor together common container operations where appropriate into higher level functions. Filtering is a really common example that seems to come up all the time.

Here's [another](https://www.quora.com/In-what-specific-manner-did-learning-Haskell-make-you-a-better-programmer/answer/Jean-Yang):

> Getting used to having abstract data types and pattern matching makes me define and leverage type hierarchies and enumerations whenever I can, as opposed to passing integers and strings around. This reduces the chances I will make some silly error that I spend hours tracking down – and also makes the code more self-documenting and easier to come back to for maintenance purposes.

And [another](https://www.reddit.com/r/haskell/comments/3absc6/how_did_haskell_make_you_a_better_programmer/csbd3tk):

> I've been writing a lot of Javascript lately and Haskell has my poor-code Spidey senses turned way up. I can't tolerate “incomplete matches” and find ways to avoid boolean blindness as a rule. I am constantly papering over Javascript's wildly duplicitous APIs with simple, consistent ones inspired by ADTs. I also write APIs which respect abstraction in a way that's nearly impossible to conceive of in normal JS usage.
>
> It's a huge step up. I still write Javascript, but I write things that a multitudes simpler, dumber, easier to understand. 

And [another](https://www.reddit.com/r/haskell/comments/3absc6/how_did_haskell_make_you_a_better_programmer/csb6sge):

> It is without irony when I say that coding in Haskell allows me to be a whole lot dumber, letting the compiler guide me. I'm trying to recreate that experience to the best of my abilities in other languages. In particular,
>
> * Make undesirable state unrepresentable in the system, or at least so hard to create that tests are bound to catch it even when that's not what they're actually testing
> * Limit the range of implicit state as much as you can (given OOP languages that's pretty futile to be honest though)

(I apologise for the slightly overexcited tone of these quotes.)

Of course, this effect isn't specific to Haskell; learning J, Racket, Prolog, F#, Nim, Rust, etc. would likely make you a better programmer too (or at the very least teach you some interesting patterns and tricks). So, more interesting questions would be “is learning Haskell a good time investment?” and “is there some other language that would be an even better time investment?”. Unfortunately, I don't have answers to either of these questions, but

Ada, Python, Ruby, C#, Swift, R, Dart, Go, Scratch, Scala, Prolog, Erlang, F#, Haskell, Scheme, ML, Rust, Alice, Clojure, Elixir, Wolfram Language, Forth, Go, Icon, J, Julia, Io, OCaml, Oz, Smalltalk, Clean, Mercury, Pure, Red, Io, Racket, Factor, Lua, Coffeescript, Purescript, Julia, Common Lisp

-----------------------------------------------------------------------------

(I don't know of any studies that show that this style is actually *better*, and conducting such studies would likely be pretty hard, but it seems to be widely accepted that 

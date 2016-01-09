% A long, long, long FAQ about Haskell

---
unfinished: true
---

# What is Haskell?

It's a programming language, like C++, Java, Python, or Javascript (but it's rather different from all these languages).

# Is this going to be Haskell propaganda?

No! I like Haskell, but it's not the primary reason I'm writing this:

  * I hate stupid “Haskell is so awesome” spiels (you can find a lot of them out there) so I wanted to write something more-or-less objective

  * I like writing, especially when it only requires typing and doesn't require that much thinking (as long as I notice whenever I try to use weasel words)

# Okay, what can I use Haskell for?

For writing programs, or games, or sites (why not). Or you can use it for quickly doing things (like calculating something, or drawing something [like this](http://projects.haskell.org/diagrams/gallery/images/Gray.big.png), or whatever).

Same as most other programming languages, really. (There are things Haskell is better/worse at than other languages, but I'll talk about them later.)

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

> I've been writing a lot of Javascript lately and Haskell has my poor-code Spidey senses turned way up. I can't tolerate “incomplete matches” and find ways to avoid boolean blindness as a rule. I am constantly papering over Javascript's wildly duplicitous APIs with simple, consistent ones inspired by [ADTs](@w:algebraic data type). I also write APIs which respect abstraction in a way that's nearly impossible to conceive of in normal JS usage.
>
> It's a huge step up. I still write Javascript, but I write things that a multitudes simpler, dumber, easier to understand. 

And [another](https://www.reddit.com/r/haskell/comments/3absc6/how_did_haskell_make_you_a_better_programmer/csb6sge):

> It is without irony when I say that coding in Haskell allows me to be a whole lot dumber, letting the compiler guide me. I'm trying to recreate that experience to the best of my abilities in other languages. In particular,
>
> * Make undesirable state unrepresentable in the system, or at least so hard to create that tests are bound to catch it even when that's not what they're actually testing
> * Limit the range of implicit state as much as you can (given OOP languages that's pretty futile to be honest though)

(I apologise for the slightly overexcited tone of these quotes.)

# So, learning Haskell could be a good time investment, right?

I don't know. Many people think it is, but I've no idea how many of those people would've liked Haskell even if it wasn't a good time investment (possibly a lot?); besides, even if Haskell makes you a better programmer, it might be still not a good time investment in terms of cost/utility. An easier question to answer would be “*if* you want to learn a new language in order to become a better programmer, what language should you learn?”. If you already know some functional language, “Haskell” is probably not going to be the right answer, so let's additionally assume that:

  * you either don't know any programming languages or only know some of the mainstream ones (like Python, C++, or Java)
  * you are doing it in order to become a better programmer, so availability of jobs/libraries/whatever doesn't come into picture

Now, I have made a huge list of languages by mixing together TIOBE's index, Quora, and Wikipedia, and I'm going to strike things out until only a few languages remain.

First, Python, C++, Ruby, Java, and Javascript are out because they are mainstream imperative languages. Then go “better-than-X” languages (because they're more convenient and sometimes incorporate features from languages like Haskell but but they aren't really different):

  * Kotlin (a better Java)
  * Coffeescript, Typescript, Dart (a better Javascript)
  * C#, D (a better C++)
  * Swift (a better Objective-C)

Overly specialised languages are out as well:

  * R (a language for statistics)
  * Julia (a language for technical computing)
  * Coq (a proof assistant more than a language)

I'll leave only 1 language in each group of similar languages (where “similar” is subjective and I might well be mistaken):

  * Scheme, **Racket**
  * Arc, **Clojure**
  * Forth, Joy, **Factor**
  * APL, **J**
  * Clean, **Haskell**
  * Purescript, Elm, **Haskell**
  * Erlang, **Elixir**

This all, however, doesn't help *that* much. How can we decide between Clojure and Racket? What about Agda vs Idris, or F# vs OCaml vs Scala? Or more obscure languages like Factor or Shen? Oh, and there are even [arguments](http://c.learncodethehardway.org/book/introduction.html) that C, of all things, makes you a better programmer (and I didn't even include C in my list):

> No matter what your background, you are probably bad at four skills:
>
> Reading And Writing
>
> :   [...] generally I find programmers do too much “skimming” and have problems reading for comprehension. They'll skim code they need to understand in detail and think they understand it when they really don't. Other languages provide tools that also let them avoid actually writing any code, so when faced with a language like C they break down. [...]
>
> Attention To Detail
>
> :   Everyone is bad at this, and it's the biggest cause of bad software. Other languages let you get away with not paying attention, but C demands your full attention because it is right in the machine and the machine is very picky. [...]
>
> Spotting Differences
>
> :   A key problem people from other languages have is their brain has been trained to spot differences in that language, not in C. When you compare code you've written to my exercise code your eyes will jump right over characters you think don't matter or that aren't familiar. I'll be giving you strategies that force you to see your mistakes, but keep in mind that if your code is not exactly like the code in this book it is wrong.
>
> Planning And Debugging
>
> :   I love other easier languages because I can just hang out. I type the ideas I have into their interpreter and see results immediately. They're great for just hacking out ideas, but have you noticed that if you keep doing "hack until it works" eventually nothing works? C is harder on you because it requires you to plan out what you'll create first. [...]
>
> Learning C makes you a better programmer because you are forced to deal with these issues earlier and more frequently. You can't be sloppy and half-assed about what you write or nothing will work. The advantage of C is it's a simple language you can figure out on your own, which makes it a great language for learning about the machine and getting stronger in these core programmer skills.

Okay, let's throw out the list and approach this from another angle. Googling “makes you a better programmer” gives us the following list of languages: Haskell, Lisp, C, APL/J/K, Clojure, F#, Scala, Erlang, Smalltalk. We've already heard arguments for Haskell and C; what about the rest of them?

(I won't be giving sources for quotes 'cause it's a bit tedious (and also some come from ebooks), but they're all easily googleable.)

(If you want to skip the quotes, [click here](#skip-quotes).)

**Erlang:**

> Erlang will make you appreciate fault tolerant design. [Pattern matching](http://c2.com/cgi/wiki?PatternMatching) is such an amazing thing that you will miss it in your language and immutability is a safety net that you will wish you had in your language. In short, it will probably make you aware of a few mistakes that you were unwittingly committing in your language.

<!-- -->

> I found that being forced into the [actor concurrency model](@w:actor model) and building up intuitions about it as a result of using Erlang helped make me better at reasoning about concurrency in systems in general. Even though it's not always the most appropriate abstraction in practice, it's a very useful data point to have in evaluating the tradeoffs when designing distributed systems.

<!-- -->

> What Erlang will make you better at is at changing not your fine grained programming skills, as much as influencing your attitude towards programming. Erlang will help you to build your skills in a manner where you consciously factor in concurrency as a capability and bake it into your solutions. Erlang will help you build the attitude that it is not fine grained error avoidance / defensive programming that's critical, but that it's important to build systems that can continue to be functional even as some other parts of them might be failing. Erlang will encourage many aspects of software operations that might often be overlooked in typical programming and really show you how many of these can be weaved right into the fabric of your programs.

**APL/J/K:**

> I recommend people study APL/J/K for the same reason: it has the right primitives that make everything expressible as maps, folds, filters, scans, ranges, and stuff like that.
>
> Even more so than Haskell.
>
> It doesn't have monads and the kind of abstractions that lets you modify control flow semantics. It doesn't even have the facilities to build abstract data types – which makes you work with less-abstract data, and realize that although some abstraction is useful, most of what is practiced today is useless.
>
> APL/J/K promote, at the same time, container abstraction, and concrete down-to-the-metal real work.

**Scala:**

> But learning Python won't greatly add to your skill set at a deep level.  Scala will. It will teach you many more new abstractions, particularly in its type system, functional programming features, and emphasis on [immutable data](@w:immutable object). These will make you a much better programmer, even when you're using other languages. While you won't see a lot of job ads looking for Scala, some employers (even those mostly using languages like Java or C++) look for skills in languages like Scala as way to distinguish great programmers from cannon fodder.

<!-- -->

> First of all, it has an incredible amount of depth in its type system, which attempts to unify the philosophies of ML and Java and (in my opinion) does a damn impressive job. The first half of *Types and Programming Languages* is, roughly speaking, the theoretic substrate for ML. But ML doesn't have a lot of the finer features. It doesn't have subtyping, for example. Also, the uniqueness constraint on record and discriminated union labels (necessary for full Hindley-Milner inference, but still painful) can have a negative effect on the way people write code. The second half of *TAPL*, which vanilla ML doesn't really support, is realized in Scala. Second, I think Scala is the language that will salvage the 5 percent of object-oriented programming that is actually useful and interesting, while providing such powerful functional features that the remaining 95% can be sloughed away. The salvage project in which a generation of elite programmers selects what works from a variety of programming styles – functional, object-oriented, actor-driven, imperative – and discards what doesn't work, is going to happen in Scala. So this is a great opportunity to see first-hand what works in language design and what doesn't.

**F#:**

> F# offers an approach to computing that will continue to surprise and delight, and mastering functional techniques will help you become a better programmer regardless of the language you use.

<!-- -->

> Furthermore, F# and functional programming makes you a better programmer even if you do not end up using the language immediately after the course.

<!-- -->

> Learning F# will not only make you more productive, it will also make you a better programmer. Once you have mastered concepts such as functional programming introduced in F#, you can apply them to programs written in other languages, as well as have a new perspective on programming.

**Clojure:**

> Finally, there's Clojure which is unique amongst the languages you list in two ways: it is the only dynamically typed language, and it is the only Lisp. I like it best for those two reasons but it's not for everyone. In the same way that Haskell will make you a better programmer, a Lisp will also make you a better programmer (in different ways, perhaps), because it is a very different idiom from “traditional” programming languages. Everything is data, and in Clojure it is all about the abstractions (sequences, protocols, etc). Clojure combines the best of OOP (several forms of a la carte polymorphism) with the best of FP (immutable data structures, a focus on small pure functions), as well as offering a pragmatic way to deal with mutable state ([Software Transactional Memory](@w:software transactional memory)) for practical, real-world FP.

> Haskell is fantastic and learning it changed how I think about a lot of software problems in ways that made me a better programmer in general. It was my first experience with good Type Inference and showed me that with the right language you can get all the great benefits of strong typing without most of the pain normally associated with that.
>
> But Haskell just didn't stick with me, I didn't end up making enough useful things with it. I'm not sure of why that was, but part of it was that the web libraries were weak, they had interesting and useful features but were mostly, in my opinion, solving problems in the wrong way or solving the wrong problems. If my area of main expertise and focus wasn't web stuff this would be different and Haskell would have been fantastic.
>
> Clojure had equally strong changes to how I think about programming by finally showing me why I want real [macros](http://clojure.org/macros) and lispy syntax.

**Lisps in general:**

> There may be other reasons as well, but I believe the above are the main ones: Lisp languages fully support both functional and imperative programming, you can create any construct you want (including control constructs like for or while if they are not provided by the library) and the syntaxes to use it, you can therefore make your own language elements and use them (in the same program, often in the same file, you have implemented them). You can then customise the basic functional language, customise (or implement, in the Lisps that do not have it) the basic imperative language, implement (or customise if you have them as libraries) declarative languages, object oriented languages, logic languages or whatever you want.
>
> Being able to do all that and also learning to do it and doing it is what will teach you to think in different ways. And that's what makes you a better programmer.

<!-- -->

> Normally code is like an assembly line – data goes in, follows one of the predetermined paths, and comes out the other end transformed in some way. In Lisps, the assembly line can also assemble itself, according to rules you  specify, without having to stop (run-time and compile-time can be interleaved arbitrarily). This allows you to easily build programs as layers of languages, which in turn gets you thinking about how languages work.

<!-- -->

> If you come from an imperative/procedural world, and you plan to learn Lisp and use Lisp's functional style, Lisp will help you learn very useful concepts that you may even be able to apply when you go back to your imperative language (or perhaps will never go back again). Some of these concepts are: higher order functions (pass functions around as if they were data), recursion (particularly with tail call optimization, which allows you to recurse indefinitively without running out of stack), lexical closures (pass state around safely), continuations (continuation passing style is a style of programming where the program flow is passed as a parameter of the function) and macros (the godly experience of creating a Domain Specific Languages that can make you far more productive by letting the Lisp runtime type your programs for you). An interesting observation is how, once you start grasping the concepts of the functional style, your programs start to look different: you start focusing more in data structures to drive your program rather than control structures, and you realize that even the layout of your old imperative programs looks odd; your old procedures would probably look like an inside-out version of your new functional style functions.

<!-- -->

> Lisp (and Scheme in particular) will teach you how to do things with very little. The syntax is so minimal it changes your view of programming languages, and certain constructs in particular.

<!-- -->

> Lisp is worth learning for the profound enlightenment experience you will have when you finally get it. That experience will make you a better programmer for the rest of your days, even if you never actually use Lisp itself a lot.

<!-- -->

> [by] using Lisp, you get a better idea about what an ideal interface should look like, and what in your code can be abstracted away as data. It also helps you to see your favorite language as a big data structure, and to better understand its semantics.

**Smalltalk:**

> Clojure is just a Lisp on the JVM. If you want to learn a Lisp, Scheme or CL would be better choices since they don't have all that JVM noise going on. If you want to learn the Java platform, learn Java because it doesn't have all that Lisp noise going on (not that learning a platform will make you much of a better programmer in the first place, and the Java language is pretty boring and narrow-minded as far as languages go.)
>
> Smalltalk, on the other hand, is probably the language that has taken class-based object-orientation the farthest. It's one of the “extreme” languages that will change how you think about programming.

While googling, I was also collecting opinions *against* languages I listed, so here are some of them:

**Scala:**

> Scala is a complex language, with a complex type system (that Odersky et al are working to simplify in the future) and it is also a hybrid OOP/FP language which means you can easily write non-FP code and miss some of the real benefits (and good concepts) of FP. I don't recommend it as a “first FP language” and only partially recommend it as a “better Java”. Don't get me wrong: it's a very impressive and capable language but I think you need a solid FP background before you can use it effectively.

<!-- -->

> Scala's type system is pretty complex, which can be a hindrance to those who don't take the time to understand it or a help to those who do. It is more multiparadigm than strictly functional, meaning, you can mix imperative, object-oriented, and functional code. This enables you to ease into functional programming, but may also prove to be a hindrance as you may frequently be tempted to just go imperative if you can't figure out a functional approach.

<!-- -->

> Haskell is simply more advanced that Standard ML or OCaml, the only ML dialects that are widely used. [...] Scala is an immensely bloated language. My instinctive reaction to Scala was that something that ugly can't have a clean implementation, and consequently I was not overly surprised when Paul Phillips, the main compiler writer on the Scala team, called it quits, and went on what seems like a retribution tour, spilling the beans on the nastiness hidden in the Scala compiler. It's quite fascinating to watch his presentations.

<!-- -->

> Besides this, Scala has way too much Java in it to really be “algebraically elegant” or even simple! Scala is a compromise. It has the legacy of Java compatibility dragging it down. That's one of the main reasons it's caught on, but it's also the reason it isn't actually particularly simple or elegant.
>
> [...]
>
> In embracing Java and OO, it throws away many of the advantages of functional programming. In fact, any non-trivial functional programming quickly becomes unbearable. Especially compared to Haskell.

**Lisp:**

> However, there are a couple of situations where Lisp may not give you much more than what you have. For example, if you already code in another functional programming language, you probably know most of the concepts [higher order functions, recurision, etc.] that I listed above and you may end up missing the safety of static typing (if you come from Haskell, for example).

**Clojure:**

> But this wasn't enough for me. Clojure triggered something inside me / I loved pure code, it was just natural to reason about, without moving parts. Maybe difficult to write but trivial to test, use and refactor. But, completely pure code was becoming a nightmare to write as projects got bigger. For example I ended up with lots of extra arguments on my functions, explicit state, or often it could become difficult to reason about complex abstractions.
>
> And I wanted more.
>
> So I learnt Haskell.
>
> I learnt that [Functor](https://en.wikibooks.org/wiki/Haskell/The_Functor_class) and [Applicative](https://pbrisbin.com/posts/applicative_functors/) give you pragmatic ways to handle a million different complex data structures and abstract data types without caring about their implementation. I learnt that Monad gives you rational ways to structure logic and the order of computations, giving you more power than in an imperative language I know (“programmable semicolons!”). I learnt that you can handle errors in pure and explicit ways. I discovered that almost everything can be composable; I can have a thousand computations that may fail, run them in parallel trivially and still catch all those errors in a single line while using the same operators I use to print text to the screen. I fell in love with currying and how easily things can work together if the language lets them. Also I learnt that concurrency can be a beautifully simple endeavour, that there are actually a lot of ways to do it and that it actually makes things faster without adding unnecessary complexity. I learnt how rich types can give structure, meaning and modularity to a piece of code (almost) for free.

<!-- -->

> The most important reason is that Haskell can teach you advanced functional programming in a way no other common languages can – including Clojure! (Although Clojure is better than most alternatives.) Haskell elevates functional programming to an artform with an incredible array of libraries and rich abstractions that really *double down* on functional programming. 

**Haskell:**

(The post I'm taking this quote from isn't entirely serious, but it's still making a good point in my opinion.)

> I've found, contrary to what you sometimes read, that learning Python and Haskell has *not* improved my programming using other languages. Haskell in particular, being so different from imperative languages, is supposed to give new insights into programming that will help you even when you are not using the language. My current experience doesn't exactly tally with this, and here is why:
>
> [...]
>
> Using functional style obfuscates your code when using other languages.
>
> C# has begun to get some features that are more friendly to functional style programming. So, the other day, when faced with a very common situation I tried a functional solution. I have a list of `Foo` objects, each having a `Description()` method that returns a string. I need to concatenate all the non-empty descriptions, inserting newlines between them.
>
> The code I wanted to write was this Python code:
>
> ~~~ python
> "\n".join(foo.description() for foo in mylist
>                          if foo.description() != "")
> ~~~
>
> Or this Haskell:
>
> ~~~ haskell
> concat $ List.intersperse "\n" $ filter (/= "") $ map description mylist
> ~~~
>
> Using generics from C# 2.0 and the methods they contain, the best I got was:

> ~~~ cs
> string.Join("\n", mylist.ConvertAll<string>(
>             delegate(Foo foo)
>             {
>                     return foo.Description();
>             }).FindAll(
>             delegate(string x)
>             {
>                     return x != "";
>             }).ToArray());
> ~~~
>
> There are some big problems with the C# version. The first is that there is very little reduction in size versus the imperative style code, if any. [...] Second, it took me longer to write. I had to do some experimenting to see how much type information I had to add to get it to compile (e.g. adding an explicit cast for the delegate turned out not to be necessary, but I did have to specify `ConvertAll<string>` instead of `ConvertAll`).
>
> Finally, there is the problem that this code will get me into trouble with my colleagues. Why am I writing such complex code – using such advanced features as anonymous delegates – when a simple loop would have sufficed? I actually left my functional version in, but was so embarrassed about it I had to add a brief explanatory note.
>
> The fact is that functional idioms work badly in languages that don't have syntactic support for them. Java, from what I know, would have been much worse. C# suffers in that although some features that enable more functional programming have arrived in C# 2.0 (along with various other language improvements), huge chunks of .NET libraries have not been updated to take advantage of them, and our own code certainly hasn't.
>
> It might be argued that you can still use the principles of functional programming (no side effects, functions depend only on their inputs etc) and get benefits that way, even if you can't use the idioms. In reality, libraries and frameworks designed for imperative languages just don't work like that.

# Yeah, I skipped the quotes.    {#skip-quotes}

So, where does this all leave us?

* People like claiming that learning functional programming makes you a better programming, and by “functional programming” they mean a good type system, immutable data, purity/controlling side effects, composable functions, pattern matching, higher order functions, recursion, ADTs, and abstraction. Haskell, F#, Scala, Racket, and lots of other languages have all/most/many of those features.

* Some languages (Clojure, Common Lisp, F#, Scala) don't really force you into the functional paradigm, which makes them more pragmatic – and often better for getting stuff done – but probably less suitable for the purpose of making you a better programmer.

* Same for abstraction: Clojure doesn't seem to give as many possibilities for abstraction as Haskell does, or at least it doesn't force you into using them:

  > You really have to absorb and internalise all abstractions of [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia) and more to have a fighting chance at understanding how to do Haskell at scale. All those abstractions are legit and make sense in context (for example when you click that list is in fact a monad) but while in Clojure you can postpone learning the abstractions until it's necessary in Haskell there's no way of getting around making this investment up front, because having a static type system means you need to know how to talk to it.
  >
  > I think Haskell is a worthwhile investment to make if you believe in correctness, but it's an upfront investment you have to make and not everyone is up for that, especially if they need to have something done now and not two years later after monads finally click.

* On the other hand, too much abstraction can be bad, and there's a danger that after learning Haskell you would start writing *overengineered* code. How much of a danger? I don't know; I don't think it's too big, but it's still there.

* Scala is “a compromise”, complicated, possibly bloated, and at the same time not advanced enough compared to Haskell (since Haskell doesn't try to be everything at once, it can simultaneously have less features than Scala and be a more advanced functional language than Scala).

* It's hard to find anyone saying anything good about F# apart from, well, it being a functional language. It has .NET ecosystem and better IDE support than Haskell, but it's probably not better than Haskell when it comes to becoming a better programmer etc.

* Even if learning Haskell makes you the best programmer ever, it's just *not going to happen* if you end up giving up on Haskell (e.g. because you wanted to get something cool done fast and Haskell didn't let you do that):

  > [it's] just that people dread big up front investments in general, and Haskell just happens to be one. It pays off in the long run as you say, but you still have to make the jump, which is probably the reason most people don't.

* With Lisp you're going to use metaprogramming/macros/etc more often than with Haskell; whether it would make you a better programmer or simply a better *Lisp* programmer is up to a debate. I'd argue that the skills of decomposition and abstraction are more broadly applicable/useful than the skill of understanding how language design is done (not to mention that in Haskell you're going to see a lot of examples of [DSLs](@w:domain-specific language) too). However, “code is data” is still a nice lesson to learn and Lisp is the best language to learn it.

* Racket doesn't get many mentions, but on the other hand it's a Lisp and a Scheme and so things said about Lisp/Scheme apply to it just as well.

* Smalltalk is to OOP languages as Haskell is to functional languages. Could be worth learning.

* Erlang makes you think about concurrency and fault tolerance. (Descriptions of other languages don't mention that.)

* C makes you more disciplined/attentive/careful. (Again, descriptions of other languages don't mention that.)

* It's possible that languages like Shen, Red, Factor, Oz, Mercury, etc. would make you a better programmer too, but nobody is talking about them much and so it's hard to compare them (especially since I haven't used any of them). 

-----------------------------------------------------------------------------

All in all, Haskell wins at abstraction and forcing you into functional paradigm, Clojure wins at pragmatism, and some systems language (maybe C, maybe something else) wins at discipline. This leads to the following recommendation:

* Try learning Haskell.

* If after some time (say, a month) you find yourself confused and impatient (because of not producing anything useful), switch to Clojure.

* In addition to that, learn a systems language (C? Go? Rust? I've no idea, which). Whether it's more or less useful than learning Haskell, I can't say either.

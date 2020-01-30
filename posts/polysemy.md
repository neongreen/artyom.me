% Polysemy (i.e. overloading) in programming

Okay, first, what the hell is polysemy?

> [Polysemy](https://en.wikipedia.org/wiki/Polysemy) is the capacity for a sign (such as a word, phrase, or symbol) to have multiple meanings (that is, multiple semes or sememes and thus multiple senses), usually related by contiguity of meaning within a semantic field.

That is, polysemy is when a word has many meanings and those meanings are somehow connected (when they aren't connected at all, it's homonymy). An example: in English words “do” and “make” have dozens of meanings. For a smaller example, see “man”:

  * the human species (i.e. man vs. animal)
  * males of the human species (i.e. man vs. woman)
  * adult males of the human species (i.e. man vs. boy)

Note how the definitions, while different, are still related to each other – in this particular case, they include each other. (A man is a man is a man— er, a man is a male is a human.)

In more complicated cases the meanings form a graph; some meanings are related, some aren't, some connections are weak, some are strong. Here's a network of meanings of the word “head” (the one for “make” is much more impressive, but I can't find it):

![](https://i.imgur.com/MJqS2GS.jpg){width=100%}

An interesting thing is that we're not used to thinking that polysemic words *actually* have different meanings – it feels to us that there must be something that unites different meanings, and we just can't grasp it. In reality it's a trick your mind plays on you. First of all, whether or not 2 things feel similar *heavily* depends on whether the same word is used for them or not – for instance, “head” and “thumb” don't feel particularly similar, but if words for “head” and “thumb” were the same, your brain would immediately invent some reason to believe that it's not a coincidence – something like “thumbs look like heads” or “thumbs are “main” fingers”. Second, even tho each 2 connected words have something in common, it doesn't mean that the whole network does (and in almost all cases it doesn't). Consider these sentences:

  1. He gave me a book.
  2. He gave me a strange look.
  3. He gave me a hearing test.
  4. He gave me hell.
  5. He gave a bow.
  6. He gave no sign of life.

The *only* thing in common here is that somebody did something. No, wait, even that isn't true. Ha!

-----------------------------------------------------------------------------

And now let's talk about how it is related to programming.

“Give” is a verb that has several meanings, which in the context of programming can be translated as “overloaded function”. If you're a Java programmer, `give` is probably going to be an interface method. (If you're a Haskell programmer – a typeclass method.) Overloaded functions are pretty common – even if your language doesn't have classes or objects, it probably still has an overloaded `+` (which you can use to add integers or floats or sometimes even concatenate strings).

Furthermore, there are 2 kinds of overloading – unprincipled and principled; people often say that unprincipled overloading is bad (spoiler: the point of this post is to show that it's not), but first, what does it even mean?

Well, *principled* overloading means that there are laws that overloaded functions have to follow – for instance, `+` can add numbers but not anything else (strings, etc). The prime example of principled overloading is Haskell, with its culture of explicitly stating laws and reusing math concepts. Here's the description of [`Monoid`][], a class that would be called `Concatenable` (or something) in most other languages:

[`Monoid`]: https://hackage.haskell.org/package/base/docs/Data-Monoid.html

> The class of monoids (types with an associative binary operation that has an identity). Instances should satisfy the following laws:
>
> ~~~ haskell
> mappend mempty x = x
> mappend x mempty = x
> mappend x (mappend y z) = mappend (mappend x y) z
> mconcat = foldr mappend mempty
> ~~~
>
> Methods:
>
> * `mempty :: a` – identity of `mappend`
> * `mappend :: a -> a -> a` – an associative operation
> * `mconcat :: [a] -> a` – fold a list using the monoid

(Note that the laws aren't enforced by the compiler, but if you create an instance that violates the laws, people will shun you.)

In fact, the laws are commonly considered to be the *definition* of the class. What's a monad? Oh, it's just a type and 2 functions, and those functions have to satisfy 3 monad laws. Got it.

This is just an aside, but I want to point out that the problem with laws is that programming isn't math for most programmers, and a math-style definition-by-listing-properties is useless for them. Programmers are humans, they have networks of meanings in their heads, and if they don't have one for monads or `+` or `doTheThing` or whatever overloaded term you introduce, they won't be able to use it. The only way to build a network of meanings is to give people examples and wait until their brains connect them. Moreover, you [can't implant your network of meanings into someone else's head](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/), that's called the monad tutorial fallacy and it's a fallacy. But don't believe what the linked post claims:

> By examining concrete objects in detail, one begins to notice similarities and patterns, until one comes to understand on a more abstract, intuitive level.

There's no abstract level – you just have a bigger network of meanings and so fitting new things into it (or noticing when new things could be fitted into it) becomes easier for you. The feeling that you *really grasp* what unites the things in your network of meanings is just that – a feeling. At best, there might be several clusters of similar things, and an “intuitive” understanding of each cluster, but that's all.

The thing is, laws aren't that helpful. They say “screw networks of meanings, it's clearly easier to fit N laws into your head than some vague intuition”, which is wrong. (An aside: lots of arguments about programming language (and technical arguments in general) are really annoying because technical people often make “logical” arguments about how human brains work. Naive logic doesn't apply to brains, read any social psychology book for counterexamples. I liked Elliot Aronson's “The Social Animal”, for instance.)

I *don't* mean that laws are useless – they let you prove things about your code (like in math) and they let you mechanically transform your code (again, like in math, just remember your trigonometry class). But they do little to aid understanding, and the absence of them doesn't mean that a typeclass is worthless.

**Meaning doesn't come from laws – it comes from names and it comes from examples.** Or in other words, meaning comes from humans who read and use your code. It's all in their heads.

And if you believe that it shouldn't be like this, that meaning should be enforceable by the compiler, that it's very wrong for names of things to *actually matter*, etc, etc, well, math is your perfect language. Math is also a language where all variables are one-letter variables and where they have more operators than all other programming languages put together. It's not a coincidence, it's the result of math [being the least human-related field of all fields][xkcd Purity].

[xkcd Purity]: https://xkcd.com/435/

Again, this is important: I'm not against laws/promises/contracts/etc, and they are useful, but I'm against the notion that lawless, unprincipled overloading should be forbidden. While being able to prove things about your code is nice, very few programmers actually make use of it, and the damage done by surprising or unexpected variants of a function is always going to be greater than the damage done by lawless-but-expected meanings.

-----------------------------------------------------------------------------

Remember how I said that the point of this post is to show that unprincipled overloading isn't bad? Well, it's not. The *actual* point of the post is that the whole question doesn't make sense unless you specify in what circumstances it's good/bad, which is, incidentally, something that people never do. For instance: “I've been coding for 15 years, I have written about 250k lines of code and I can confidently say that overloading is bad (in fact, a while ago we at company X started using our own language, and it has no overloading on purpose).” Well, “I've been coding for 15 years” does *not* imply “I know what is good”, it implies “I know what's good for people who've been coding for 15 years and thus have completely different skills and priorities”. Okay, I'm willing to say that in enterprisey setting you might want to discourage overloading (perhaps because you have to read others' code often and being able to understand what's going on quickly is extremely important to you, or perhaps because you want to eliminate as many potential mistakes as possible, no matter the cost) – what now? How does it apply to the rest of the users?

On the other hand, “overloading saves a lot of typing and I've never had any bugs due to it” is equally meaningless. What if you have an IDE/editor with good autocomplete and other features that make “a lot of typing” a non-issue? What if after some time spent programming you stop caring about typing, just like after some time spent living you stop hating getting up early? What if you've never had to work in a team with more than 3 people and so there's a big chance that your intuitions about overloaded things will always match others' intuitions?

Can we please stop having stupid arguments about overloading?

(By the way, “saves a lot of typing” is not nearly the biggest benefit of overloading. The biggest benefit is that it turns long actions into short ones, thus reducing the amount of distraction. An example: “remember where the non-overloaded function lives, import the module, give it a name if it clashes with functions from other modules, get back to the place where you wanted to use the function” becomes just “use the function”. Another example, this time with functions having different names: “try to remember whether what I want is called `toString` or `fromText` or `decode` or `decodeUtf8` or `unpack`, give up, google” becomes “use the function that I use every time because it's always called the same”. Yet another example: “try to invent a unique name” becomes “use the name that everybody else uses for this kind of thing”; and at this point I should remind you that naming things is [one of the hardest things in computer science][hard].)

[hard]: https://twitter.com/codinghorror/status/506010907021828096

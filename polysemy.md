% Polysemy in programming languages

> [Polysemy](@w) is the capacity for a sign (such as a word, phrase, or symbol) to have multiple meanings (that is, multiple semes or sememes and thus multiple senses), usually related by contiguity of meaning within a semantic field.

That is, polysemy is when a word has many meanings and those meanings are somehow connected (when they aren't connected at all, it's homonymy). Examples: in English words “do” and “make” have dozens of meanings. In Russian the word “идти” has dozens of meanings. An example of a word with only several meanings is “man”:

  * the human species (i.e. man vs. animal)
  * males of the human species (i.e. man vs. woman)
  * adult males of the human species (i.e. man vs. boy)

Note how the definitions, while different, are still related to each other – in this particular case, they include each other. A man is a man is a man, or in other words, a man is a male is a human.

Here's a network of meanings of the word “head” (the one for “make” is much more impressive, but I can't find it):

![](https://i.imgur.com/MJqS2GS.jpg){width=100%}

We're not used to thinking that polysemic words *actually* have different meanings – it feels to us that there must be something that unites different meanings, and we just can't grasp it. Which is actually true, but it's also a trick your mind plays on you – if words for “head” and “thumb” were the same, your brain *would* find a way to unite them (it's actually pretty easy), but since the words aren't the same, you're willing to say that the meanings are different.

Now consider these sentences:

  1. He gave me a book.
  2. He gave me a strange look.
  3. He gave me a hearing test.
  4. He gave me hell.

If you're a programmer, it should be easy for you to understand how different the meanings of “give” are in these sentences: try to imagine how `give` could be implemented if it was a function.

(There's not going to be any similarities, in case you're wondering.)

-----------------------------------------------------------------------------

If you're a Java programmer, `give` is probably going to be an interface method. (If you're a Haskell programmer – a typeclass method.) And people are using them all the time; even if your language doesn't have classes or objects, it probably still has an overloaded `+` (which you can use to add integers or floats or even strings). So, what's the point of this post?

Ah, but there's no point yet. The point only begins when I mention that people constantly try to give *laws* for overloaded functions – they are okay with the same name referring to 2 different things, as long as you can give some promises about those things. Moreover, Haskell is one of the languages where it goes much further – the laws are commonly considered to be the *definition* of what the laws refer to. What's a monad? Oh, it's just a type and 2 functions, and those functions have to satisfy 3 monad laws. Got it. The problem is that programming isn't math for most programmers, and a math-style definition-by-listing-properties is useless for them. Programmers are humans, they have networks of meanings in their heads, and if they don't have one for monads or `+` or `doTheThing` or whatever overloaded term you introduce, they won't be able to use it. The only way to build a network of meanings is to give people examples and wait until their brains connect them. Moreover, you [can't implant your network of meanings into someone else's head](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/), that's called the monad tutorial fallacy and it's a fallacy. But don't think that

> By examining concrete objects in detail, one begins to notice similarities and patterns, until one comes to understand on a more abstract, intuitive level.

(as the linked post claims), there's no abstract level – you just have a bigger network of meanings and so fitting new things into it (or noticing when new things could be fitted into it) becomes easier for you. The feeling that you *really grasp* what unites the things in your network of meanings is just that – a feeling.

(By the way, sorry for not giving examples from languages other than Haskell. I simply don't know other languages, that's all.)

-----------------------------------------------------------------------------

Back to laws and promises, and back to the point, which is: laws don't work. They say “screw networks of meanings, it's clearly easier to fit N laws into your head than some vague intuition”, which is wrong. (An aside: lots of arguments about programming language (and technical arguments in general) are really annoying because technical people often make “logical” arguments about how human brains work. Naive logic doesn't apply to brains, read any social psychology book for counterexamples. I liked Elliot Aronson's “The Social Animal”, for instance.)

I *don't* mean that laws are useless – they let you prove things about your code (like in math) and they let you mechanically transform your code (again, like in math, just remember your trigonometry class). But they do little to aid understanding, and the absence of them doesn't mean that a typeclass is worthless.

**Meaning doesn't come from laws – it comes from names and it comes from examples.** Or in other words, meaning comes from humans who read and use your code. It's all in their heads.

And if you believe that it shouldn't be like this, that meaning should be enforceable by the compiler, that it's very wrong for names of things to *actually matter*, etc, etc, well, math is your perfect language. Math is also a language where all variables are one-letter variables and where they have more operators than all other programming languages put together. It's not a coincidence, it's the result of math [being the least human-related field of all fields][xkcd Purity].

[xkcd Purity]: https://xkcd.com/435/

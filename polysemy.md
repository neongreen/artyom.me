% Polysemy in programming languages (particularly Haskell)

> [Polysemy](@w) is the capacity for a sign (such as a word, phrase, or symbol) to have multiple meanings (that is, multiple semes or sememes and thus multiple senses), usually related by contiguity of meaning within a semantic field.

Examples: in English words “do” and “make” have dozens of meanings. In Russian the word “идти” has dozens of meanings. An example of a word with only several meanings is “man”:

  * the human species (i.e. man vs. animal)
  * males of the human species (i.e. man vs. woman)
  * adult males of the human species (i.e. man vs. boy)

Note how the definitions, while different, are still related to each other – in this particular case, they include each other. A man is a man is a man, or in other words, a man is a male is a human.

Here are some traits of polysemic words (according to some paper linked on Wikipedia):

  1. the various senses of a polysemous word have a central origin
  2. the links between these senses form a network
  3. understanding the ‘inner’ one contributes to understanding of the ‘outer’ one

And here's a sample network for the word “run”:

![](http://www.helsinki.fi/varieng/series/volumes/12/egan/egan_figure1.png)

In programming some things, too, have got lots of meanings. For instance, `for` might be able to traverse lists, arrays, maps, vectors, and so on. `+` might be able to add integers, rational numbers, strings, etc. Again, something like this exists in pretty much every widespread language to some extent, and it seems good to me.

The trouble appears when people start trying to bring this “chaos” to order. For instance, “`+` should work for numbers but not for strings”. Or “let's not have a notion of default values, because who decides whether the default value for `Int` should be 0 or 1? And why the default value for a list is “empty list”?” The general sentiment, at least in Haskell community (I speak about Haskell because it's my primary language), seems to be “let's not have one name refer to several things unless we can state a clear set of laws uniting those things”.

This approach has several benefits. With a set of laws existing, you can safely do transformations on your program – transformations you wouldn't be able to do otherwise. You might also be able to [deduce things](http://www.haskellforall.com/2013/12/equational-reasoning.html) about your program. Finally, often your code becomes more easily understandable because the reader can just apply the law instead of figuring out what the operation does in this particular case.

However, there's also something people are forgetting:

  * Human brains don't always work by logic. It's not correct that applying one of [monad laws](https://wiki.haskell.org/Monad_laws) is always easier than remembering that 0 is “arbitrarily” the default value for Int.

  * Humans mostly aren't malicious. If you don't specify a law that some function “openThingy” must obey, still nobody is going to purposefully create a function called “openThingy” that would actually close a thingy, or fire missiles, or whatever.

In English, “do” is often a placeholder for “default action”. Do drugs? Consume drugs. Do an accent? Speak with an accent. Do 70km/h? Drive with speed 70km/h.

Are these meanings arbitrary? Sort of, yeah. (Especially if you remember that “do somebody” means either “have sex with somebody” or “kill somebody”.)

Are they useful? I'd say they are. (It's not a coincidence, by the way, that the shortest and easiest to pronounce words are also the ones that have dozens of meanings.) When you have a cluster of things with similar meanings, it's often better to have a single word for all those things even if there's no clear law uniting them, and it's true for natural and programming languages alike, because both kinds are *human languages*. I'm not arguing that programming languages are literally like natural ones; I'm arguing that the most general principles (and ambiguity resolution in context is one of them) apply to all languages, be it design or art or programming or math (those all have their own languages, right).

Sometimes arguments about programming language (and technical arguments in general) are really really annoying because technical people often make “logical” arguments about how human brains work (and nobody ever links to any actual research). It's logical that short names get read faster than long names! It's logical that things should be descriptive instead of having people guess stuff from context! It's logical that `foo_bar` reads better than `fooBar`, because “\_” looks like a space and we're used to reading spaces! It's logical that links should be underlined! It's logical that text should be black on white! It's logical that Lisp's syntax is the best, because there's almost no syntax to remember! It's logical that ambiguity is bad!

Ye-eah, right.

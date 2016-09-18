---
pagetitle: artyom.me
---

# This previously said “hire me maybe”

Here's my [CV](/cv), but I work for [Serokell](https://serokell.io/) at the moment (among other things we're writing a [Bitcoin analog for banks](https://github.com/input-output-hk/rscoin-haskell)) and so probably won't agree to work for you right now. But hey, you can still write me and maybe in a year or so I'll be yours if you:

* are awesomer
* aren't awesomer but pay substantially more
* or you offer relocation to some nice European country

# Look at my Telegram channel

[Here it is.](https://telegram.me/lightgreen_life)

Some of the posts:

  * [How discounts, loyalty cards and customer questionnaires only exists for the needs of the business, not because they're being nice](https://telegram.me/lightgreen_life/8)

  * [A long, multi-part rant about how bureaucracy has infected programming](https://telegram.me/lightgreen_life/73)

  * [A comparison of Haskell and guitar](https://telegram.me/lightgreen_life/56)

  * [An ELI5 explanation of list fusion in Haskell](https://telegram.me/lightgreen_life/58)

  * [An amazing lifehack](https://telegram.me/lightgreen_life/60)

  * [A theory of boundaries](https://telegram.me/lightgreen_life/44)

  * An ad for Homestuck: [in all games invertory is basically an array, what if it was some other data structure?](https://telegram.me/lightgreen_life/15)

  * [How people feel about cultural appropriation](https://telegram.me/lightgreen_life/28) (and it makes sense)

  * [Some night photos](https://telegram.me/lightgreen_life/69)

  * App/site/project/etc ideas:
      [one](https://telegram.me/lightgreen_life/31),
      [two](https://telegram.me/lightgreen_life/25),
      [three](https://telegram.me/lightgreen_life/38),
      [four-five-six](https://telegram.me/lightgreen_life/64)

# Haskell

I'm [teaching a bunch of people Haskell](https://github.com/neongreen/haskell-ex) by giving them tasks to solve (and then we collectively discuss solutions). You can join too!

I've also written some libraries:

  * [microlens](@gh:aelve/microlens), a small lens-compatible library for lenses. Should be useful for library writers and stuff.

  * [ilist](@gh:aelve/ilist), a library with optimised index-related functions for lists – you don't need those often, but if you do, this library is probably the best place to get them.

I also started the [/r/haskelltil](http://reddit.com/r/haskelltil) subreddit (for Haskell tips and tricks and code samples and oddities and curiosities and idioms and interesting facts and everything that is short enough that you'd be ashamed to submit it to [/r/haskell](http://reddit.com/r/haskell) or write about it at your blog). It didn't catch on but some content there is still useful.

Finally, right now I'm busy working on [Aelve Guide](https://github.com/aelve/guide), a wiki-like guide to Haskell ecosystem and community. You can help.

## lens over tea

A series of articles (called “lens over tea”) about [lens](@hackage) and its implementation.

  * [part 1](/lens-over-tea-1) – lenses 101, traversals 101, a bit of implementation details (also lens operators, functor composition, `Const`, `Identity`, difference lists, monoids of endomorphisms under composition, default signatures for class methods, equality constraints, and irrefutable patterns).

  * [part 2](/lens-over-tea-2) – composition, laws, getters/actions/setters (also history of lenses, categories, the way to write lenses which would compose “normally”, some links to `Reader`/`Writer`/`State` learning materials, `Void`).

  * [part 3](/lens-over-tea-3) – folds (also `Foldable`, `Apply`, a nice trick for combining folds, and `Fold1`).

  * [part 4](/lens-over-tea-4) – isomorphisms, some profunctors, lens families (also `Forget`, `Tagged`, `Proxy`, a bit about pure profunctor lenses, existential types, algebra of types, and a cat video).

  * [part 5](/lens-over-tea-5) – prisms (also a recap of isomorphisms with diagrams, a bit about affine traversals, `Pointed`, and `coerce`).

  * [part 6](/lens-over-tea-6) – Template Haskell, aka “write your own `makeLenses`”

It's a work in progress; the future parts will mention:

  * indexed things
  * `Bazaar`/`Magma`/`Molten`/`Mafic`, traversal stuff like `partsOf`, `taking`
  * `fusing` and `confusing`
  * benchmarks (`view _1` vs `fst`? how much does `fusing` help? etc)
  * vertical composition, [link](http://stackoverflow.com/a/17529470/615030)
  * `ala`, `AlongsideLeft`
  * `upon`, [link](http://stackoverflow.com/q/17006679/615030)
  * pure profunctor lenses and traversals, [link 1](https://www.reddit.com/r/haskell/comments/1jeo0p/theres_a_massive_gap_between_the_average_and/cbe1ebv), [link 2](https://github.com/purescript-contrib/purescript-lens/issues/26), [link 3](http://lpaste.net/103359), [link 4](http://r6research.livejournal.com/27476.html), symmetric lenses (`(g a -> f b) -> g s -> f t`), [link](http://slbkbs.org/pr.hs)
  * `Control.Lens.Plated`, `Control.Lens.Level`, `Control.Lens.Zoom`

## Other stuff

  * [Aeson: the tutorial](/aeson)
  * [10 questions about Haskell: a shitpost](/haskell-10)
  * [Making a CTF task in Haskell](/haskell-ctf)
  * [Some common and annoying mistakes in Haddocks](/haddock-mistakes)

# Stuff that is sort of related to psychology

  * [Unconditional empathy](/empathy-consequentialism)
  * [The YX problem](/yx)

# Stuff that is sort of related to linguistics

  * [Why you don't want “irregardless” to be a word](/irregardless)
  * [Polysemy (i.e. overloading) in programming](/polysemy)

# Racket

Several years ago I've been learning [Racket](@w:Racket (programming language)) and making notes (which then became somewhat popular):

  * [Introduction](/learning-racket-1). First three days, no prior knowledge, getting a feel for the syntax... You get the idea.

  * [Macros, Macros and a Bit of Modules](/learning-racket-2). Another three days. Mostly macros, as you could've guessed.

<div class="gray">

  * [list of topics to cover](/racket-topics). Gray because it can't be finished almost by definition.

</div>

I'm not learning Racket anymore, but I might start again in the future.

# Stuff I did

  * [This site](/inside). Yes, it runs on a couple of shell scripts, but I thought I'd explain them anyway.

# Mindless compilations

  * [Flattrs of Popular Software Projects](/flattrs).

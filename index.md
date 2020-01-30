---
pagetitle: Artyom Kazak
---

# Read my blog

This is my blog: <https://blog.artyom.me>.

# If you want to hire me

The best way to hire me is by hiring [Monadfix](https://monadfix.com) services. I'm also open to other opportunities – I would love to try my hand at sales, marketing, and product development. Here is my [LinkedIn profile](https://www.linkedin.com/in/artyom-kazak-1a8737105/).

## lens over tea

A series of articles (called “lens over tea”) about [lens](https://hackage.haskell.org/package/lens) and its implementation.

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
  * [Making a CTF task in Haskell](/haskell-ctf)
  * [Some common and annoying mistakes in Haddocks](/haddock-mistakes)

# Stuff that is sort of related to psychology

  * [Unconditional empathy](/empathy-consequentialism)
  * [The YX problem](/yx)

# Stuff that is sort of related to linguistics

  * [Why you don't want “irregardless” to be a word](/irregardless)
  * [Polysemy (i.e. overloading) in programming](/polysemy)

# Racket

Several years ago I've been learning Racket and making notes (which then became somewhat popular):

  * [Introduction](/learning-racket-1). First three days, no prior knowledge, getting a feel for the syntax... You get the idea.

  * [Macros, Macros and a Bit of Modules](/learning-racket-2). Another three days. Mostly macros, as you could've guessed.

<div class="gray">

  * [list of topics to cover](/racket-topics). Gray because it can't be finished almost by definition.

</div>

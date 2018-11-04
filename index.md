---
pagetitle: artyom.me
---

# If you want to hire me

I'm not planning to leave Wire at the moment, but you can write me anyway so that if I do end up looking for another job, I would know that you exist.

Here's my [resume](/cv).

# Haskell

I've written some libraries, see <https://hackage.haskell.org/users/Artyom>.

I also started the [/r/haskelltil](http://reddit.com/r/haskelltil) subreddit (for Haskell tips and tricks and code samples and oddities and curiosities and idioms and interesting facts and everything that is short enough that you'd be ashamed to submit it to [/r/haskell](http://reddit.com/r/haskell) or write about it in a blog). It didn't catch on but some content there is still useful.

Right now I'm busy working on [Aelve Guide](https://github.com/aelve/guide), a wiki-like guide to Haskell ecosystem and community.

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

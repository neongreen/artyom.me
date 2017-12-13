% Artyom Kazak: CV

---
header-includes:
  - \usepackage[bottom]{footmisc}
...

NB. This CV is available [on my site][cv-web], [as plain HTML][cv-plain],
and [as PDF][cv-pdf].

[cv-web]: https://artyom.me/cv
[cv-plain]: https://artyom.me/cv-plain.html
[cv-pdf]: https://artyom.me/cv.pdf

## Summary

**Who I am:** a Haskeller with seven years of experience (working for two
years). I have pretty good knowledge of the ecosystem and tooling – right
now I'm writing a [book about Haskell][IH], and I'm also doing consulting
at [Dirt Cheap Haskell][].

[IH]: https://intermediatehaskell.com
[Dirt Cheap Haskell]: https://dirtcheaphaskell.io

**Where I work:** at [Serokell][] – I'm one of core developers
of [Cardano][], a 30 kLOC cryptocurrency with dozens of devs on the team.
See pretty [commit stats][]. Other than writing a substantial part of the
code and doing bug-hunting, I was involved in many architectural and design
decisions.

[Serokell]: https://serokell.io
[Cardano]: https://iohk.io/projects/cardano
[commit stats]: https://github.com/input-output-hk/cardano-sl/graphs/contributors

**Open-source work:** my Github is [\@neongreen][]. I also maintain a bunch
of libraries on Hackage (<https://hackage.haskell.org/user/Artyom>) – the
two bigger ones are [fmt](@hackage) and [microlens](@hackage).

[\@neongreen]: @gh:neongreen

**Contacts:** you can contact me here (in English or in Russian):

  * email: yom@artyom.me
  * Telegram: \@lightgreen
  * Skype: mayangreen

## Potential dealbreakers

I have dropped out of university. Depending on your hiring policies and your
country's visa policies, this might be a dealbreaker.

## Haskell

Other than my work for Serokell:

  * **Server-side:** for a client, I wrote a backend for a static website
    hosting (with Snap and SQLite), a CLI tool for interacting with it, and
    parts of the server and Liquid markup parser/renderer. I also wrote
    the backend for a personal project – a [Haskell wiki][Guide].

    [Guide]: https://guide.aelve.com/haskell

  * **Parsing:** I designed a small JSONPath-like language for extracting
    data from JSON and MessagePack logs and wrote a parser and an intepreter
    for it. Not opensourced :(

  * **REST APIs:** I have written a [bot][] for Telegram and a binding for
    Telegram API. (I started using servant only recently and haven't yet had
    a chance to use it in non-Serokell code.)

    [bot]: @gh:neongreen/untilbot

  * **Lenses:** I wrote a [series of articles][lens over tea] explaining in
    detail how `lens` is implemented and the theory behind it, and I also
    wrote [`microlens`](@hackage) (a minimalistic, well-documented lenses
    library).

    [lens over tea]: https://artyom.me/#lens-over-tea

  * **Template Haskell:** I know it pretty well and even (masochistically)
    enjoy writing it – for instance, I implemented a [version][makeLenses] of
    `makeLenses` for a blog post, and another example
    is [migration-generating code][migrate] that I wrote for `safecopy`.

    [makeLenses]: https://artyom.me/lens-over-tea-6#the-answer-4
    [migrate]: http://hackage.haskell.org/package/safecopy-migrate-0.1.0.0/docs/src/Data-SafeCopy-Migrate.html#changelog

  * **GUI:** have written several utilities using GTK.

  * **FFI:** have written [code][fake-type] for simulating keypresses on
    Linux. It binds to X11 in a non-trivial way.

    [fake-type]: https://github.com/aelve/fake-type/blob/master/lib/FakeType.hs

Additionally, I have written perhaps the most popular [Aeson tutorial][] and
I used to be teaching people Haskell at [Haskell Learning Group][HLG].

[Aeson tutorial]: https://artyom.me/aeson
[HLG]: https://github.com/haskell-learning-group/haskell-learning-group

## Oh, also algorithms

I've been actively participating in IOI/ACM-style programming competitions:

  * Belarusian National Olympiad (2012): 15th place, silver
  * All-Russian Team Olympiad (2011): 10th place, bronze
  * Moscow Open Olympiad in Programming (2012): silver

That was several years ago, but I can refresh my knowledge if needed.


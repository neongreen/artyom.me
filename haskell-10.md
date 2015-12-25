% 10 questions about Haskell: a shitpost

okay, so there was a thread with questions on r/haskell recently, and someone who sounds suspiciously like a /b/tard made [this shitpost](https://www.reddit.com/r/haskell/comments/3cfax3/new_haskellers_ask_anything/csuzebg) (which has been deleted now, but here I quote it in its entirety):

> 1) Best resource to learn category theory?
> 
> 2) What other math must be known to not look like a scrublord?
> 
> 3) How to hide haskell-induced autism from javatards et al?
> 
> 4) Emacs vs VIM for haskell?
> 
> 5) What areas are lacking in libraries in comparison to popular languages like Java? (For project ideas.)
> 
> 6) Can people generally survive on writing haskell code?
> 
> 7) How to subtly tell non-haskellers that you're better than them without looking like a dickwad?
> 
> 8) Top 3 (different) haskell books/resources?
> 
> 9) What are the most interesting Haskell projects being worked on right now?
> 
> 10) Who's ass is it most prudent to kiss in the haskell community?

of course, ne got downvoted into oblivion and people were responding like this:

> overall i think you will find your general attitude in this comment is not how this community operates. you should pay attention to the mores of a community before stomping in

I, for one, enjoyed that delightfully out-of-place shitpost (I need a healthy dose of 4chan in my life, really), upvoted it (which made no difference whatsoever), and started writing a response, but it got too long and I also got afraid that nobody would ever see it, so I decided to publish it here, because maybe – just maybe – I'm not the only person who needs 4chan in nir life

(I wouldn't ever dare submit it on r/haskell, but I do wonder what the comments would be if I did)

-----------------------------------------------------------------------------

> 1) Best resource to learn category theory?

dunno, I'm a scrublord actually

there's [this series of posts](http://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/), which I tried to read once and got lost after a couple of posts (again, keep in mind that I'm a scrublord and that I *absolutely* failed to understand linear algebra when I was in university and maybe this says something about my mathematical abilities but again maybe not)

sorry for failing to answer this one

> 2) What other math must be known to not look like a scrublord?

according to my observations on unsuspecting haskellers, if you know enough math to understand what ² in O(n²) means you'll be just fine unless you accidentally get into anything that requires, like, random numbers, or statistics, or similar kind of shit, but even then people won't care much 'cause everyone knows that [programmers don't know statistics](http://zedshaw.com/archive/programmers-need-to-learn-statistics-or-i-will-kill-them-all/)

> 3) How to hide haskell-induced autism from javatards et al?

replace the word “functor” by “container” or “producer” when you speak. also take care not to mention that “things that can be put together” are monoids.

don't look horrified when someone tells you that a global variable could be used in some place – if someone overuses them, point out specific *things caused by global variables* which are bad (like “it'll be hard to multithread this shit later, y'know”), but don't say that global variables are bad by themselves. (also can always simply say “your code looks fucking awful” without explaining why)

in languages that allow making new types, don't bother frantically making type synonyms for “name”, “email”, etc, especially if said languages also have good IDEs and docstrings – and when you make a new type anyway, write “I'm making this type so that nobody would accidentally mix it with \<some other type\> because it might lead to very bad things” in the comment (which you don't have to do in Haskell because everybody already knows it)

don't suddenly decide to use linked lists everywhere. in general, Haskell sometimes makes people forget that arrays and mutable structures *exist* – don't fall into this trap

> 4) Emacs vs VIM for haskell?

doesn't matter, but if you never used both and you choose vim I think at first you're going to spend much more time googling and annoying everyone with your “how do I set up vim + haskell” questions. so, probably just use emacs

incidentally I use emacs. have I mentioned that I'm very objective and unbiased?

> 5) What areas are lacking in libraries in comparison to popular languages like Java? (For project ideas.)

system stuff: for god's sake, we don't even have a library to copy a picture into clipboard yet, and out of our 2 libraries which let you download a file easily, one doesn't compile and both have useless dependencies (like an rss feeds parser) which are incompatible with some other popular libraries

libraries for working with different file/data formats are either missing or *all written in completely different styles* so you have to spend lots of time studying each library before you can use it (that's the ugly side of “oh it's so easy to write DSLs in Haskell”)

numerical stuff is worse than Python's and the curse of different styles applies here as well

all existing web frameworks may not be awful in terms of safety and whatever but they're awful in terms of beginner-friendliness or maybe they just have shitty tutorials. I really don't know. I spent 3 hours recently just trying to make a web page that would contain a single text box and a button to reverse text in that textbox, that was some fucking frustrating experience

I'll just stop here

> 6) Can people generally survive on writing haskell code?

not generally, no. unless they are lucky or not mediocre (I am lucky, for instance). you might get lucky too if there's a Haskell job near you and they don't let people work remotely, or if you're the only haskeller in your country, or something like that, but generally there's a lot of people who are good at Haskell and are desperate to escape the world of javatards

I think some guy recently posted a Haskell job on reddit and then said that out of 40 candidates ne could pick like *anyone* and they were all good and oh how much better it is than ruby where out of 200 candidates 150 would be shitty. yeah, it's awesome, but for you it means that at any moment there are 40 good haskellers around who try to snatch all Haskell jobs (and I don't think you're going to beat them)

> 7) How to subtly tell non-haskellers that you're better than them without looking like a dickwad?

I'm going to pretend that you didn't mean that you want to offend someone, only that you want to convey the following piece of information: “I'm using Haskell and this implies I'm better than you” (if you just want to convey “I'm better than you”, then your question is irrelevant to Haskell and why the hell are you asking it on r/haskell?). since this statement is obviously false, I guess you'd have to resort to various psychological tricks to convince them

first of all, you could create an impression that Haskell is super hard to use, then learn it in 6 months or so, then tell everyone you learned it in a week

shorten code with [pointfree.io](http://pointfree.io/), put shortened code into your sources, don't tell people you haven't written it by yourself, pretend to understand exactly what's going on in `ap (ap . (liftM2 ap .) . liftM2 (.) (.) . (.) . (.)) ((. flip) . (.) . (.))`, keep saying “but it's obvious”

avoid other people who know Haskell, but when you absolutely can't, stick to what you already know but claim that it was easy to understand (“wait, but what's so hard about currying/monads/lenses? Sorry, I just genuinely don't understand what could be hard about it... Can you explain?”)

say that writing anything in Haskell is 10 times easier and faster, that you get code that is as fast as C, that yesterday you added `par` in one place and immediately got perfect 4× speedup on your quad-core CPU, and that the compiler prevents all possible bugs in existence, so that everybody else would feel stupid and inferior for somehow failing to use this *objectively* superior language

mention a couple of times how Haskell is an awesome first language and that the only reason people have troubles with it is that their brains have been messed up by the imperative paradigm (but it's not their fault, of course)

> 8) Top 3 (different) haskell books/resources?

1. bitemyapp's [guide](https://github.com/bitemyapp/learnhaskell) is probably the ultimate meta-resource, and ne's also writing a [book](http://haskellbook.com/) which is going to be the Best Pedagogically Sound Book Ever but I haven't read it yet so I won't be recommending it (bitemyapp also likes to complain about LYAH and RWH but RWH seemed pretty fine to me, so I don't know)

2. [Haskell Fast & Hard](https://www.fpcomplete.com/school/starting-with-haskell/haskell-fast-hard) doesn't look bad

3. this is probably cheating but I don't care: if you're shameless enough, you can just go to the #haskell irc channel and say “now someone teach me haskell please” and they would do just that (unless someone suspects that you want to get your homework done – but if you sound like you genuinely care about Haskell itself, they would explain anything to you)

> 9) What are the most interesting Haskell projects being worked on right now?

there's [Backpack][] which is going to improve Haskell's module system greatly

there are projects like [Liquid Haskell][] which are about marrying Haskell with SMT solvers which would finally let you use the compiler to prove things about your programs – like you ought to, it's 2015 after all – without learning all that agda-schmagda nonsense or sinking into the depths of olegery (named after Oleg Kiselyov) or what Conor McBride lovingly called [Hasochism][] (but since some people are also [trying][DH] to add dependent types to Haskell without resorting to SMT solvers, I've no idea what would win. or maybe they'll just combine both things. or maybe they even solve different problems. whatever)

I think we're also going to see resurrected HaRe (the refactoring thingy for Haskell) at some point in the future and it might or might not be exciting I haven't really seen what it can do

if you're talking about projects written in Haskell but not about Haskell... uh, no idea. I skimmed thru HCAR (from May 2015) and nothing there seemed exciting to me, and there weren't any announces on r/haskell either

I guess I'd know if I spent more time on #haskell or attending hackathons and other social gatherings – which I don't

[Backpack]: https://ghc.haskell.org/trac/ghc/wiki/Backpack
[Liquid Haskell]: http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/about/
[Hasochism]: https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf
[DH]: https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell

> 10) Who's ass is it most prudent to kiss in the haskell community?

that's a sorta vague question and I wish I knew how to interpret “prudent” here... okay, let's see

there's Gabriel Gonzalez (aka Tekmo), who is known for proving nir code in nir head / using a piece of paper (which sounds cool and “nice, this guy is actually proving nir code before releasing it”) as well as writing [pipes](@hackage) and being the underdog of the pipes–conduit competition. [conduit](@hackage) is a similar library written by Michael Snoyman, who is

  * not as young
  * not as humble
  * isn't widely known to prove code in nir head
  * is automatically vaguely evil because ne works for dirty capitalists, i.e. [FPComplete](https://fpcomplete.com) (while Tekmo is a Twitter engineer but doesn't advertise it much so it doesn't count)
  * is additionally evil for caring about “production” too much which results in messy enterprisey web frameworks like Yesod (while everything Tekmo writes is pure, proven, self-contained, non-messy, precise, and mathematical – which we all love, or at least I do)

we've also got Edward Kmett who maintains [more libraries than everyone else](http://hackage.haskell.org/user/EdwardKmett) and also covers *too darn much* with those libraries:

  * [an alternative to Parsec](@hackage:trifecta)
  * [an in-memory database](@hackage:tables)
  * [the most complete OpenGL binding](@hackage:gl) (also, the biggest package on Hackage)
  * [something which is probably better than all other SMT solver bindings](@hackage:ersatz)
  * [a generalisation of radix sort which is going to result in an O(n) sort for most things people usually sort](@hackage:discrimination)
  * [an automatic differentiation library](@hackage:ad) (it's some math thing I don't know anything about, but people like it)
  * [an alternative to pipes/conduit](@hackage:machines) which nobody knows how to use but everyone secretly hopes is going to blow pipes/conduit out of water
  * [a linear algebra package](@hackage:linear)

ne's like Leonardo da Vinci a bit but without that huge beard

(also there's a whole irc channel – #haskell-lens – devoted to things Edward does or plays with)

finally, everyone used to mention Simon Peyton Jones and Oleg Kiselyov a lot, but now they don't do it as much for some reason

just in case: this is *not* a list of best haskellers or whatever. this isn't even a fucking list at all, it's got just 2 people, c'mon (also, if you want a longer list, look at [Haskell Cast](http://haskellcast.com/))

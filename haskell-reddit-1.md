% Comments on random stories on r/haskell, part 1

---
series:
  top: comments on r/haskell
  toplink: /#comments-on-random-stories-on-r-haskell
---

January has ended, here we go

me, dumping my thoughts on you

you, not reading this post because it offers little value

## 56 points: [Dropbox Hack Week: GraphQL Server in Haskell](https://www.reddit.com/r/haskell/comments/43uyld/dropbox_hack_week_graphql_server_in_haskell/)

> Last week was hack week at Dropbox.

Unrelated, but: it's always been surprising to me that “small companies like Dropbox that *really* only have one product, c'mon” have hack weeks – but the fact that a “small company like Dropbox that *really* only has one product, c'mon” can have a valuation of 10 billion dollars isn't as surprising. Okay, back to the post.

> A common performance problem in the implementation of web services is serial IO. Let's say you have a service that returns the names of all of your friends.

Aha, now I know what Dropbox is going to use all that money for, they're making a service that returns the names of all of your friends, by which I mean Facebook, except that it doesn't have all *other* features of Facebook – just names of your friends, in a JSON record, one API call away. Neat.

(Plot twist: the API call takes no parameters, and works even if you aren't registered.)

> The problem is that each user info lookup for my friends occurs sequentially. Even with TCP connection pooling, that's still a packet round-trip in the datacenter.

Okay, right, I always solve this problem by making another API call that goes like `[User] -> IO [UserInfo]` instead of the usual `User -> IO UserInfo`, but there's gotta be a better solution.

> One solution is to always batch IO. Every function takes a list and returns a list. This can be made to work (indeed, I've achieved excellent performance by carefully, manually, batching) but doesn't compose as your services scale.

(Hm-m, could it be that I don't have a job because I never had to write anything that would have to scale?)

> At IMVU, we went all-in on REST, building a rather substantial framework to make it easy to create REST services. We anticipated the fact that REST services tend to require many round-trips from clients, so we built a response denormalization system, where the service can anticipate the other services a client will need to hit and include their responses too.

Translation: instead of returning `User`, return something like this:

~~~ haskell
data QueryResult a = QueryResult {
  -- | What you explicitly queried for
  result :: a,
  -- | Just in case you need it, info about all users mentioned
  -- in the thing you queried
  mentionedUsers :: Map User UserInfo,
  -- | etc etc etc
  mentionedStuff :: Stuff,
  -- | do you want fries with that
  fries :: [Fry] }
~~~

> This sounded great at first, and in fact was mostly an improvement from the prior state of affairs. But, at scale, REST has some challenging performance problems.

Translation: sometimes it still doesn't return everything the user wanted, and it's bad; and when it does return everything the user wanted, it also returns a shitload of things the user doesn't want.

Not an ideal solution.

> This is where [GraphQL](https://facebook.github.io/graphql/) comes in. In GraphQL, the *client* specifies the data that it wants. There is only one request, even if the data is deeply nested, and the server only has to fetch exactly what's needed.

Ooh, so apparently Facebook had this problem (because Facebook has had *any* problem that anybody ever had), and they invented a query language specifically for the task.

/imagines a university course called “Just Learn To Use What Microsoft, Google, and Facebook Have Already Created”

> There are [GraphQL implementations](https://github.com/chentsulin/awesome-graphql) for many languages but many of them don't solve the serial IO problem I described to start this post. In fact, a naive GraphQL implementation might issue IO *per field* of each object requested.
>
> For hack week, I wanted to explore the design of a GraphQL server that issued all of its backend IO in optimally concurrent batches.

Okay, awesome, this is going to be a post about *actual code*, let's get straight to it.

> Facebook open sourced their excellent [Haxl](https://github.com/facebook/Haxl) library which converts code written with serial IO into efficient batched requests. [...] My prototype GraphQL resolvers are surprisingly naive, specified with [sequential code](https://github.com/dropbox/datagraph/blob/429957ebdbf1ef3553dabb2a8f7b16056ef4ac41/src/Main.hs#L87). Haxl automatically batches up the requests and hands them to the [DataSource for execution](https://github.com/dropbox/datagraph/blob/429957ebdbf1ef3553dabb2a8f7b16056ef4ac41/src/StarWarsDataSource.hs#L92).
>
> In addition, there is nothing clever about the GraphQL [request handler](https://github.com/dropbox/datagraph/blob/429957ebdbf1ef3553dabb2a8f7b16056ef4ac41/src/Main.hs#L137) or [graph traversal and filtering](https://github.com/dropbox/datagraph/blob/429957ebdbf1ef3553dabb2a8f7b16056ef4ac41/src/Main.hs#L46) — all cleverness is handled by Haxl.
>
> At this point, you might be thinking “So was anything challenging about this project?” On the data fetch side, no, not really. :)

...yeah, I see.

In case you're wondering, the rest of the post is “The performance is pretty good: on some AWS instances, I measured about 3500 queries per second per machine and a query latency averaging 3 ms.” plus several paragraphs about how Haskell is better than Go and Python for this. And here's the [source](https://github.com/dropbox/datagraph) of the whole thing if you want to read it, which you probably don't.

Meh. I'm glad that I learned about GraphQL, and I'm glad that now an association between “Haxl” and “batched IO requests” exists in my mind, but that's about it.

On to the next post.

## 36 points: [Haskell in industry](https://www.reddit.com/r/haskell/comments/43w30w/haskell_in_industry/)

Okay, another post from somebody who uses Haskell at company X, except that this time it's sort of official. (And thus much more boring.) People at [Wagon](https://www.wagonhq.com/) have been trying to write about Haskell for a while, and in my mind they neatly fit in the cluster of companies who use Haskell but *aren't in my tribe* – they don't sound like Haskell programmers, they sound like people who observe Haskell from somewhere high above.

(Which is fine and everything but it doesn't mean I'm not going to complain – you have your goals[^their-goals], I have my goals[^my-goals].)

[^their-goals]: Such as a) advertising their company, b) advertising Haskell to other software developers who aren't on r/haskell, and c) getting reputation points on [Quora](https://www.quora.com/Is-Haskell-suitable-for-commercial-software-development).

[^my-goals]: Such as a) providing entertainment and -that nice feeling when someone relates to you- to some people, and b) attracting visitors to my blog.

> Intermediate Haskellers quickly pick up advanced concepts on the job: parsers, monad transformers, higher-order types, profiling and optimization, etc.

Wait wait wait if using monad transformers is an advanced concept then *why the fuck am I still sitting without a job?*

/sends a death glare in the general direction of San Francisco

> Some libraries have become de-facto standards that are stable and performant

Would be weird if Haskell didn't have a single library that is both stable and performant, wouldn't it.

> We do sometimes find gaps in Haskell's available libraries.

Would be equally weird if there were no gaps in Haskell's available libraries, wouldn't it.

> And like every open-source ecosystem, there is some bad code out there. We rely on community recommendations and a sense of dependency hygiene to stay productive.

On one hand, true, on another hand, that's what I would've written if I was desperate to write *anything* about Haskell.

-----------------------------------------------------------------------------

This is the only important thing in the post, and it's easy to miss if you have already switched to the skimming mode. The important thing is that they explicitly say that memory leaks and performance *aren't* a problem – contrary to what everybody else says:

> Fast enough under most circumstances. [...] Easy to optimize when needed. [...] We almost never have to address performance in our Haskell code. [...] There's a lot of talk about memory leaks caused by Haskell's lazy evaluation model. We run into this very infrequently in practice, and in those rare situations GHC's profiler has led us to solutions quickly.

A hundred more quotes like this, and you'll be able to use Haskell at work. Probably.

(As an aside, in an ideal world we'd have a page called “Haskell testimonials” that would collect quotes like this and sort them by topic (performance/space leaks/hiring/etc), trustworthiness, and year of publication, but we're not in an ideal world.)

## 1 point: [How do I get current version of Haskell?](https://www.reddit.com/r/haskell/comments/43wzxn/how_do_i_get_current_version_of_haskell/)

Here's the post in its entirety:

> I'm running Debian 8. I used this simple instruction from Haskell's [site](https://www.haskell.org/platform/linux.html#linux-debian). When I run ghci, it says it's version 7.6.3 and I see it's from 2013. Or maybe it is a stable version? Although some guy said that it's not and compiler may give me distracting errors. I just wanted to simply get current version of Haskell and start learning it because I read so much good opinions about learning functional programming. :-(

Poor confused Debian user who doesn't know that all of Debian's packages are perpetually outdated :-(

The linked instruction was as follows:

> Good news! Haskell Platform is already available in your distribution's package repository.
>
> Simply run,
>
>     $ sudo apt-get install haskell-platform

Do you think it should warn about the package being outdated? For instance, would you feel comfortable making a pull request adding something like this to the page?

> Note that this package is often severely outdated; you're advised to use the **Generic** installation method instead.

You're probably fine with this particular text being on the page – if you were the maintainer of Haskell.org, you might add it there without thinking much. But do you think that if you weren't a maintainer and made a pull request, it would be looked at kindly?[^pr] Do you expect to be told that the wording should be different, that we ought to add similar warnings to other distributions (for consistency) and this would require researching whether Haskell Platform is outdated or not for other distributions, and then we'd have to check them again every month or so because oh no how bad it would be if we said that Gentoo has outdated Haskell Platform but it doesn't anymore, and anyway, why should we do all this when people who use Debian ought to know that Debian chooses stability over bleeding edge? It's not a problem with Haskell, it's a problem with Debian, right?

[^pr]: I made a pull request with that exact wording. It [was merged](https://github.com/haskell/haskell-platform/pull/232) without questions, but then another maintainer thought that making it softer – “If you want the latest, you are advised...” – would be better. So, “if you were the maintainer, you might add it without thinking much” was true, but “...or you might not” is equally true – which is, by the way, beside the point, because having any variant of the warning is better than having no warning at all.

It's good if all these concerns sound slightly ridiculous to you, but if they don't, then it's not a problem with Debian or Haskell.org maintainers, it's a problem with you – your fear of rejection, and your belief that others have higher standards than you (and that in *some sense* they're right to have higher standards – see all the justifications above, ones that you might even start defending in order to protect yourself from cognitive dissonance).[^projecting]

[^projecting]: Am I projecting my fears on you? Yep. Are those fears stupid nonetheless? Sure they are.

## 42 points: ["rebase", a more progressive alternative to the "base" package](https://www.reddit.com/r/haskell/comments/4383na/rebase_a_more_progressive_alternative_to_the_base/)

Ah, here we go again, Russians and their “alternatives”. An alternative to the base package? Russian. An [alternative to Prelude](@hackage:base-prelude)? Russian. An [alternative to Parsec](@hackage:megaparsec)? Russian. An [alternative to lens](@hackage:microlens)? Russian. An [alternative to mtl](@hackage:ether)? Russian. An [alternative Haddock theme](https://www.reddit.com/r/haskell/comments/4141ge/attempt_at_a_modern_haddock_theme/)? Most likely Russian.

Mysterious, isn't it? Nope. I'm Russian too, so I naturally pay special attention to Russian names (and typical grammatical mistakes) in library announcements. Never trust a stranger on the internet not to have some bias ne forgot to tell you about.

Anyway, back to [rebase](@hackage). What does it do?

It gives you a module, `Rebase.Prelude`, that exports most things from base, as well as `Data.Time`, `Control.DeepSeq`, `Data.Functor.Contravariant`, and lots of types (like `Text`, `ByteString`, `Map`, and `Set`). Additionally it provides a bunch of creatively named modules (`Rebase.Data.Set`, etc) that let you export things from `Data.Set` without having to add [containers](@hackage) to the dependencies section in your .cabal file.

First it seems wrong: what's the point of such obviously useful things as trimming unused imports, [packunused](@hackage), etc if you're going to circumvent all that by adding a heap of unused dependencies to your package just because you were too lazy to add only the things you need?

Then it seems right: are you really conveying any useful information to anybody by specifying in your .cabal file that you use text and vector but not bytestring? Adding a Yesod dependency shows that you're using Yesod and not Snap; adding a lens dependency shows that your package is reasonably modern and will also take 20m to compile; but text, vector, bytestring, and containers dependencies provide zero information.

Finally it seems weird: why don't we have this as “package collections” or something? Why do you have to create a package with lots of modules and reexports and so on when all you want is a list of package names (and perhaps some versions)? Why aren't import lists first-class?

And then you realise that nobody cares, and even immensely convenient [base-prelude](@hackage) (that consists of a single module reexporting everything in base) only has [6 dependencies](http://packdeps.haskellers.com/reverse/base-prelude) on Hackage (not counting those written by its author), and that's when you can safely decide that you won't use [rebase](@hackage) either, close the tab, and keep complaining mentally about how dependencies are a pain.

-----------------------------------------------------------------------------

Have I managed to shame you into considering to try it? Well, here's another thing to sway you back: if you do try it, you likely won't win much (since out of all its dependencies you probably only use 5 – containers, text, time, unordered-containers, and vector), *and* whenever you stumble upon any module not exported by rebase you'll have to add the dependency back or send a pull request to the author. By modules not exported by rebase I mean the following:

  * `Data.Text.Encoding` (needed if you want to convert `Text` to/from `ByteString`)
  * `Data.ByteString.Char8` (no idea why you would need it but [lots of people do](https://github.com/search?l=haskell&q=Data.ByteString.Char8&type=Code&utf8=%E2%9C%93))
  * `Data.Map.Lazy`
  * `Data.Tree`
  * `Data.Vector.Unboxed`, `Data.Vector.Mutable`, etc
  * `Data.Time.Calendar.OrdinalDate` (which you need if you want something as simple as finding out what day of week is today)
  * `Data.Time.Clock.POSIX` (which you need if you have to deal with timestamps of any kind, including ones commonly encountered in JSON)

And, you also won't get filepath, or random, or directory, or aeson. Oh, right, but that wasn't the goal of the package!

> The policy behind the package is only to reexport the non-ambiguous and non-controversial APIs, which the community has obviously settled on.

*Then how is it going to make your life better if you end up having to tolerate import lists anyway?* As a reference for what packages are controversial and what aren't? Or as a way not to repeat the following 4 lines in your code over and over?

~~~ haskell
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.ByteString (ByteString)
~~~

Just make a goddamn `MyPrelude` module and copy-paste it into each new project, it's going to be more useful anyway. Make a .cabal file template for yourself, too.

## 35 points: [Neil Mitchell's Haskell Blog: New HLint version and API features](https://www.reddit.com/r/haskell/comments/43ruij/neil_mitchells_haskell_blog_new_hlint_version_and/)

> Summary: In the new version of HLint, errors are warnings and warnings are suggestions. I plan to remove the old API very soon.

OH MY GOD

2 THINGS HAVE BEEN RENAMED

-----------------------------------------------------------------------------

No, but really. Why 35 upvotes? Is it because people are glad this particular thing has been fixed? Is it because lots of people use HLint and are glad that it's being worked on, that it's not dead?

Unlikely. Nobody uses Leksah, yet announcements of minor versions of Leksah have been getting 80+ upvotes. Understand the upvotes, and you'll understand people's *feelings;* understand their feelings, and you'll ~~be able to manipulate them~~ stop being sad about your posts getting few upvotes.^[Or perhaps stop being happy about your posts getting lots of upvotes, but I bet you'll be happy anyway.]

What's special about HLint and Leksah? What's special about GHC? Why does a [post about fixing a performance bug in Aeson](https://www.reddit.com/r/haskell/comments/35udfy/fixing_an_aeson_performance_bug_sometimes_the_old/) get 100 upvotes? It's not because it's interesting from the technical point of view: the gist of the post is “Aeson had a bug where parsing a megabyte of consecutive backslashes was taking over a second, that was because of thunks, I fixed it by switching from a `ByteString` builder to writing bytes into a buffer”. It has hardly made anyone's life better, too – not many people parse megabytes of consecutive backslashes, and even Bryan has no idea why Facebook has data with megabytes of consecutive backslashes in it.

Wait, what does Facebook have to do with it? Oh, right, in the 1st half of the post Bryan casually talks about Facebook's spam filter that was rewritten in Haskell recently.

It's not surprising ne'd get so many upvotes, then – except that here's a [post](https://www.reddit.com/r/haskell/comments/1us600/new_year_new_library_releases_textattoparsecaeson/) that merely announces speed improvements in aeson and text (no Facebook this time) and still gets the same number of upvotes. And don't forget that it's literally enough to announce a minor release, point out that 2 renamings happened, and get 35 upvotes, while writing a [library for making SQL queries](https://www.reddit.com/r/haskell/comments/42qdwx/ann_beam_0320_powerful_typesafe_sql_database/) or a [better library for generics](https://www.reddit.com/r/haskell/comments/4272ai/ann_genericseot02/) isn't.

-----------------------------------------------------------------------------

Well, I've got a theory. It goes as follows: people love it when you tell them that their lives have just gotten better and they didn't have to do anything for that.

Look at this post: [80% of Haskell packages that use floating point expressions contain numerical instabilities. The Herbie GHC Plugin automatically rewrites these expressions into a numerically stable form.](https://www.reddit.com/r/haskell/comments/3lxz27/80_of_haskell_packages_that_use_floating_point/). I don't know whether many people are going to use it – I think not – but the post itself is *perfect*. How many Haskell packages use floating point? Probably a lot, right? And this plugin can make 80% of them better? And since it “automatically rewrites”, doesn't it mean that we can pretty much say that all those packages have already become better? Whoa.

If you bothered to read the post, tho, you'd find out that

> 48 of the 1351 packages (3.5%) use floating point computations internally. Of these, 40 packages (83%) contain expressions whose numerical stability is improved with the Herbie plugin.

40 packages suddenly doesn't seem like a lot, but admit it, you're still hoping that those 40 packages are *crucial*, that lots of other packages depend on those 40 packages, that by improving them everything else will be improved too. Oh, and you expect Herbie to become a part of GHC one day – maybe not this year or the next year, but it's still possible now and wasn't possible before. The path to a better future has been cleared.

Or look at this post: [I think I've nailed it! I've solved the records problem!](https://www.reddit.com/r/haskell/comments/2svayz/i_think_ive_nailed_it_ive_solved_the_records/). It's the most upvoted post on the whole of r/haskell. Everyone knows about the records problem. Everyone is frustrated about it not being solved yet, even tho many people aren't even affected by it much. The comments go like this:

> Is no one on reddit today? This is literally the most exciting thing that's ever happened on /r/haskell. Nikita has secured his place in heaven. Why aren't we all high-fiving right now?

And then... nothing happens. The library gets discussed a lot, but not used ([at least not in public][record github]). A few months later the `OverloadedRecordFields` extension [gets revived][ORF]. We'll have records sort of solved in GHC 8.2 – and perhaps it's partly thanks to [record](@hackage) – but it's still not true that people upvote useful things. [record](@hackage) was never useful, otherwise it'd be, y'know. Used.

[record github]: https://github.com/search?l=haskell&p=1&q=%22import+record%22&ref=searchresults&type=Code&utf8=%E2%9C%93

[ORF]: http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/

People upvote things that fuel their hopes[^haskell-mode], be it “Haskell is used at Dropbox”, or “Facebook will finally make Haskell a popular language”, or “all libraries will become 2× faster”, or “this pesky problem with records that everyone complains about will be finally solved”, or “latest GHC will be awesome”, or, in case of HLint, “this magic tool got even better at improving my code, now my code will be good”. Yeah, if you end up running it on your code even once.

[^haskell-mode]: Surprisingly (to me), posts about [improvements made to haskell-mode](https://github.com/haskell/haskell-mode/wiki/Month-in-Haskell-Mode-July-2015) are an exception – they never are upvoted much. On the other hand, when they actually [announce a new release](https://www.reddit.com/r/haskell/comments/1hd21v/announce_haskellmode_1307_survey/) it *does* get upvoted much. All in all, either my hypothesis is shit or it's merely slightly more complicated than it looks.

## 40 points: [A decompiler for GHC-compiled Haskell](https://www.reddit.com/r/haskell/comments/42w0vf/a_decompiler_for_ghccompiled_haskell/)

Ahh, I remember how I once wrote a [crackme](@w) in Haskell and was sure nobody would ever be able to crack it. (In reality, of course, nobody cared.) Anyway, it's cool that what seemed impossible several years ago is pretty easy now. Reading thru a [description](http://sctf.ehsandev.com/reversing/lambda1.html) of decompiling a small Haskell program can also be interesting if you can read assembly and recognise the patterns, but for me it's black magic so I just skimmed it.

## 0 points: [We built our app and website's entire backend in Haskell. It's a spectacular and weird journey, and the results are so fast!](https://www.reddit.com/r/haskell/comments/43ubt4/we_built_our_app_and_websites_entire_backend_in/)

Here's the site in question: <https://willchill.co/>. As of now, the landing page contains:

  * a drawing of a gray fox
  * “download on the App Store” and “get it on Google Play” buttons

That's all. In the footer you can find some “About”/“FAQ”/etc links. All hail minimalism. In case you're wondering (are you?), the actual purpose of the app can be gleamed from this description:

> WillChill is a fun app that simplifies your social schedule. Enter your free time each week, pick the friends you want to hang out with, and we'll take care of finding the best time for you to meet up.

And when called out as bait-and-switch (since the site itself never mentions Haskell), the response was as follows:

> I swear (my hand on your choice of thick book within my arm's reach) we wouldn't intentionally be so underhanded!
>
> We've been working on this for something like six months, and we're eager to show off. So eager that there was a great flash of light and we momentarily forgot how to reddit properly!
>
> We'll get a substantial post together as soon as we can, and hopefully provide useful information.
>
> We, the WillChill team, are thankful for your patience.

“there was a great flash of light and we momentarily forgot how to reddit properly”. [claps slowly]

I'm not sure it was a dumb attempt to advertise their app – “we're eager to show off” is a plausible explanation too. When people want to show off they do stupid things. The question is: why did they think it had a chance?

And the answer is: because it did. According to Reddit, 45% of votes were upvotes.[^fuzzing] By itself this doesn't give us any information about how many votes does the post have overall; however, the top comment – the one saying “The site you're linking to doesn't appear to contain any information about your experience building the Haskell backend.” – has 38 upvotes, so it's unlikely that the original post only had 1 or 2 upvotes and 3 or 4 downvotes. Overall, a substantial number of people voted on the post, and *45% upvoted it*.

[^fuzzing]: I'm not sure whether “45% upvoted” even means anything for posts with negative score – Reddit implements vote fuzzing – but right now [this post](https://www.reddit.com/r/programming/comments/42zwb6/6_hot_programming_languages_to_add_to_your_tool/) is “6% upvoted”, which counts as evidence that not all heavily downvoted posts are “45% upvoted” and the number might be at least somewhat accurate.

Does it suggest that people are fine with utterly contentless posts as long as they give them hope (the “Haskell is used by people out there to build actual things!” brand of hope, this time)? I'm not sure; perhaps a small percentage of people upvote all posts that mention Haskell and “production” in the same sentence, but the rest don't care. However, what I know is that after having spent some time on r/haskell I have a definite impression that people are desperate for any success stories. You want proofs? Here you go.

[Haskell in production for Norwegian government](https://www.reddit.com/r/haskell/comments/3mm3uh/haskell_in_production_for_norwegian_government/), 105 points. Someone literally wrote a tool for generating barcodes and another tool for scanning barcodes in Haskell, called it “Haskell in production for Norwegian government”, and it was enough to amaze people.

[Haskell-based Bicycle Parking Guidance System in Utrecht](https://www.reddit.com/r/haskell/comments/3959r0/haskellbased_bicycle_parking_guidance_system_in/), 93 points. The only sentence in the linked story that mentions Haskell is:

> The computer programming language used to operate this system is Haskell.

Yeah.

-----------------------------------------------------------------------------

Okay, you can't learn anything from the posts, so what? Isn't it natural to be glad when somebody writes something complicated and *important* in Haskell and it gets used in real world by a government or by a city or whatever?

It is, but it only proves my point. Do you think C++ coders need validation from Norwegian government? Can you imagine “Java-based Bicycle Parking Guidance System in Utrecht” reaching the top of r/java?

Actually, let's look at r/java. (Yeah, it's unrelated to Haskell, but whatever, it's still interesting.) Java is successful; thus, Java programmers can't be tickled by success stories. They are insecure about something else. Could it be..?

...bingo, it's -being perceived as the most bureaucratic/enterprisey language in the history of programming-. Doesn't matter if it isn't so, Hacker News thinks it is. Hence the 20th most popular post on r/java – after some funny jokes and complaints about Oracle – is [the author of Minecraft saying Java doesn't suck](https://www.reddit.com/r/java/comments/34aemf/tone_it_down_a_notch/). YAY WE'VE BEEN VALIDATED GUYS LET'S CELEBRATE.

Don't be fooled by the [top comment](https://www.reddit.com/r/java/comments/34aemf/tone_it_down_a_notch/cqszmb7) starting with “Minecraft does not perform well, which is usually the criticism it receives.” – it's not a voice of reason/opposition saying “well Java doesn't suck but Minecraft is still kinda slow”, it's a preemptive response to people who think Java sucks. “I admit C++ is faster than Java. I also admit Java may seem boring. But it doesn't matter that Java isn't fast, because on the other hand it can be coded quickly! Java seems boring merely because it's “decent in nearly every category you'd rate a language on”!” – and further down the thread you see people denying even the “Java is slower” admission by saying that Minecraft is slow because its author “doesn't know how to optimize 3d graphics”.

Oh, and here's another one: [Why Java? Tales from a Python Convert](https://www.reddit.com/r/java/comments/3vzf1w/why_java_tales_from_a_python_convert/). Python is a really nice language to write in, “executable pseudocode”, and somebody converted from it to Java? That must feel better than an atheist converting into Christianity (for Christians). And again, the top comment betrays the insecurity:

> A lot of people seem to think that Java (pre-8) sucks, but I find it rather nice. Sure, it's a bit verbose (though really not as much as people make it out to be), but at least you always know what you're looking at.

## 164 points: [Explicit is better than implicit (Gabriel Gonzalez's blog)](https://www.reddit.com/r/haskell/comments/3p8zwh/explicit_is_better_than_implicit_gabriel/)

This one is from 3 months ago, but I have something to say on it anyway. Such as “I disagree”.

Not with the post, tho. The post isn't about explicit vs implicit, the post merely promotes a nice trick where you can use lenses to make it explicit that you're acting on `Right` or `Left` or 1st element of a tuple or 2nd element of a tuple or whatever. I guess the *need* for this post arose from the asymmetry that many Haskell types have – for instance, it's really easy to apply a function to the `Right` branch of `Either` without any lenses (you just use `fmap`), but there's no corresponding function to modify the `Left` branch of `Either`. (Same with tuples – you can use `fmap` to act on the 2nd element but not on the 1st.)

I totally agree that if you have, I don't know, a list of boys' names and girls' names, and you use `Either`, and you want to do something with only the girls' names, you should use `over (each . _Right) f` and not `map (fmap f)`.

What I disagree with is the title – i.e. the “implicit is bad” attitude. Exhibit A: the [Don't use Default](http://phaazon.blogspot.fr/2015/07/dont-use-default.html) post. Exhibit B: [this comment](https://www.reddit.com/r/haskell/comments/3mhbuq/so_i_was_working_on_putting_some_math_im_learning/cvg85fs). Exhibit C— darn, I can't find a good exhibit C, which means that the problem isn't as wide-spread as I imagined it to be. But I still feel that people don't appreciate ad-hoc overloading enough, so I'm going to argue *for* it anyway by linking to [this post](/polysemy) I wrote just for the occasion.

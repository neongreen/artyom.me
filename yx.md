% The YX problem

---
unfinished: true
---

But first, the XY problem.

[The XY problem](http://xyproblem.info/) is well known on forums, Q&A sites, and chatrooms. Somebody asks “how to do Y?”; in reality ne wants to do X, but ne's asking about Y because ne thinks X can be solved by doing Y. But Y is a *bad* solution for X, so if you just tell the person how to do Y, it'll be wrong. Here's an example: you're writing a program and you need to know some file's extension, but instead of asking “what function returns file's extension?” you ask “how to get last 3 characters of a string?” (because you wrongly think that all extensions have 3 characters). Another example: you want a small yellow fluffy chicken which is adorable and yellow and says pip-pip-pip, but instead of just buying a chicken you start asking your friends whether they know where to get a cheap incubator.

The XY problem is a problem because a) it wastes everyone's time, and b) it prevents the true problem from being solved. Right?

Well, right, but sometimes the cure is worse than the disease.

-----------------------------------------------------------------------------

Here's how a technical person might react when somebody in nir field of view attempts to use mouse to copy a piece of text: “Wait, how come you don't know that Ctrl-C copies things?! Okay, okay, okay, stop doing whatever you were doing right now. You just press Ctrl and— yes, Ctrl is this key— no, *together*, and release... no need to wait this long, by the way... and now it's copied! Much faster than using the mouse – I wonder how come you didn't know about it after using the computer for %N years. By the way, Ctrl-V pastes, and Ctrl-X cuts, and Ctrl-A selects everything, but okay, you probably don't care much about the last one, I'll just leave now”.

Similarly, programmers become annoyed when somebody attempts to solve a problem in an unidiomatic way. It doesn't happen because your time is being wasted, it happens because you don't like unidiomatic solutions to *anything*. The same reason you stayed up late the previous night – 20 open tabs about GnuPG and keyservers, because you wanted to add some guy's repository to your system but it was throwing an error about a missing key, and you needed the repository because you wanted to install some program and a binary was available on the site but you want all programs on your laptop to be installed from repositories... You wasted 3 hours on that. (Another hour was wasted on googling what's the *right* way to do autostart in Gnome and installing gnome-tweak-tool and then figuring out why it didn't work.) It goes on and on; you tell yourself it'll make maintaining the system easier in the long run, but in reality you don't know whether it's worth the effort, it's likely that you haven't [done the math][xkcd automation] – you're doing it because you don't like things to be done the “wrong” way, whatever it means. And hence the attempts to prevent others from [going on the wrong path][xkcd XY].

[xkcd automation]: https://xkcd.com/1205/
[xkcd XY]: http://xkcd.com/763/

The thing is, when you start acting on your feelings, you essentially say “I don't care about helping you, what I care about is you not doing the bad thing you want to do, so I'm going to talk you out of it”. And that's the YX problem.

-----------------------------------------------------------------------------

Note that the XY problem doesn't always lead to the YX problem. To steal an example from [Two Attitudes In Psychiatry][SSC attitudes]:

[SSC attitudes]: http://slatestarcodex.com/2016/02/24/two-attitudes-in-psychiatry/

> A mother brings her 6 year old son to the doctor, complaining that he gets nauseous every morning. She wants the doctor to prescribe an anti-nausea pill. The doctor probes further and finds the kid only gets nauseous on school days. In fact, he only gets nauseous on school days when he has a particular gym class. The doctor asks the kid if there are any problems in that gym class, and the kid is reluctant to say anything. After a while, he finally admits there is a bully in that class. The mother calls the school, and the school takes care of the bully. After that the kid is no longer nauseous in the mornings.

In this case, while the mother's solution was wrong, nir goal and the goal of the doctor were aligned – they both wanted the to stop the child from being nauseous. Everything is civil, everyone is happy. Compare it to this example:

> I remember a textbook talking about a case study by a famous psychiatrist. The patient had come in talking about how her husband was being borderline-emotionally-abusive to her. The psychiatrist interrupted her and said that she was perpetuating this dynamic to feed her own narcissism. The patient said this was absolutely not true and she wasn’t narcissistic. The psychiatrist said she would never be able to get over her provoking-her-husband problem until she admitted the depth of her narcissism. The patient refused to keep seeing the psychiatrist after that, and the psychiatrist commented that it had been a hopeless case from the beginning – the extent of her narcissism was so great that she would never acknowledge that somebody else might know more than she did.

The woman wants to deal with nir husband, and the psychiatrist wants... what? If there actually was some trick, some way to help the woman solve nir problem without getting rid of the narcissism, would've the psychiatrist given it to the woman and sent nem away? “But nir real problem is narcissism” – well, if by “real” you mean “root cause”, then why not tell the woman to get a divorce and never marry anyone again? “Because that's not what ne wants”? But ne doesn't want to get rid of the narcissism either.

When you discuss the solution and it turns out that the actual problem is different, or when you take a guess and say “maybe you want X” and it turns out that the other person indeed wanted X, that's not the YX problem. The YX problem is when you *don't like* what the other person wants, and this can't be solved with additional investigation, and thus the discussion turns from “what exactly do you want?” to “why can't you see that you don't want what you want?”.

-----------------------------------------------------------------------------

There are 2 reasons why the YX problem is, well, a problem. The first reason is everything I said earlier, which can be summarised as follows: there should be a way for people to seek help without having someone else's *values* inflicted on them in the process. Just like it's important to have a way to communicate privately even if it means that Bad Guys would be able to use it too, it's important to have a way to get your questions answered even if it means that it would result in Wrong Things being done. (Google is getting there, but it's not enough yet.)

(Also, if you think “but I don't get paid for helping people, so getting to inflict my values on them is okay” – is it okay for the company that produces the messenger you use to read your messages? It's legal, yes, and perhaps even ethical if they warn you in advance, but it doesn't make them particularly nice anyway.)

However, there's another reason why it's bad, and it's about empathy. Specifically: what do you think the other person feels when you say “you probably don't want what you think you want”? To get an idea, read [this comment about Apple Stores](https://news.ycombinator.com/item?id=10024518), for instance, or [this one](https://news.ycombinator.com/item?id=9084152), or just believe me when I say that having people argue with you about what *you* want can make you feel like shit literally for the rest of the day, and the worst thing is that it happens regardless of whether they are ultimately right or not. Yes, people want stupid things sometimes. Yes, sometimes those things are really stupid. Is preventing them from doing those things important enough for you to risk making them feel bad as long as stupid things won't be done?

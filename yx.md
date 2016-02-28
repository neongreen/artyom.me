% The YX problem

---
unfinished: true
---

Here's what I mean by the YX problem: it's when you're asked to solve a problem, but instead you start solving a problem you *feel strongly about*, and not the problem the other person wants you to solve.

But before talking about the YX problem, let's talk about the XY problem. The XY problem is when somebody asks “how to do Y?”, but in reality ne wants to do X (and ne's asking about Y because ne thinks X can be solved by doing Y). Here's an example: you're writing a program and you need to know some file's extension, but instead of asking “what function returns file's extension?” you ask “how to get last 3 characters of a string?” (because you wrongly think that all extensions have 3 characters). Another example: you want a small yellow fluffy chicken which is adorable and yellow and says pip-pip-pip, but instead of just buying a chicken you start asking your friends whether they know where to get a cheap incubator.

The XY problem is well known on forums, Q&A sites, and in chatrooms (yes, people still use IRC to ask their questions). Some dislike it merely because it wastes everyone's time; some dislike it because it's a sign of dangerous ignorance that can hurt other people. However, I think that in many cases there's something else going on.

Here's how a technical person might react when somebody in nir field of view attempts to use mouse to copy a piece of text: “Wait, how come you don't know that Ctrl-C copies things?! Okay, okay, okay, stop doing whatever you were doing right now. You just press Ctrl and— yes, Ctrl is this key— no, *together*, and release... no need to wait this long, by the way... and now it's copied! Much faster than using the mouse – I wonder how come you didn't know about it after using the computer for %N years. By the way, Ctrl-V pastes, and Ctrl-X cuts, and Ctrl-A selects everything, but okay, you probably don't care much about the last one, I'll just leave now”.

Similarly, programmers become annoyed when somebody attempts to solve a problem in an unidiomatic way. It doesn't happen because your time is being wasted, it happens because you don't like unidiomatic solutions to *anything*. The same reason you stayed up late the previous night – 20 open tabs about GnuPG and keyservers, because you wanted to add some guy's repository to your system but it was throwing an error about a missing key, and you needed the repository because you wanted to install some program and a binary was available on the site but you want all programs on your laptop to be installed from repositories... You wasted 3 hours on that. (Another hour was wasted on googling what's the *right* way to do autostart in Gnome and installing gnome-tweak-tool and then figuring out why it didn't work.) It goes on and on; you tell yourself it'll make maintaining the system easier in the long run, but in reality you don't know whether it's worth the effort, it's likely that you haven't [done the math][xkcd automation] – you're doing it because you don't like things to be done the “wrong” way, whatever it means. And hence the attempts to prevent others from [going on the wrong path][xkcd XY], and hence the YX problem.

[xkcd automation]: https://xkcd.com/1205/
[xkcd XY]: http://xkcd.com/763/

-----------------------------------------------------------------------------

A commonly used justification for steering people off the wrong path is that it's in their own interest: “if you write the code this way, it's going to be really hard for you to change it later”. However, the thing is that it may just as easily not be: for instance, “if I help you write that awful code, it might be *me* who will have to maintain it later” (a different example: “no, I won't tell you how to hack sites”). The point is that the underlying feeling is the same in both cases: “I feel bad about you doing it”. You're not doing it because it's in the other person's interest, you're saying “it's in your interest” to convince the other person to listen to you.

On the other hand, when you aren't doing what you were asked to, it's not necessarily the YX problem. To steal an example from [Two Attitudes In Psychiatry][SSC attitudes]:

[SSC attitudes]: http://slatestarcodex.com/2016/02/24/two-attitudes-in-psychiatry/

> A mother brings her 6 year old son to the doctor, complaining that he gets nauseous every morning. She wants the doctor to prescribe an anti-nausea pill. The doctor probes further and finds the kid only gets nauseous on school days. In fact, he only gets nauseous on school days when he has a particular gym class. The doctor asks the kid if there are any problems in that gym class, and the kid is reluctant to say anything. After a while, he finally admits there is a bully in that class. The mother calls the school, and the school takes care of the bully. After that the kid is no longer nauseous in the mornings.

Compare it with this one:

> I remember a textbook talking about a case study by a famous psychiatrist. The patient had come in talking about how her husband was being borderline-emotionally-abusive to her. The psychiatrist interrupted her and said that she was perpetuating this dynamic to feed her own narcissism. The patient said this was absolutely not true and she wasn’t narcissistic. The psychiatrist said she would never be able to get over her provoking-her-husband problem until she admitted the depth of her narcissism. The patient refused to keep seeing the psychiatrist after that, and the psychiatrist commented that it had been a hopeless case from the beginning – the extent of her narcissism was so great that she would never acknowledge that somebody else might know more than she did.

In the former case, the solutions are different but the immediate goals of the doctor and the mother are aligned: they both want the kid not to be nauseous. In the latter case, the patient wants to deal with nir husband, and the psychiatrist wants... what? If there actually was some trick, some way to help the woman solve nir problem without getting rid of the narcissism, would the psychiatrist just give it to the woman and send nem away? “But nir real problem is narcissism” – okay, if by “real” you mean “root cause”, then why not tell the woman to get a divorce and never marry anyone again? Because that's not what ne wants? But ne doesn't want to get rid of the narcissism either.

Programmers Psychiatrists dream of a world where all people , and programmers dream of a world where everything 

Why does “feeling strongly about some other problem” matter? Because when it's present, a conflict is much more likely. When you're a doctor and the mother wants you to give nir kid an anti-nausea pill but you want the school to get rid of the bully, at least you have the same goal one level higher – make the kid not nauseous. But when you feel strongly about preventing bad code from being written, while 

the YX problem appears when the other person clearly communicates that ne doesn't want nir code to be safe/good/etc, and instead of saying “okay, then this solution will be better for you” you say “BUT WHY”

when you want your daughter to be safe, whose interest is that?

when you want your daughter's teeth to be good, whose interest is that?

The “doing what you're asked” and “solving what you think is the actual problem” is pretty much fundamental



-----------------------------------------------------------------------------

how much you dislike the behavior itself

how much is the other person going to dislike the fix

<learning context> (proud of figuring out things by nemself/etc)

mention just how annoying it is (#haskell)

what if the person is shy and wants to avoid being told ne's doing something horribly wrong? what if the person starts panicking when something like that happens, or remembers it for the whole day? what if the person is less risk-averse than you? what if the person values not-being-asked-questions so much that ne'd rather do something wrong than..?

-----------------------------------------------------------------------------

not giving enough options is a sin of programmers (e.g. Telegram desktop and the checkbox thing)

overengineering like Haskell

doing everything with jQuery = the opposite of Haskell: “how to do X with jQuery” usually gets an answer even if it's possible without jQuery



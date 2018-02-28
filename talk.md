# Intro

Hi all, thanks for coming along. This is the first time I'm giving this talk
but because I believe so strongly in what I'm about to say I hope I can make it
much better.

Let's start with something fun. I'm going to assume the role of Gary Bernhardt
and give the infamous JavaScript WAT talk.

And, just to let you know I'll am going to pronounce it "wat", just the same
as Gary Bernhardt does.

# Wat talk

Does anyone know, in JavaScript, what array + array is. Well let me ask you
first, what _should_ array + array be?

Empty array, I would also accept type error.
Uh, that is NOT what array + array is. WRONG! WRONG! Array + array is
*[DRAMATIC PAUSE]* EMPTY STRING!

Obviously, I think that's obvious to everyone.

Now, what should array + object be? This should obviously be type error since
they're completely disparate types. Does anyone know what this is?

*PRESS ENTER*

It's object.

*TYPE {} + []*

Now of course because this is plus you can flip the operands and the same thing
comes out so if we do {} + [] we should get exactly the same thing, which
as you can see *[press enter]*, you do.

And finally, the only one of these that's actually true -- you know
because you add arrays you get an empty string, that doesn't make sense - is
object + object which is in fact *[press enter]* not a number.

So this one's actually right.


*SWITCH TO SLIDES*

EXACTLY! Right, what is even going on? I don't quite understand what person
with a brain in their head would think this is a good idea.

OK, OK, enough making fun of languages which suck, let's talk about JavaScript.

*TYPE Array(16)*

If I say Array(16) I get an array of 16 things which is represents as
16 commas, which is obvious.

*TYPE Array(16).join("wat")*

And if I then join them with a string 16 times I get -- this is actually
the only thing in this entire presentation that is reasonable

*EDIT TO BE: Array(16).join("wat" + 1)*

Now, if I take that string and add a one to it casts the one to a string and
we get wat1 a bunch of times. Fine.

Does anyone know what will happen if I _subtract_ one from the string?

*EDIT TO BE: Array(16).join("wat" - 1)*

I'm assuming no one does, but I'll give you a hint.

*EDIT TO BE: Array(16).join("wat" - 1) + " Batman!"*

Does anyone know?

*[PRESS ENTER]*

*[PAUSE]*

WATMAN

# My thesis

People seem to be scared of functional programming. Also people really don't
seem to like types, at least not at first. There's often this sentiment
that types get in the way, or that they hinder perfectly reasonable
programming idioms.

In a way, it _is_ easier to write in dynamically typed, procedural, imperative
languages. But only easier in the sense that the compiler/interpreter
rejects _less_. It's for this reason that if you took two novices, the novice
that chose the dynamically typed imperative language, will probably get a running
program written faster.

But those of us who are concerned with maintainability will quickly argue that
quick to write does not necessarily mean easy to maintain.

Those of us from the strongly-typed functional programming community will
argue that functional programming makes our programs easier to reason about
and that strong static typing will prevent us from introducing a large
class of bugs.

In today's talk I want to attack the problem of convincing people that their
language sucks from a different perspective, that of programming language
semantics.

Now, let me be the first to say that programming language semantics sometimes
even gives me the willies. Pull up your typical paper on PL semantics
and you couldn't be blamed for thinking you'll need to learn Greek just to
understand it.

*SING THIS*  Gammas and deltas and turnstiles and taus, these are a few of my
favorite symbols.

I can understand the barrier to entry.

But here's the thing you have to realise: if you don't have an _explicit_
model of how your programming language works, you *do* have an _implicit_
model. How else could you reason about the correctness of anything you wrote?

In order to read and understand a program, you simply must have some kind of
model in your head about what each statement in the programming language
means. The higher the fidelity of that model the better you are able to reason
about your program, especially when it's not working the way you intended it
to!

This model, whether you want to call it that or not, _is_ a semantics of the
language.

Now for an aside. If you can get past all the greek in programming language
semantics you'll discover that something known as _operational semantics_ can
also be thought of as a simple interpreter for the language.

The next thing to ask yourself is do you want that semantics to be simple and
concise so that you have a chance of keeping it your tiny little monkey brain,
or would you rather that it is complex, full of special case behaviour and
only lovable by language lawyers and people who want to appear smart?

JavaScript is thought of as an easy language to learn. I must admit it is quite
easy to get some JavaScript up and running and, even though I'm going to bash
it for the rest of this talk, I would still recommend it as a first programming
language since, if you have a browser, you have a programming environment for
it.

However, _easy_ is not the same as _simple_ as Rich Hickey, the inventor of
Clojure said in a very good talk of his called "Simple made Easy"

So tonight, what I thought I'd do was take you through just what the hell was
going on in the WAT talk. Then we'll delve into the EcmaScript standard
and then, we'll take a look an interpreter I wrote for a fragment of JS.
_Remember_: this intrepeter can be thought of as an operational semantics
for JavaScript.

After that we'll look at an interpreter for a more sensibly designed language
and you can be the judge of what is simpler.

# The slight dishonesty in the WAT talk

Okay, so it turns out that Gary Bernhardt took a little theatrical license in
his talk.

It turns out that the last two examples really weren't addition at all. In
fact the first two brackets are not parsed as an empty object. They are parsed
as an empty code block. What follows is a unary plus (which has the effect
of casting to a number) applied to the second argument.

So in fact, you get something much closer to what you've already seen.

*SHOW THEM IN jsc*

Nevertheless, as we'll soon see JavaScript's semantics really are a mess.

# ECMAScript standard

# Simple semantics does not equal simple to reason about

I want to tell you about an important caveat to the message of this talk.

I've shown you a language with a complex semantics and how terrible that is,
but even in a language with simple semantics, it could be hard to reason about
programs _written in that language_.

For instance you could have a simple semantics for an imperative language
which has state, mutation and references. It could still be hard to reason
about your _programs_ even though it was easy to reason about your _language_.

# END NOTE

I'm still working this talk out. I think I'll make a pretty
convincing case for but I'll wager that's because you've self-selected
to be here at FP Syd tonight, and you're already partial to these ideas.

My end-goal is to give this talk at a more mainstream event and have it
convince a few of the skeptics in the audience. If you can give me any ideas
please let me know.
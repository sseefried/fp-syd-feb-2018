# FP Syd February 2018

# Purpose 

Here is a stream of consciousness about what I am trying to achieve with this
talk.  I have long heard from people that they don't understand languages like
Haskell, Scala, etc but they _get_ Python, Ruby, JavaScript. They won't usually
disagree that these languages have warts but they will generally believe that
they can keep the complexity in their head, and that they "know" what a given
piece of code will do.

Now here's where things get a little sticky. If they profess to know what a
given piece of code does they are acknowledging that they have a an _implicit_
mental model of what is going on. What I find strange about this is that they
then tend to shy away from _explicit_ models. Now this might make sense because
the only languages that tend to have these explicit models are the ones that
they think are ivory tower languages or languages that are too hard to
understand.

However, they have never sought out explit models of programming language
semantics for the kinds of languages they like. My argument is that if they did
this then they would find that:

a) no such models exist
b) even if they did exist they would be hopelessly difficult to understand
   and full of special case behaviour.

In short, there would be no deeper unity to the language. It becomes clear that
these languages are very much a hodge-podge of features that interact in ways
even the language creators don't foresee.

So, how do I go about proving this?

I plan to start the talk with a quick run through "Deep C". What's interesting
about this talk is that it appears to be saying "C is difficult, you should try
to learn all of its intracacies" whereas I take away a very different message.
The message I take away is that C is a hard language to learn and reason about.
Why not program in a language that is less complex to reason about?

I also plan to take a brief look at the JavaScript WAT talk for the same
reason.

The difficult part of the talk is whether to introduce some Isabelle semantics
for a simple language and then to see if we can't introduce some of the more
difficult things. I even toyed with the idea of trying to write down the
semantics for a fragment of the JavaScript language.

# Key take aways

- Whether you know it or not you are modelling the language in your
  head. Why not be explicit about it?
- Wouldn't reducing the language constructs to a minimal set of powerful 
  features make sense? Less to remember. Simpler to reason bout.

# DONE

- Had a look at **Deep C**. It turned out not to be as bad as I remembered.
  I think if I am going to use this material I will have to use it sparingly.

- The second reference **SO you think you know C** was a great exercise
  in the perils of _undefined_ behaviour.

# TODO

- Find cases of language creators not foreseeing certain outcomes
- Look at the ECMAScript standard and find sections which are hopelessly complex, written in English and therefore _ambiguous_.
- Find quotes from Brett Victor about "simulating in your head"

--------------------------------------------------------------------------------

# Wat talk

    > [] + []
    
It's the empty string

    > [] + {}
    [object Object]

    > {} + []
    0

    > {} + {}
    NaN

    > Array(16)
    ,,,,,,,,,,,,,,,,

    > Array(16).join("wat")
    watwatwatwatwatwatwatwatwatwatwatwatwatwatwatwat

    > Array(16).join("wat" + 1)

    > wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1

    > Array(16).join("wat" - 1) + " Batman!"
    NaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaN Batman! 

Less silly

    > "3" - 1
    2

    > "wat" - 1
    NaN

--------------------------------------------------------------------------------

> AdditiveExpression : AdditiveExpression + MultiplicativeExpression
> 1. Let lref be the result of evaluating AdditiveExpression.
> 2. Let lval be ? GetValue(lref).
> 3. Let rref be the result of evaluating MultiplicativeExpression. 
> 4. Let rval be ? GetValue(rref).
> 5. Let lprim be ? ToPrimitive(lval).
> 6. Let rprim be ? ToPrimitive(rval).
> 7. If Type(lprim) is String or Type(rprim) is String, then
>    a. Let lstr be ? ToString(lprim). 
>    b. Let rstr be ? ToString(rprim).
>    c. Return the String that is the result of concatenating lstr and rstr. 
> 8. Let lnum be ? ToNumber(lprim).
> 9. Let rnum be ? ToNumber(rprim).
> 10. Return the result of applying the addition operation to lnum and rnum. See the Note below 12.8.5.
 
> NOTE 1: No hint is provided in the calls to ToPrimitive in steps 5 and 6. All
> standard objects except Date objects handle the absence of a hint as if the hint
> Number were given; Date objects handle the absence of a hint as if the hint
> String were given. Exotic objects may handle the absence of a hint in some other
> manner.

> NOTE 2: Step 7 differs from step 3 of the Abstract Relational Comparison 
> algorithm, by using the logical‐or operation instead of the logical‐and operation.


Why is `[] + []` the empty string? 

I can't be sure but I think it's because `ToPrimitive([])` yields `""`
But when I read through `ToPrimitive` that is not what I discover.
I had a look at `GetValue`. I don't think it can be that either.

There is no primitive `Array` type! `Array` is actually an `Object`. 
https://stackoverflow.com/questions/31709156/does-javascript-have-exotic-objects

Okay, I've worked out the full process.

- @12.8.3-1 `lref` is bound to array literal `[]`
- @12.8.3-2 `lval` is `GetValue([])` which returns `[]`
- @12.8.3-3 `rref` is bound to `[]`
- @12.8.3-4 `rval` is `GetValue([])` which returns `[]`
- @12.8.3-5 `lprim` is bound to `ToPrimitive(lval)`
    - @7.1.1-1 Assert that it is ECMAScript language value is true.
    - @7.1.1-2 `Type([])` is `Object` and 
      a) hint is set to `default`
      b) n/a
      c) n/a
      d) `exoticToPrim` is bound to `GetMethod(input, @@toPrimitive)` which is 
         `undefined` in this case.
      e) n/a
      f) hint was `default` so now it is set to `number`
      g) `OrdinaryToPrimitive([], "number")` is called
        - @7.1.1.1-1 Assertion that it is `Object` is true        
        - @7.1.1.1-2 Assertion that hint is "string" or "number" is true
        - @7.1.1.1-3 n/a since `hint` is `"number"`
        - @7.1.1.1-4 `methodNames` is set to << "valueOf", "toString" >>
        - @7.1.1.1-5 We try `valueOf` first, which is `IsCallable` but after calling
          it we get `[]` back which is an `Object` so now we need to try `toString`
          which returns the empty string `""`. We now return it and skip step @7.1.1.1-6
- @12.8.3-6 [elided] Returns `[]` for similar reasons as above.
- @12.8.3-7 `Type(lprim)` is `String` so we get `lstr` is `""`, `rstr` is `""`
  and we concatenate the two together to `""`

Et voila.

--------------------

So why does `{} + []` evaluate to `0`. I would expect to to be
`[object Object]`. However `({}) + []` does evaluate to `[object Object]`.
It turns out that `{} + []` does not parse as and Object added to an Array.
It parses as a code block followed by `+[]`.

So `{} + {}` is `NaN` because `+{}` is `NaN`.

To understand `+[]` see @7.1.3. `+[]` first calls `ToPrimitive([])` which we
known returns `""`. Then `ToNumber` is called on this which yields `0`.

`+{}` is similar except now `ToNumber` is called on `"[object Object]`" which
yields `NaN`.


--------------------------------------------------------------------------------

# Basic flow of talk as of 18 Feb 2018

I'm thinking the talk will go like this:

a) I will introduce the Wat talk.
b) I will then go through the derivation and I will explain that the last
   two cases really weren't fair since he trying to say that binary addition
   was happening when in fact that was not the AST that parsed at all.
c) I will present an interpreter I've written for a very simple arithmetic
   language and then one I have written for JavaScript.


# References

**So you think you know C**
https://hackernoon.com/so-you-think-you-know-c-8d4e2cd6f6a6

**Deep C (and C++)**
https://www.slideshare.net/olvemaudal/deep-c

**Exotic objects in JavaScript**
https://stackoverflow.com/questions/31709156/does-javascript-have-exotic-objects

**Explains the Wat talk**
http://2ality.com/2012/01/object-plus-object.html
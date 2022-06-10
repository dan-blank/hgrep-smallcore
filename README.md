# hgrep-smallcore

hgrep-smallcore is yet another haskell reimplementation of [grep](https://en.wikipedia.org/wiki/Grep) based on the paper ["Regular-expression derivatives reexamined"](https://www.ccs.neu.edu/home/turon/re-deriv.pdf). I wrote this together with [Borna Bešić](https://github.com/bornabesic) (who focused on the user facing parts, while I focused the regex engine) as an university project and polished it up slightly. It is able to process ERE compliant regexes (for the moment, sans [negated character sets](https://www.regular-expressions.info/charclass.html)). Works best on Linux. Interesting features:

* It reduces [regexes](https://www.regular-expressions.info/posix.html) to a small core of 4 constructs, where the smallest construct is that of a character set (see 4.2 in the paper):
``` Haskell
data CRE =
    CUnion CRE CRE  | -- "a|b"
    CConcat CRE CRE | -- "ab" 
    CStar CRE       | -- "a*"
    CSet CRange       -- i.e. [Singleton 4] or [SpanRange 0 136756]
```

* Due to character sets as central concept, hgrep-smallcore supports Unicode out of the box.

Install
---
Within the folder, run "stack install". This will create an executable within the subfolder .stack-work.

Usage
---
On the command line, call path-to-binary/hrep-exe.exe PATTERN FILE* 

Where PATTERN is a ERE compliant regex and FILE* is at least one file.

Lessons and concepts learned
----
**[print-based debugging with Debug.Trace](https://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html):** Since I use Haskell only in academic, simple contexts so far, there was no need for debugging capabilities until I started this project. Debug.Trace provided exactly what I needed: Simple, print-based debugging.

**[Pattern synonyms](https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms):** Useful for making matching complex constructors more readable.

**Mistake: Use of constants in-place as guards in pattern matching.** Wrong example:
``` Haskell
data SomeWrapperAroundAnInt = SomeWrapperAroundAnInt Int

someConstant = 42

someFunction (SomeWrapperAroundAnInt someConstant) = ... -- do something, assuming that the input is (SomeWrapperAroundAnInt 42)
```
When using `someConstant` within a pattern matching, `someConstant` functions as a new variable name and binds whatever value is at this position in the pattern. I misused the constant as a guard, expecting the pattern to only match when the value at this position is equal to the constant. A rather obvious mistake in hindsight. Here is a stackoverflow thread with solutions that work: https://stackoverflow.com/questions/35429144/haskell-using-a-constant-in-pattern-matching

# NicLib

*nixy's presonal ```Prelude```*

## Current Status

Much of library has been pruned in favor of better packages I've recently discovered (see below.)

## Summary

A library I use for my personal projects. I put it here hoping it'll be helpful to some people. Most functions herein are well written and complete (i.e. not hacks.) Some have been optimized, and some not. Most exported functions are ready-to-use for any project you have, but even some exported functions are hacks that I wrote for testing purposes, or ghci sessions. Such functions may be removed at any future commit, so if you want them, you might want to copy them into your own personal library, unless you want to search through GH's commit history ;) In fact, on occasion I'll do a huge revamp of the whole library, rewriting in an entirely different style, using different methodologies or frameworks, and new idioms. So as much as each commit is stable, each commit may break compatibility with the previous one.

I'd said that I'd have QuickCheck and maybe too HUnit tests by now, but I was wrong. After all,...who needs testing...? (o.o); Still not yet considering benchmarks.

For every commit, all source files in should compile with ```stack build```

I expect that I'll be the only person to actually do a build NicLib, but if you want to stack build to use some functions in any serious capacity, but aren't sure if you should due to stability &ndash; or if you generally have any passing comments or questions, feel free to contact me about it ([nicholas.chandoke@gmail.com](nicholas.chandoke@gmail.com)). (But of course if it's a question the answer of which may help others, please [open an Issue](https://github.com/nick-chandoke/niclib/issues/new) about it.)

I write NicLib to account for lacking functionality that I commonly use. I'm more than happy to discover that someone else has done a better job and already put the package on stackage or hackage. My goal is not to compete with others or write great code just for accolades; I merely wish for things to be possible, easy, and elegant, even if I need to make it so &ndash; all the better if I don't! If you know of a package that's been around (and has probably been tested or optimized better than mine) that has the same functionality found here in NicLib, please open an Issue in Github about it, so that I may remove & deprecate the function from NicLib in favor of the suggested one.

If NStdLib doesn't have what you're looking for, have a look at <http://hackage.haskell.org/packages/#cat:Control>; these are the kinds of functions I find myself writing.

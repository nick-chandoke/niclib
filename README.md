<p> A library I use for my personal projects. I put it here hoping it'll be helpful to some people. Most functions herein are well written and complete (i.e. not hacks.) Some have been optimized, and some not. Most exported functions are ready-to-use for any project you have, but even some exported functions are hacks that I wrote for testing purposes, or ghci sessions (for example, <code>fileToDOM</code> in <code>NicLib.HTML</code>). Such functions may be removed at any future commit, so if you want them, you might want to copy them into your own personal library, unless you want to search through GH's commit history ;) In fact, on occasion I'll do a huge revamp of the whole library, rewriting in an entirely different style, using different methodologies or frameworks, and new idioms. So as much as each commit is stable, each commit may break compatibility with the previous one. For the forseeable future, the version number will stay at 0.1.0.</p>
<p>I've just recently learned stack &amp; cabal, and I'm quickly picking-up QuickCheck and HUnit. Tests will be added to the repo in June~July 2018. Not sure about benchmarks yet.</p>
<p>For every commit, all source files in should compile. Currently I'm using GHC 8.4.1 (but GHC >=8.2 should work fine.)</p>
<p>I expect that I'll be the only person to actually do a cabal install on NicLib, but if you want to <code>cabal install</code> to use some functions in any serious capacity, but aren't sure if you should due to stability &ndash; or if you generally have any passing comments or questions, feel free to contact me about it (<a href="mailto:nicholas.chandoke@gmail.com">nicholas.chandoke@gmail.com</a>). (But of course if it's a question the answer of which may help others, please <a href="https://github.com/nick-chandoke/niclib/issues/new">open an Issue</a> about it.)</p>
<p>The library consists of
<ul>
	<li>Aeson.hs: 3 Aeson/JSON functions to make life easier (actually, now that I'm using git and contributing, I'll see about trying to get these functions put into the aeson package)</li>
	<li>NStdLib.hs: a place where I put useful generalzations of functions that I find myself writing for specific personal or professional projects</li>
	<li><ul>Structures
		<li>IndexedSet.hs: a questionably efficient Map. I created it for storing large sets of URLs; IndexedSet allowed me to store a list of Ints, each of which referenced a URL. By breaking-down a URL into common values (schemes and domains), I greatly reduced my memory footprint. However, I never really used IndexedSet; almost the next day after writing it, I began writing Trie, which was much easier and more efficient.</li>
		<li>Trie.hs: I couldn't understand how the Trie in containers package was a trie, or how to use it. I'm pretty sure it's different from what I was looking for, i.e. <a href="https://en.wikipedia.org/wiki/Trie">trie as described on Wikipedia</a>. My implementation of Trie allows tagging each branch with any given value.</li>
		<li>Buffer.hs: Keep track of n most recently accessed elements of a sequence</li></ul>
	<li>FileSystem.hs: Filesystem- and filename-specific convenience functions. Originally in NStdLib, but I thought them many and specific enough to deserve their own module.</li>
	<li>HTML.hs: functions built around <a href="http://hackage.haskell.org/package/taggy">taggy</a>. Mostly used for scraping</li>
	<li>IO.hs: typedefs for <code>StateT</code> and <code>ExceptT</code>, which I frequently use in HTHL and Net.hs. Also the typedef <code>ErrorMsg</code>, which I'm currently trying to generalize to any <a href="http://hackage.haskell.org/package/ListLike"><code>ListLike</code></a>.</li>
	<li>Net.hs: functions built around <a href="http://hackage.haskell.org/package/http-conduit">http-conduit</a> that makes HTTP GET'ting and exception handling easier</li>
	<li>URL.hs: I found the network-uri package useless. I needed a module for web browser-like handling of URLs, with somewhat forgiving parsing, and a much better definition of relative vs. absolute URL. Works, and it tested, but is still under development, as it doesn't handle parsing of URLs in general (e.g. irc::// schemes, or URLs with HTTPS login and port number specification.)</li>
	<li>Parser.hs: contains 1 function &ndash; <code>parse</code> &ndash; that takes a list of states and state transformations and parses-out a value.</li>
</ul>
Almost all modules rely on NStdLib, perhaps most notably using the <code>Isomorphism</code> typeclass, and the <code>FilePath</code> newtype.</p>
<p>If NStdLib doesn't have what you're looking for, have a look at <http://hackage.haskell.org/packages/#cat:Control>; these are the kinds of functions I find myself writing.<br/>
If you know of a package that's been around (and has probably been tested or optimized better than mine) that has the same functionality found here in NicLib, please open an Issue in Github about it, so that I may remove & deprecate the function from NicLib in favor of the suggested one.</p>

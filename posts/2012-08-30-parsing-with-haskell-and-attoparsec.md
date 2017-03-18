---
title: Parsing with Haskell and Attoparsec
category: Haskell
---

In an effort to better understand parsing in the Haskell world, I decided to port a hand-crafted parser I'd written for Subversion dump files in [C++11](https://github.com/jwiegley/subconvert/blob/master/src/svndump.cpp) into a Haskell version. It's not that the old parser didn't work well, but I wanted to see what Haskell could make of the problem with its laziness and combinator-oriented parsing libraries.

If you want to follow along in your editor, here is the [C++ code](https://github.com/jwiegley/subconvert/blob/master/src/svndump.cpp) and the [Haskell code](https://github.com/jwiegley/svndump/blob/master/src/Subversion/Dump/Raw.hs).

My first approach was writing the parser using [Parsec](http://www.haskell.org/haskellwiki/Parsec). This indeed turned out to be quite easy and clear. However, I wrote the parser in a naïve way, and thus it took an hour and a half to parse my 11 gigabyte file (by comparison, C++ takes 112 seconds). I later learned what I had done wrong, but not before switching to [Attoparsec](http://hackage.haskell.org/packages/archive/attoparsec/0.10.2.0/doc/html/Data-Attoparsec-ByteString.html).

Attoparsec is quite similar to Parsec; so much so that I was able to rewrite the parser before that hour and a half had finished. Nothing else was changed: just by using Attoparsec, the time dropped to 38 minutes. That led me to believe that it was a better choice in general for my particular problem.

Allow me now to digress for a moment and describe the Subversion dump file format, so you can better understand why Attoparsec was a good choice. Essentially a dump file looks like this:

    Header
    Revision info
    Node info
    Node info
    Node info
    Revision info
    Node info
    Node info
    ...

The nodes following each revision make up the actions representing that revision. If a new file was committed to Subversion, for example, there would be an "add" node for it in that revision's list. Each revision and node looks roughly like this:

    Headers
    [Properties]
    [Data Blob]

The headers are simple `Key: Value` type fields, containing 7-bit ASCII. Properties are length-variable, and may contain UTF-8 encoded text. For example:

    K 7
    svn:log
    V 20
    This is a log entry.

After the properties there *might* be a binary blob, which is a bunch of bytes specified by a length field in the headers. No decoding should take place for this data, since it represents exactly what was committed to version control.

The reason Attoparsec handles this well is that the data is just a stream of bytes, with interpretation happening after the parse, not during. So Attoparsec lets me tear through it without doing any extra work -- work I can defer until later, when the user actually needs those decoded values.

Where was I? Ah yes, at 38 minutes. The next step was to look at the process monitor, where I learned the parser was consuming 18 Gb of memory to parse my file. This meant it was keeping all the blobs in memory, whether they were needed or not. Based on [another post](http://stackoverflow.com/questions/4151265/attoparsec-allocates-a-ton-of-memory-on-large-take-call), I learned the problem lay in asking Attoparsec to return a list of entries. Because of the way Attoparsec works, it needs to build a complete list before returning anything. The solution, therefore, was to call the parser repeatedly in a function that appends each entry to a list after it's parsed. Here's what the old code looked like:

``` {.sourceCode .literate .haskell}
readSvnDumpRaw_Old :: BL.ByteString -> [Entry]
readSvnDumpRaw_Old input =
  case parse (parseHeader *> many1 parseEntry) input of
    Fail _ _ y -> error y
    Done _ xs  -> xs
```

And here's the new code:

``` {.sourceCode .literate .haskell}
readSvnDumpRaw :: BL.ByteString -> [Entry]
readSvnDumpRaw dump =
  case parse parseHeader dump of
    Fail {}         -> error "Stream is not a Subversion dump file"
    Done contents _ -> parseDumpFile contents

parseDumpFile :: BL.ByteString -> [Entry]
parseDumpFile contents =
  case parse parseEntry contents of
    Fail _ _ y       -> error y
    Done contents' x -> x : parseDumpFile contents'
```

This change, allowing the parse to be lazy with respect to entries, brought the total time down to 4 minutes. Still a bit far from 112 seconds in the C++ case, but now within reason. I thought perhaps this was as fast as it was going to get, but no. With help from Cale Gibbard on \#haskell, I was able to use the Haskell profiler and find another huge gain.

Attoparsec and strictness
-------------------------

Attoparsec is a strict parser, which means it requires the `ByteString` it parses to be fully read into memory. However, there is a variant, `Attoparsec.Lazy`, that adds a *little bit* of laziness by breaking the input into chunks. The parsing is still strict, but its strict over shorter ranges of data.

The downside is that it does still all have to be read. Whether it gets read all at once, or in chunks, that's a lot of I/O -- especially if the caller may never need the results of all that I/O.

Take a look at my test driver:

``` {.sourceCode .literate .haskell}
main :: IO ()
main = do
  [fileName] <- getArgs
  contents   <- BL.readFile fileName
  print $ length $ readSvnDumpRaw contents
```

All this does is count the number of entries in the file. I don't need the contents of the blobs to be read in; in fact, I want them skipped over and that data never loaded. In C++, I use `fseek` to skip over the blob contents if I know the caller will never use them (and this knowledge needs to be passed in before-hand as a parameter). But since Haskell is lazy, I thought this feature would come for free. Why was my parser still so much slower?

The answer lay in `Attoparsec.Lazy`'s semi-strictness. It was still reading in all the blobs, just in chunks now. To fix this and avoid reading the blobs at all, I just it out of the Attoparsec parser. Now instead of returning fully parsed `Entry` values, the parser returns an `Entry` with everything but the blob, plus an integer indicating how long the blob is. I can then skip over the blob in my lazy `ByteString` and let Attoparsec parse the headers for the entry after. In essence, I removed blob parsing Attoparsec entirely and turned it into a header-only parser.

The change to the parsing function was minimal:

``` {.sourceCode .literate .haskell}
parseEntry' :: Parser (Entry, Int)

parseDumpFile' :: BL.ByteString -> [Entry]
parseDumpFile' contents =
  case parse parseEntry' contents of
    Fail _ _ y -> error y
    Done contents' (entry, bodyLen) ->
        entry { entryBody = BL.take (fromIntegral bodyLen) contents' }
      : parseDumpFile (BL.drop (fromIntegral bodyLen) contents')
```

With this change, the total time dropped to 26 seconds! To put this time into perspective, here are some comparative numbers:

  --------------------------------------------------------------------------
  Method
  Time
  Lines of Code
  ------------------------ ------------------------ ------------------------
  `egrep`                  C++                      Haskell
  62s                      112s                     26s
  1                        471                      68
  --------------------------------------------------------------------------

So in the end, a program only 68 lines long (not including blank lines and comments) was able to parse 11 gigabytes of data 2.4 times faster then egrep -- and into a meaningful data structures to boot. That's a rate of 401 Mb/s, pretty near the maximum transfer speed of my RAID-0 array. Also, it required none of the tricks I had to pull in C++11, since Haskell's laziness means that Unicode and other conversions only happen for those entries the user is interested in -- and for just the particular fields they're interested in!

To remove the disk I/O bottleneck, I reran the tests on a RAM disk:

  -------------------------------------------------------------------------
  Method
  Time
  ------------------------------------ ------------------------------------
  `egrep`                              C++
  59.4s                                93.7s
  -------------------------------------------------------------------------

The C++ parser improved slightly, but the Haskell parser sped up 2.5 times, clocking in at just over 1 Gb/s! That's amazing, and much better than I'd hoped. Plus I found the resulting code clearer, easier to write, and a fair bit more fun to work with.

I consider this a major win for Haskell's lazy-oriented style, even if it did take a few hours with the profiler to find the best way to take advantage of it. The code is [here](https://github.com/jwiegley/svndump/blob/master/src/Subversion/Dump/Raw.hs), or you can install the `svndump` package from Cabal.


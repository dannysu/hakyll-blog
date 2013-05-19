---
date: 2013-05-19 00:47:52
title: Toronto Library New Holdable DVDs With Haskell
tags: Mad Coding, Haskell
---
The Toronto library doesn't allow you to put a hold on newer DVDs until several
months later. When a new batch of DVDs becomes holdable on the 15th of every
month they'll show up on [this list][1]. This weekend I was reminded of the idea
to write a program to automatically grab the list and check Rotten Tomatoes for
ratings to decide what movies to put on hold. I started writing the program in
Haskell and my progress so far is now on my [github][2]. What's missing right
now is actually logging in and place a hold.

<br>
**Use HTTP.Conduit to fetch web pages**

I used [http-conduit][3] to grab the HTML source from Toronto library website.
It's pretty straightforward. Just install it by `cabal install http-conduit`,
then use the `simpleHttp` function.

<pre class="brush:haskell">
import Network.HTTP.Conduit

main :: IO ()
main = do
    content &lt;- simpleHttp newMoviesURL
</pre>

<br>
**Use regex-tdfa for regular expressions**

Whenever I need to use regex to extract data from HTML source code, I used
[regex-tdfa][5]'s =~ function.

Example:

<pre class="brush:haskell">
import qualified Data.ByteString.Lazy as L
import Text.Regex.TDFA ((=~))

updated :: L.ByteString -&gt; L.ByteString
updated s = if length matches &gt; 0
            then last $ head matches
            else L.empty
              where matches = s =~ "&lt;h3[^&gt;]*&gt;Updated (.*)&lt;/h3&gt;"
</pre>

<br>
**Use Data.Aeson for parsing JSON**

For parsing Rotten Tomatoes JSON API data, I used [aeson][4] package for that.
Install it by `cabal install aeson`. Below is how I mapped the JSON result to
what I need.

First the declarations:

<pre class="brush:haskell">
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics (Generic)

data RTCast = RTCast {
      name :: L.ByteString
    } deriving (Show, Generic)

data RTMovie = RTMovie {
      year :: Int
    , ratings :: RTRatings
    , abridged_cast :: [RTCast]
    } deriving (Show, Generic)

data RTInfo = RTInfo {
      movies :: [RTMovie]
    } deriving (Show, Generic)

instance FromJSON RTInfo
instance FromJSON RTMovie
instance FromJSON RTCast
instance FromJSON RTRatings
</pre>

then to actually decode:

<pre class="brush:haskell">
    let rt = decode content :: Maybe RTInfo
</pre>

  [1]: http://www.torontopubliclibrary.ca/books-video-music/video/new-holdable-adult.jsp
  [2]: https://github.com/dannysu/new-holdable-dvd
  [3]: http://hackage.haskell.org/package/http-conduit
  [4]: http://hackage.haskell.org/package/aeson
  [5]: http://hackage.haskell.org/package/regex-tdfa

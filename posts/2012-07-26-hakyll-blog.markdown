---
date: 2012-07-26 00:27:00
title: Blog now powered by Hakyll
tags: Miscellaneous, Mad Coding, Hakyll, Haskell
---
**UPDATE:** See my [new blog post][6] about upgrading to Hakyll 4.

This post talks about my experience converting my WordPress blog to a static
site generated using [Hakyll][2].

Recently I read about [Jekyll][1], which is a static site generator written in
Ruby. The idea of a static site generator really appeals to me because there is
nothing dynamic about my blog. I was using WordPress which needs PHP and MySQL,
but all of the content could be generated once and be cached forever anyway. I
also found out about Hakyll which is a similar thing written in Haskell. I
thought "Why not convert my personal blog to use Hakyll? Gives me some excuse to
practice some Haskell too!". After about a week trying to get up to speed on
Hakyll, my blog is now completely static and looks nearly identical to my
wordpress version.

There are some very nice benefits of being a static site:

- More secure without WordPress, MySQL and PHP. No more updating to get security
  patches.
- Free hosting from GitHub Pages or cheaper hosting.
- Faster and handles load better too!

It does take some time to convert so below are some of my tips after having gone
through the experience.


# Data Migration

For exporting my wordpress pages and posts, I used [exitwp][3]. It's pretty easy
to use, but I did have to go through all pages and posts once again to fix
things up. Particularly because I used to upload files and images to wordpress
and the references to those files had to be updated.


# Coding Site Logic Using Hakyll's Domain Specific Language (DSL)


### Recent Posts On Every Page

My wordpress blog shows 10 most recently added posts on every single page in the
footer. I tried to program the same thing in Hakyll but eventually had to use a
workaround.

Hakyll builds up a dependency graph for the rules that you write in the DSL.
However, to have recent posts on all pages there needs to be a cycle. In order
to build the list of recent posts, I needed to have processed all posts first.
While processing all posts I needed to know what are the recent posts, thus the
cycle.

As a workaround I ended up using Rake and go through the site generation process
twice. First pass generates a recent_template.html file containing the 10 most
recent posts. The second pass re-generates all pages including links to recent posts.

In Rakefile:

```ruby
task :build do
  sh "./site rebuild"
  sh "cp _site/recent_template.html recent.markdown"
  sh "./site rebuild"
  sh "rm _site/recent_template.html"
end
```


### Markdown Reference-Style Links

Hakyll uses [Pandoc][4] to enable markdown syntax. Unfortunately Pandoc doesn't
enable markdown's reference-style link syntax by default. The way to get that
working in Hakyll is by calling pageCompilerWith instead of pageCompiler.

```haskell
-- Allow for reference style links in markdown
pandocWriteOptions = defaultWriterOptions {
      writerReferenceLinks = True
    , writerLiterateHaskell = True
    }

match "posts/*" $ do
    route   $ wordpressRoute
    compile $ pageCompilerWith defaultHakyllParserState pandocWriteOptions
```

Pandoc looks awesome and outputs to epub too! Something to play with for my
next document conversion needs.


### WordPress style links

My wordpress blog uses links that simply look like directories. E.g.
http://dannysu.com/2012/07/26/hakyll-blog/  
In order to maintain the same links as much as I can, I wrote the following
function to have Hakyll generate index.html under the same directory structure.

For a file in "posts/2012-07-26-hakyll-blog.markdown" it is routed to directory
"_site/2012/07/26/hakyll-blog/index.html" using the following code:

```haskell
wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "") `composeRoutes`
        gsubRoute "pages/" (const "") `composeRoutes`
            gsubRoute "^[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceWithSlash)`composeRoutes`
                gsubRoute ".markdown" (const "/index.html")
    where replaceWithSlash c = if c == '-' || c == '_'
                                   then '/'
                                   else c
```


### Paginated Post Listing

For post listing, my old wordpress blog uses pagination and links such as
"/blog/page/1/", "/blog/page/2/", etc. To get the same, I used code from [Ian
Ross's blog][5] also generated using hakyll.


# Haskell Confusion

My Hakyll blog is to my liking now, but I did have to tinker with Hakyll for a
while. I find that reading Haskell code still tough when I encounter new
compiler flags that I'm not familiar with.

For the longest time I was staring at the DSL for Hakyll wondering how the type
system converted "templates/*" to (Pattern a)

```haskell
match "templates/*" $ compile templateCompiler
```

Turns out it was due to OverloadedStrings:

```haskell
{-# LANGUAGE OverloadedStrings -}
```

  [1]: https://github.com/mojombo/jekyll/
  [2]: http://jaspervdj.be/hakyll/
  [3]: https://github.com/thomasf/exitwp/
  [4]: http://johnmacfarlane.net/pandoc/index.html
  [5]: https://github.com/ian-ross/blog
  [6]: /2013/03/20/hakyll-4/

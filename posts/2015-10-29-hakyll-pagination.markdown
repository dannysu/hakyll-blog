---
date: 2015-10-29 20:34:07 PDT
title: Pagination with Hakyll
tags: Mad Coding, Hakyll, Haskell
---
When Hakyll v4 was released, I wrote about upgrading from v3 to v4 and I also
wrote about [paginated post listing][1]. Things have changed since the release
of Hakyll v4.5.4.0 as it now gained pagination support. That's good because
I've been wanting to make my post listing better by showing more than 2 posts
per page, so I figured it's time to start using some of the built-in support
for pagination.

There aren't many examples out there, but [Jasper's own site][2] is a good
source. In particular look at the photos code in Main.hs. I'll go through my
own implementation here as well.

The new things you need to know are `buildPaginateWith`, `paginateEvery`,
`paginateRules`, and `paginateContext`. Probably the best way to know how to
use them is to just read the [source code][3] for their definitions.

# buildPaginateWith

The `buildPaginateWith` is defined as follows:

```haskell
buildPaginateWith
    :: MonadMetadata m
    => ([Identifier] -> m [[Identifier]])  -- ^ Group items into pages
    -> Pattern                             -- ^ Select items to paginate
    -> (PageNumber -> Identifier)          -- ^ Identifiers for the pages
    -> m Paginate
buildPaginateWith grouper pattern makeId = do
```

You need to supply a `grouper` function that decides how to separate a list of
Identifiers into smaller groups. For me, I decided on dividing the list into
sub-lists of 10 as shown in my implementation below. I chose to call my
function `grouper` as well keeping it the same as what's used in Hakyll code.
The new `paginateEvery` function is similar to the `chunk` function in my old
code.

```haskell
-- Run sortRecentFirst on ids, and then liftM (paginateEvery 10) into it
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 10) . sortRecentFirst) ids
```

The second argument to supply to `buildPaginateWith` is a Pattern, which you
can just give it a string and the OverloadedStrings feature will convert that
to a Pattern. I wrote about [this][5] in my Hakyll 3 post.

Finally the last argument is a function that turns a paginated page number into
an Identifier. This is also up to you how you want to present it in the end for
your website. For me I'm having it as `/blog/page/{{page number
here}}/index.html` as shown.

```haskell
makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "blog/page/" ++ (show pageNum) ++ "/index.html"
```

Then in my main function it's called like this:

```haskell
main = hakyll $ do
  pag <- buildPaginateWith grouper "posts/*" makeId
```

# Generate Paginated Pages

Next comes to actually generating the paginated pages. Some of the code below is
specific to my site, so you can just ignore those.

paginateRules is defined as:

```haskell
paginateRules :: Paginate -> (PageNumber -> Pattern -> Rules ()) -> Rules ()
```

You'll be passing the **Paginate** you created with `buildPaginateWith` previously
as the first argument.

`paginateRules` will invoke the 2nd argument with the paginated page number
being process and a `Pattern` that matches all the stuff that should go on that
paginated page.

We'll make use of the ability for producing a list of items. [Hakyll site][6]
has a nice tutorial on this as well.

What I do first is loading all the final snapshots of things that'll go on this
paginated page and make sure to sort it properly:

```haskell
posts <- recentFirst =<< loadAll pattern
```

I then get a paginate context with the new `paginateContext` function, which
gives you variables useful for dealing with navigations. We'll be using those
later on.

Then you'll use `listField` to have a *posts* key that you can reference later
in the template to display a list of things.

Putting them all together it ends up looking like this:

```haskell
main = hakyll $ do
  paginateRules pag $ \pageNum pattern -> do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll pattern
          let paginateCtx = paginateContext pag pageNum
              ctx =
                  constField "title" ("Blog Archive - Page " ++ (show pageNum)) <>
                  listField "posts" (teaserCtx tags) (return posts) <>
                  paginateCtx <>
                  defaultContext
          makeItem ""
              >>= loadAndApplyTemplate "templates/blogpage.html" ctx
              >>= relativizeUrls
```

Note that the teaserCtx is specific to how I want my posts to appear as. This
is mainly because I don't want to show the entirety of the post content. What
you use there is up to you.

# Templating

The `templates/blogpage.html` is where it makes use of the post contents. I use
the `$for(posts)$` construct to display the post title, date, its tags, and a
link to the full content.

```html
$for(posts)$
<div class="story">
    <div class="title"><a href="$url$" rel="bookmark" title="Permanent Link to $title$">$title$</a></div>
    <div style="opacity:0.8;"><small>
            <i>$date$ / $tags$</i>
    </small></div>
    <br>
    <p>$teaser$ <span class="read-on"><a href="$url$">&hellip;</a></span></p>
</div>
$endfor$
```

Then on the same page, you now can use the paginate context stuff to link it
all together. Below I'm using if-statements for checking `previousPageNum` or
`nextPageNum` and displaying `previousPageUrl` or `nextPageUrl` accordingly. If
you take a look at the Paginate.hs source code, you'll also see other available
things in the paginateContext that might be of interest.

```html
<div class="story twocol-story newer">
$if(previousPageNum)$
<a href="$previousPageUrl$">◀ Newer Entries</a>
$endif$&nbsp;
</div>
<div class="story twocol-story older" style="text-align:right;">
$if(nextPageNum)$
<a href="$nextPageUrl$">Older Entries ▶</a>
$endif$&nbsp;
</div>
```

---
## 

For the complete source code, see my [github][4].

  [1]: /2013/03/20/hakyll-4/
  [2]: https://github.com/jaspervdj/jaspervdj/blob/master/src/Main.hs
  [3]: https://github.com/jaspervdj/hakyll/blob/master/src/Hakyll/Web/Paginate.hs
  [4]: https://github.com/dannysu/hakyll-blog/blob/master/site.hs
  [5]: /2012/07/26/hakyll-blog/
  [6]: http://jaspervdj.be/hakyll/tutorials/04-compilers.html#producing-a-list-of-items-for

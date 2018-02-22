# What We Like

This is an online gallery consisting of DeviantArt pictures favourited (liked)
by a pre-defined group of users. If you're running an online community that has
some presense on DeviantArt, you might want to create such a gallery to show
off works that your community members like.

# How it works

Each DeviantArt user has an Atom feed with all his/her favourites. The feed's
URL looks like this:
https://backend.deviantart.com/rss.xml?q=favby%3AMinoru2048%2F70385256&type=deviation.
The URL contains both the nickname and user's numeric ID. The only way to get
this URL (as far as I know, at least) is to request
https://minoru2048.deviantart.com/favourites/ and to find `<link
rel="alternate" …>` in the page's `<head>`.

Those feeds are fetched periodically, parsed and stuffed into the DB. The
website simply presents latest entries.

DeviantArt doesn't enable Cross-Origin Resource Sharing, so we cache all images
on your server. You have to set the size of the cache, and we'll delete older
images as new ones arrive. This also means you shouldn't link to images hosted
on your servers—eventually they'll be gone, and your links will break. Give
credit to the original creators—link directly to their Deviations instead!

Instead of missing images, we might want to serve a placeholder saying that the
image is gone. We could provide more info if we kept information on all
entries, but that would lead to unbounded DB growth—not a good thing. So dumb
placeholder it is.

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically be recompiled and redeployed to localhost.

## Tests

```
stack test --flag what-we-like:library-only --flag what-we-like:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.

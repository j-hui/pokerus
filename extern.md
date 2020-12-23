External Dependencies
=====================

I track these using [Git subtrees][git-subtree] rather than submodules,
because these are a little less painless to clone, maintain, and delete.
No need for remote tracking either---that's just more setup to do.

[git-subtree]: https://www.atlassian.com/git/tutorials/git-subtree

I add dependencies like this:

    git subtree add --prefix extern/.pokerus-extern/<dependency> <dependency-git-url> master --squash

Updates look like:

    git subtree pull --prefix extern/.pokerus-extern/<dependency> <dependency-git-url> master --squash

Update commands are saved in `update-extern.sh` for convenience.


Dependencies
------------

So far, I have the following dependencies:

- [`wfxr/forgit`][forgit]: FZF for interactive Git commands

[forgit]: https://github.com/wfxr/forgit

% Some common and annoying mistakes in Haddocks


Slashes and broken links
------------------------

Slashes mean emphasis, so this:

    This is Haskell: (http://en.wikipedia.org/wiki/Haskell)

would be rendered like this:

> This is Haskell: (http:/*en.wikipedia.org*wiki/Haskell)

Haddock tries to autodetect links, but often fails – so better always wrap them in angle brackets.


Slashes and people who like slashes
-----------------------------------

For those of us who like using slashes in prose: `good/bad/ugly` would get rendered as “good*bad*ugly”, so escape slashes like this: `good\/bad\/ugly`.


Quotes and links to nonexistent modules
---------------------------------------

Haddock thinks that all quoted words are links to other modules, so by using `"This"` in a sentence you might get a link to an unexistent module “This”. Again, escaping helps here. Or using `“”` quotation marks instead of `""` ones. Or simply not taking words starting with a capital letter in quotes.


Code blocks and lambdas
-----------------------

Code blocks (ones surrounded by `@`s) aren't verbatim. This:

    @
    map (\a -> a ++ reverse a) xs
    @

will produce this:

    map (a -> a ++ reverse a) xs

(note the missing slash). The solution once again is escaping.


Code blocks and operators with angle brackets
---------------------------------------------

Even worse, if you dare use `<$>` in a code block, it will turn into a link to a module called “\$”. Either escape angle brackets, or put `<$>` in single quotes.


Code blocks and emphasis
------------------------

This one doesn't pop up often, because operators with `/` in them are pretty rare (there's `/=`, `/`, `//`, but not much else).

    @
    l //= x = l %= (/x)
    @

will produce this (note the emphasis):

<pre><code>l <em>/= x = l %= (</em>x)</code></pre>

Either escape slashes, or put such operators in single quotes.


Code blocks and alignment
-------------------------

Aligning code in code blocks can be tricky. For instance, this nicely aligned code:

    view ::             Getter s a     -> s -> a
    view :: Monoid m => Fold s m       -> s -> m
    view ::             Iso' s a       -> s -> a
    view ::             Lens' s a      -> s -> a
    view :: Monoid m => Traversal' s m -> s -> m

is produced by this Haddock markup:

    'view' ::             'Getter' s a     -> s -> a
    'view' :: 'Data.Monoid.Monoid' m => 'Control.Lens.Fold.Fold' s m       -> s -> m
    'view' ::             'Control.Lens.Iso.Iso'' s a       -> s -> a
    'view' ::             'Lens'' s a      -> s -> a
    'view' :: 'Data.Monoid.Monoid' m => 'Control.Lens.Traversal.Traversal'' s m -> s -> m

(Editing such code, naturally, is a pain.) So, check your alignment in browser every time you modify a piece of code in docs.


Code examples and links to identifiers
--------------------------------------

You can create links to identifiers by wrapping them in single quotes, which works in code blocks and is kinda nice. However, the same thing doesn't work in code examples, so by doing

    >>> (1,2) '&' '_1' '%~' 'negate'
    (-1,2)

you will get what you see, instead of nicer

<pre><code>>>> (1,2) <a href="http://hackage.haskell.org/package/base/docs/Data-Function.html#v:-38-">&</a> <a href="http://hackage.haskell.org/package/lens/docs/Control-Lens-Tuple.html#v:_1">_1</a> <a href="http://hackage.haskell.org/package/lens/docs/Control-Lens-Operators.html#v:-37--126-">%~</a> <a href="http://hackage.haskell.org/package/base/docs/Prelude.html#v:negate">negate</a>
(-1,2)</code></pre>


Links and line wrapping
-----------------------

If your editor wraps lines to some width, and a link happens to be wrapped...

    ...here goes a link: <http://example.com
    just an example>. Hope you liked it.

...it won't be recognised as a link. Watch out for wrapped links, I guess.

% How to Build Your Own Static Site from Matches and Acorns

It is well-known that all site engines and CMSs suck – either they don't have
enough features, or have too many features, or don't support *this*
particular dialect of Markdown you're used to, or written using some fancy
Node.js frameworks and you can't customise them because you don't know
Node.js. Yes, [Ghost][], I'm looking at you.

[Ghost]: https://ghost.org/

This site relies on a couple of programs and shell scripts, but at least I
can do with it whatever the fuck I want. The caveat, of course, is that I can
also only do what I am *able* to do; but it only motivates me to learn more –
or to give up features I don't really need anyway.

The repository for this site is [on Github](@gh:neongreen/artyom.me).

# Page generation

All pages are static HTML files without extensions (so that this page would
have URL `artyom.me/inside` and not `artyom.me/inside.html`).

## Pandoc

To generate HTML from Markdown, I use [Pandoc][]. It is written in Haskell,
it can generate tables of contents, footnotes, has syntax highlighting and
MathJax support, recognizes practically all markup formats, and generally is
awesome.

[Pandoc]: http://johnmacfarlane.net/pandoc/ 

## Template

I use my own template for Pandoc. It can be found [here][page.template].
Basically, Pandoc generates bare HTML from Markdown and then it
uses the template to complete the page, along with some modifications
depending on variables.

### Unfinished vs. finished

~~~ html
<div style="background-color:$if(unfinished)$ #eb2142 $else$ #19e672 $endif$"...
~~~

I can supply variables to Pandoc – for instance, this metadata block

~~~~
---
unfinished: true
---
~~~~

at the top of the Markdown source declares a variable called
`unfinished`. Pandoc's `$if(unfinished)$ foo $else$ bar $endif$` means that
it will insert “foo” if `unfinished` is present and “bar” otherwise (actual
value of `unfinished` doesn't matter). In this case I'm using `unfinished` to
change the color of page header from green to red.

### Last modified at

~~~ html
This site was last updated on <b>$today$</b>
~~~

Here Pandoc will splice value of variable `today` into the file (I used to
also provide hour and minute of last modification, but these are gone now as
it forces all files to change *all* the time I update the site, not just the
first time I do it on some particular day – and having to wait for all files
to be uploaded every single time I want to fix a typo is painful). This
variable is passed to Pandoc in generating script; more on that later.

### View source

~~~ html
<a href="https://github.com/neongreen/artyom.me/blob/master/$src$">view source</a>
~~~

Again, `src` is passed to Pandoc.

### Next in series

When I include a `series` variable into file, Pandoc creates a box at the end
of page which contains links to previous and next pages in series, as well as
a link to “table of contents” or the first page in series or something.

The `series` block looks like this:

~~~~
---
series:
  top: “Learning Racket” series
  toplink: /#racket
  next: /learning-racket-2
---
~~~~

and it is handled by this piece of template file:

~~~ html
$if(series.top)$
<div id="series">
  $if(series.prev)$
    <a href="$series.prev$">&lt;&lt;&lt;</a>
  $else$
    <span class="gray"><a href="$series.toplink$">&lt;&lt;&lt;</a></span>
  $endif$
  
  <a href="$series.toplink$">$series.top$</a>

  $if(series.next)$
    <a href="$series.next$">&gt;&gt;&gt;</a>
  $else$
    <span class="gray"><a href="$series.toplink$">&gt;&gt;&gt;</a></span>
  $endif$
</div>
$endif$
~~~

When `series.next` or `series.prev` are not defined, they're replaced
with `top` links.

## CSS

You can see the file [here][css.css] (it's not minimised). I didn't know
how to write CSS, so I mostly stole bits and pieces from other places.

### Custom classes

I have a few custom classes. For instance, wrapping a piece of Markdown into
`<div class="note"> ... </div>` results in this:

<div class="note">

This is a sidenote (which isn't actually a sidenote but whatever).

</div>

This happens due to this piece of CSS:

~~~ css
.note {
    background: #CCE8FF;
    padding: 1px 1em;
    margin-left: 0;
    margin-right: 0;
    border: 1px solid #80C6FF; }

.note p {
    margin: 1em 0; }
~~~

In the same vein, making text “ghosted” (gray text, gray links) is done by

~~~ css
.gray                 {color: #AAA;}
.gray a:link          {color: #888;}
.gray a:visited       {color: #777;}
~~~

### Tables

For tables I use this style:

~~~ css
th, td {
    padding: 1px 8px; }

table {
    margin-bottom: 1em;
    width: 100%;
    margin-left: auto;
    margin-right: auto; }

table, th, td {
    border: 0.5px solid grey;
    border-spacing: 0; }
~~~

Because of `width: 100%`, tables are full-width by default. To make a table
narrow, I wrap it into `<div class="autowidth"> ... </div>`, which is defined
as

~~~ css
.autowidth > * {
    width: auto; }
~~~

In this example, `> *` gives attribute `width: auto` to all children which
are exactly one level deep.

# Bells and whistles

## Typo correction

As you know (hopefully), you can select any text on this page, press Ctrl-Enter and it will be reported to me. This is done using [Orphus][]. They provide a script to insert into your page and it will do everything automatically, but this script is a) minimised, and b) doesn't work without an Orphus image on the page (and it even checks dimensions of the image to prevent 1×1 transparent images), which is why I [modified](@gh:neongreen/orphus.js) it a bit.

[Orphus]: http://orphus.ru/en

Since ~~you~~ most people ~~can't~~ would probably have trouble pressing
Ctrl-Enter on a phone or tablet, I made the script export its reporting
procedure and added a button calling it. Here's the code for the button:

~~~ html
<button type="button" onclick="orphus.reportSelected();"
        style="padding:0.2em">this button</button>
~~~

and here's how to make a script export a function:

~~~ javascript
orphus = (function () {

  // ... all code ...

  return {
    reportSelected: reportSelected };
})();
~~~

## Site search

Not present yet. If you know of a good one (instant search, preferably
server-side), let me know.

## Statistics

One word: [Piwik][]. It's really simple to install and maintain, but if you
can't install anything on your server, you could also use
[Google Analytics][GA].

[Piwik]: http://piwik.org/
[GA]: http://google.com/analytics/

By the by, I [don't hide my stats](/stats).

# Scripts

## Generation

Page and feed generation is done by [this][generate.hs] script, which I run
on my laptop. First it queries current date, and then for each file with
`.md` extension it runs Pandoc, passing to it variables `src` (name of file
being processed) and `today`.

You need [pandoc-contrib](@gh:aelve/pandoc-contrib) if you want [shortcut links](@gh:aelve/shortcut-links) to work.

RSS feed is generated from a [description file][feed.feed] (it simply `read`s
a value corresponding to an RSS feed – perhaps I should switch to JSON or
YAML, tho).

## Uploading

Uploading is done by [this][upload.sh] script, which is currently just one
line:

~~~ bash
rsync --exclude '.git' -PLcr . root@artyom.me:/var/artyom.me/
~~~

# Why matches and acorns?

Did you know that in USSR everything – from [children's toys][toys] to
[space rockets][rockets] – was made out of matches and acorns? No? Well, now
you do.

[toys]: http://i.imgur.com/FdVbeVN.jpg
[rockets]: http://i.imgur.com/xpo2IT3.jpg

[page.template]: https://github.com/neongreen/artyom.me/blob/master/page.template
[upload.sh]: https://github.com/neongreen/artyom.me/blob/master/upload.sh
[generate.hs]: https://github.com/neongreen/artyom.me/blob/master/generate.hs
[css.css]: https://github.com/neongreen/artyom.me/blob/master/css.css
[feed.feed]: https://github.com/neongreen/artyom.me/blob/master/feed.feed

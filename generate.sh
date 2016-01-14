#!/bin/sh

today=$(date -u "+%B %-d, %Y")

shortcut_links=$HOME/code/pandoc-contrib/.cabal-sandbox/bin/pandoc-shortcut-links

for f in `find -name "*.md"` 
do
  if [[ $f == flow/* ]]; then
    continue
  fi

  outf=${f%.*}
  echo "converting $f"

  pandoc -f markdown -t html5 --mathjax -o $outf -V src:"$f" -V today:"$today" --template=page.template --standalone --css /css.css --filter $shortcut_links "$f"
done

pandoc -f markdown -t latex -V links-as-notes -V geometry:margin=1.5in -o cv.pdf --filter $shortcut_links cv.md

pandoc -f markdown -t html5 -o cv-plain.html --standalone --filter $shortcut_links cv.md

echo "––––––––––"

runghc genrss.hs feed.feed

echo "––––––––––"

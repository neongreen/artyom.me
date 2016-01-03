#!/bin/sh

today=$(date -u "+%B %-d, %Y")

for f in `find -name "*.md"` 
do
  if [[ $f == flow/* ]]; then
    continue
  fi

  outf=${f%.*}
  echo "converting $f"

  pandoc -f markdown -t html5 --mathjax -o $outf -V src:"$f" -V today:"$today" --template=page.template --standalone --css /css.css --filter ~/code/pandoc-contrib/dist/build/pandoc-shortcut-links/pandoc-shortcut-links "$f"
done

pandoc -f markdown -t latex -V links-as-notes -V geometry:margin=1.5in -o cv.pdf --filter ~/code/pandoc-contrib/dist/build/pandoc-shortcut-links/pandoc-shortcut-links cv.md

pandoc -f markdown -t html5 -o cv-plain.html --standalone --filter ~/code/pandoc-contrib/dist/build/pandoc-shortcut-links/pandoc-shortcut-links cv.md

echo "––––––––––"

runghc genrss.hs feed.feed

echo "––––––––––"

.RECIPEPREFIX +=

# default: generate

build:
  stack build

generate:
  rm -rf output/
  mkdir output/
  stack exec generate

assemble:
  cp -R assets/* output/
  cp -R html/* output/

serve:
  (cd output; node ../serve.js)

upload:
  rsync -PLcr output/ artyom.me:/var/artyom.me/

index: index.md
  stack exec -- generate index

watch:
  echo -e "assets/css.css\npage.template" | entr sh -c 'make index assemble'

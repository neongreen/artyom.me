.RECIPEPREFIX +=

# default: generate

build:
  stack build

generate:
  rm -rf output/
  mkdir output/
  stack exec generate
  cp -R assets/* output/
  cp -R html/* output/

serve:
  (cd output; node ../serve.js)

upload:
  rsync -PLcr output/ artyom.me:/var/artyom.me/

.RECIPEPREFIX +=

# default: generate

build:
  stack build

generate:
  stack exec generate

serve:
  node serve.js

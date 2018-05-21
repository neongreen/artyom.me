.RECIPEPREFIX +=

# default: generate

build:
  stack build

generate:
  stack exec generate

serve:
  node serve.js

upload:
  rsync --exclude '.git' --exclude '.stack-work' -PLcr . artyom.me:/var/artyom.me/

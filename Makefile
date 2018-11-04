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
	rsync -PLcr assets/ artyom.me:/var/artyom.me/

index: index.md
	stack exec -- generate index

watch:
	printf "assets/css.css\npage.template\n" | entr sh -c 'make index assemble'

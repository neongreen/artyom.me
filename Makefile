build:
	stack build

generate:
	rm -rf output/
	mkdir output/
	stack exec generate

assemble:
	cp -R assets/* output/
	# cp -R html/* output/

index: index.md
	stack exec -- generate index

watch:
	printf "assets/css.css\npage.template\n" | entr sh -c 'make index assemble'

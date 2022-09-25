.PHONY: all format build test lint clean 

all: format build test lint

format:
	@fourmolu -q -i src/*.hs test/*.hs

build: 
	@stack build --fast --pedantic

test:
	@stack test

lint:
	@hlint src/*.hs test/*.hs

clean:
	@stack purge

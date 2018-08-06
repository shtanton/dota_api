dev: build run

build:
	dune build src/main.exe

run:
	./_build/default/src/main.exe

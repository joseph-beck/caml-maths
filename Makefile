DUNE ?= dune

build:
	$(DUNE) build

run: build
	$(DUNE) exec mathsparser

docs:
	$(DUNE) build @doc

fmt:
	$(DUNE) fmt

.PHONY: build run

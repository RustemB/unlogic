all: build

build:
	@elm make src/Main.elm --optimize

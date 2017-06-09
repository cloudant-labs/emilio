
all: script


script:
	@rebar compile escriptize


check:
	./emilio -c emilio.config src/

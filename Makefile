
all: script


script:
	@rebar compile escriptize


check: script
	./emilio -c emilio.config src/


.PHONY: script

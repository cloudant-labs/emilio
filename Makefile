
all: script


script:
	@rebar compile escriptize
	@./emilio -c emilio.config .

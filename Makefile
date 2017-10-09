
all: script


script:
	@rebar compile escriptize


check: script
	./emilio -c emilio.config src/
	./emilio -w priv/documentation/whitelist priv/documentation


whitelist: script
	./emilio -f csv priv/documentation | tail -n +2 \
		> priv/documentation/whitelist; true


clone-projects:
	@cd test && ./clone-projects.sh


check-projects: script
	@cd test && ./check-projects.sh


.PHONY: script check-projects clone-projects

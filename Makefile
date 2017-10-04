
all: script


script:
	@rebar compile escriptize


check: script
	./emilio -c emilio.config src/


clone-projects:
	@cd test && ./clone-projects.sh


check-projects: script
	@cd test && ./check-projects.sh


.PHONY: script check-projects clone-projects

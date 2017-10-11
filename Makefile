
all: script


script:
	@rebar compile escriptize


check: script
	./emilio -c emilio.config src/
	./emilio -w priv/documentation/whitelist priv/documentation
	./bin/check-whitelist priv/documentation/whitelist


readme: script
	./bin/generate-readme > README.md


whitelist: script
	./emilio -f csv priv/documentation | tail -n +2 \
		> priv/documentation/whitelist; true


clone-projects:
	@mkdir -p test
	@cd test && ../bin/clone-projects.sh


check-projects: script
	@mkdir -p test
	@cd test && ../bin/check-projects.sh


.PHONY: script readme whitelist check-projects clone-projects

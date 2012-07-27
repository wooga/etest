REBAR=which rebar || ./rebar

all: clean compile

compile:
	$(REBAR) get-deps compile

clean:
	$(REBAR) clean

test: compile
	./bin/etest-runner

.PHONY: all compile clean test


.PHONEY: rel

all: deps compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean
test: compile
	./rebar eunit app=emetric
	./rebar ct

distclean: clean relclean
	./rebar delete-deps

rel: deps compile
	./rebar escriptize

relclean:
	rm -fr emetric



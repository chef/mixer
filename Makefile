all: compile eunit

clean:
	@./rebar clean

compile:
	@./rebar compile

eunit:
	@./rebar eunit
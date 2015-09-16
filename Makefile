all: compile

clean:
	@./rebar clean

compile:
	@./rebar compile

eunit: compile
	@./rebar eunit

dialyzer: compile
	@dialyzer --src src

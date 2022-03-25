all: compile eunit dialyzer

clean:
	@./rebar3 clean

compile:
	@./rebar3 compile

eunit:
	@./rebar3 eunit

dialyzer:
	@dialyzer --src src

.PHONY = all shell

all:
	./rebar3 do dialyzer, eunit, ct

shell:
	./rebar3 shell

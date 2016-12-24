.PHONY = all shell

all:
	./rebar3 do dialyzer, eunit, cover -v

shell:
	./rebar3 shell

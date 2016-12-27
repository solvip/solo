.PHONY = all shell

all:
	./rebar3 do dialyzer, eunit, cover -v

shell:
	./rebar3 shell

bench:
	./rebar3 escriptize
	./_build/default/bin/solo_bench

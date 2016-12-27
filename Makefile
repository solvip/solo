.PHONY = all shell

all:
	./rebar3 do dialyzer, eunit, cover -v

shell:
	./rebar3 shell

escriptize:
	./rebar3 escriptize

bench: escriptize
	./_build/default/bin/solo_bench

profile: escriptize
	./_build/default/bin/solo_bench profile

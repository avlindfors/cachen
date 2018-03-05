.PHONY: rel compile start clean

rel:
	rebar3 release && _build/default/rel/cachen/bin/cachen

compile:
	rebar3 compile

start:
	_build/default/rel/cachen/bin/cachen

clean:
	rm -rf _build/

REBAR=$(shell which rebar || echo ./rebar)
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

all: compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://raw.github.com/wiki/rebar/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) get-deps

test: compile
	@$(REBAR) xref eunit recursive=false

examples: eg
eg:
	@erlc -I deps -o ebin examples/*.erl

.PHONY: test dialyzer typer

$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib crypto compile -r deps

dialyzer: $(DEPSOLVER_PLT)
	dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src

typer: $(DEPSOLVER_PLT)
	typer --plt $(DEPSOLVER_PLT) -r ./src

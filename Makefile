DIALYZER?=dialyzer
TYPER?=typer
REBAR?=rebar
DEP_PLT?=${PWD}/.dep_plt

DIALYZER_W?= -Wunmatched_returns \
             -Werror_handling \
             -Wrace_conditions \
             -Wunderspecs

all: compile

release: compile
	@${REBAR} generate

compile: deps
	@${REBAR} compile

deps:
	@${REBAR} get-deps

clean:
	@${REBAR} clean

${DEP_PLT}:
	ERL_LIBS=${PWD}/lib @${DIALYZER} --output_plt ${DEP_PLT} --build_plt \
		--apps erts kernel stdlib crypto procket

dialyzer: ${DEP_PLT}
	@${DIALYZER} --plt ${DEP_PLT} ${DIALYZER_W} --src lib/erl_ocat/src

typer: ${DEP_PLT}
	@${TYPER} --plt ${DEP_PLT} -r lib/erl_ocat/src

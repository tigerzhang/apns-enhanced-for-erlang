REBAR = ./rebar -j8
all: deps compile

compile: deps
	${REBAR} compile

deps:
	${REBAR} get-deps

clean:
	${REBAR} clean

generate: compile
	cd rel && ../${REBAR} generate -f

relclean:
	rm -rf rel/apns

run: generate
	./rel/apns/bin/apns start

console: generate
	./rel/apns/bin/apns console

erl: compile
	erl -pa ebin/ -pa lib/*/ebin/ -s apns

.PHONY: deps compile generate

RUN := +Bc +K true -smp enable -pa ebin -sname apnsmng -s crypto -s inets -s ssl

all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	rebar dialyze

update-deps:
	rebar update-deps

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

debug: all
	erl  -boot start_sasl ${RUN} -s apns -s appmon -s debugger

run: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN} -s apns;\
	else\
		erl  -boot start_sasl ${RUN} -s apns;\
	fi

shell: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN};\
	else\
		erl  -boot start_sasl ${RUN};\
	fi

test: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run apns_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run apns_tests main;\
	fi

test_mng_cases: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run apns_manager_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run apns_manager_tests main;\
	fi

test_tcp: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run tcp_server_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run tcp_server_tests main;\
	fi

run_mng: all
	if [ -f `hostname`.config ]; then \
		erl -noinput -config `hostname` ${RUN} -run apns_manager_app start; \
	else \
		erl -noinput ${RUN} -run apns_manager_app start; \
	fi

test_mng: all test_mng_cases
	erl -boot start_sasl +Bc +K true -smp enable -pa ebin -s crypto -s inets -s ssl -s appmon -s apns -sname test_mng -detached
	sleep 10
	./test_data.sh localhost 1
	sleep 60
	kill `pgrep -f "beam.*-sname test_mng"`

.PHONY: test_mng run

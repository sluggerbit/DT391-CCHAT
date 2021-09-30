all: client.erl server.erl lib/cchat.erl lib/dummy_gui.erl lib/genserver.erl lib/grm.erl lib/gui.erl lib/lex.erl lib/lexgrm.erl lib/test_client.erl
	make -C lib
	erl -compile client.erl server.erl lib/cchat.erl lib/dummy_gui.erl lib/genserver.erl lib/grm.erl lib/gui.erl lib/lex.erl lib/lexgrm.erl lib/test_client.erl

clean:
	rm -f *.beam *.dump

run_tests: all
	erl -noshell -eval "eunit:test(test_client), halt()"

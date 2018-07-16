source = $(wildcard *.erl)
beams = $(patsubst %.erl,%.beam,$(source))
test_modules = $(patsubst %_tests.erl,%_tests,$(wildcard *_tests.erl))

run: $(beams)
	@erl -s rpn start -s init stop -noshell

$(beams): %.beam: %.erl
	erlc $<

eunit: $(beams)
	$(foreach test_module,$(test_modules),erl -s $(test_module) test -s init stop -noinput;)

clean:
	rm -f *.beam

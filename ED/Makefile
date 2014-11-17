
NODENAME = ed@127.0.0.1

compile:
	mkdir ebin 2>/dev/null
	erl -make

clean:
	rm -rf ./ebin/*.beam

start:
	erl -smp -name "$(NODENAME)" -pa ebin -setcookie chocolate -s make all \
			-eval "application:start(ed)"
noshell:
	erl -smp -noshell -name "$(NODENAME)" -pa ebin -setcookie chocolate -s make all \
			-eval "application:start(ed)"

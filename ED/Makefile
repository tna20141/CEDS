
NODENAME = ed@127.0.0.1

compile:
	erl -make

clean:
	rm -rf ./ebin/*.beam

start:
	erl -smp +K true +P 1000000 -name "$(NODENAME)" -pa ebin -setcookie chocolate -s make all \
			-eval "application:start(ed)"


NODENAME = ed

compile:
	erl -make

clean:
	rm -rf ./ebin/*.beam

start:
	erl -smp +K true +P 1000000 -sname $(NODENAME) -pa ebin -setcookie chocolate -s make all \
			-eval "application:start(ed)"

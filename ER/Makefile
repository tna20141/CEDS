compile:
	erl -make

clean:
	rm -rf ./ebin/*.beam

start:
	erl -smp +K true +P 1000000 -sname er -pa ebin -setcookie chocolate -s make all \
			-eval "application:start(er)"
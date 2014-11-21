
NODENAME = sim_1@192.168.7.40
TONODE = room_1@192.168.7.40
USECEDS = true
NUM = 2

compile:
	mkdir ebin 2>/dev/null
	erl -make

clean:
	rm -rf ./ebin/*.beam

start:
	erl -smp -name "$(NODENAME)" -pa ebin -setcookie chocolate -s make all \
		-eval "sensors:start_link(['$(TONODE)', $(USECEDS), $(NUM)])"

noshell:
	erl -smp -noshell -name "$(NODENAME)" -pa ebin -setcookie chocolate -s make all \
		-eval "sensors:start_link(['$(TONODE)', $(USECEDS), $(NUM)])"

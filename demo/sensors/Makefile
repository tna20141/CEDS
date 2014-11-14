
NODENAME = temperature_1@192.168.7.40
TONODE = room_1@192.168.7.40

compile:
	mkdir ebin 2>/dev/null
	erl -make

clean:
	rm -rf ./ebin/*.beam

start:
	erl -smp +K true +P 100000 -name "$(NODENAME)" -pa ebin -setcookie chocolate -s make all \
		-eval "temperature_sensor:start_link('$(TONODE)'),humidity_sensor:start_link('$(TONODE)')"


NODENAME = sim_1@192.168.7.40
TONODE = room_1@192.168.7.40
USECEDS = true

compile:
	mkdir ebin 2>/dev/null
	erl -make

clean:
	rm -rf ./ebin/*.beam

sensor_start:
	erl -smp -name "$(NODENAME)" -pa ebin -setcookie chocolate -s make all \
		-eval "temperature_sensor:start_link(['$(TONODE)', $(USECEDS)]),humidity_sensor:start_link(['$(TONODE)', $(USECEDS)])"

sensor_noshell:
	erl -smp -noshell -name "$(NODENAME)" -pa ebin -setcookie chocolate -s make all \
		-eval "temperature_sensor:start_link(['$(TONODE)', $(USECEDS)]),humidity_sensor:start_link(['$(TONODE)', $(USECEDS)])"

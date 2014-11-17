#!/bin/bash

num=$1
local_ip=$2
ed_ip=$3

for i in {1..$num}; do
	ed_name=room_master@$ed_ip
	sensor_name=condition_$i@$local_ip

	cd demo/sensors
	make start NODENAME="$sensor_name" TONODE="$ed_name"
done

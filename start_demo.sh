#!/bin/bash

num=$1
ip=$2

cwd=$PWD

for ((i = 1; i < $num+1; i++)); do
	ed_name=room_$i@$ip
	sensor_name=condition_$i@$ip

	cd ED
	make noshell NODENAME="$ed_name" &

	sleep 0.1

	cd $cwd

	cd demo/sensors
	make noshell NODENAME="$sensor_name" TONODE="$ed_name" &

	sleep 0.1

	cd $cwd
done

sleep 1000

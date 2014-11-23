#!/bin/bash

num=$1
ip=$2

cwd=$PWD
ed_base_name=room_

for ((i = 1; i < $num+1; i++)); do
	ed_name=${ed_base_name}$i@$ip
	cd ED
	make noshell NODENAME="$ed_name" &

	sleep 0.1
	cd $cwd
done

cd demo/sensors
make noshell NODENAME="sensor@$ip" TONODE="$ed_base_name@$ip" NUM=$1

sleep infinity

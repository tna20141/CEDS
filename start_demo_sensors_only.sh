#!/bin/bash

num=$1
ip=$2
to_ip=$3

cwd=$PWD
ed_name=room_all@$to_ip

cd demo/sensors
make noshell NODENAME="sensor@$ip" TONODE="$ed_name" NUM=$1 USECEDS="false"

sleep infinity

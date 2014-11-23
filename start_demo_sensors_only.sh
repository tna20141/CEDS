#!/bin/bash

num=$1
ip=$2

cwd=$PWD
ed_name=room_all@$ip

cd demo/sensors
make noshell NODENAME="sensor@$ip" TONODE="$ed_name" NUM=$1 USECEDS="false"

sleep infinity

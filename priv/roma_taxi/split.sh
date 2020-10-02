#!/bin/bash
z=1
while [ $z -lt 400 ]; do
    echo "Building $z.txt..."
    grep "^$z;" taxi_february.txt > $z.txt
    z=$(($z+1))
done

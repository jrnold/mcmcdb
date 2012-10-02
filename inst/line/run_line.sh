#!/bin/bash
make
for i in {1..2}
do
    echo $i
    ./line.stanx --data=data.R --iter=200 --samples=chain_$i.csv
done

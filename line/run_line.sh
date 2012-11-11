#!/bin/bash
make
for i in {1..2}
do
    echo $i
    ./line.stanx --data=data.R --seed=1234567  --warmup=2000 --iter=2200 --samples=chain_$i.csv
done

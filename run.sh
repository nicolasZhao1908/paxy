#!/bin/bash

sleep_prop1=(100)
sleep_prop2=(100)
sleep_prop3=(100)
acceptor_delays=(5900 6000 6100 6200 6300 6400)

erl -make

for i in "${sleep_prop1[@]}"; do
    for j in "${sleep_prop2[@]}"; do
        for k in "${sleep_prop3[@]}"; do
            for d in "${acceptor_delays[@]}"; do
                echo $i, $j, $k, $d;
                filename=paxy_sleep_"$i"_"$j"_"$k"_delay"$d".out;
                export delay_shell=$d; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > experiments/$filename & pid=$!; sleep 100; kill $pid
                result=$(grep "Total" experiments/$filename | awk -F '[:]' '{print $2}')
                echo $i, $j, $k, $d, $result >> experiments/clean
            done
        done
    done
done

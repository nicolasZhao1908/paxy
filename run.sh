#!/bin/bash

sleep_prop1=(100)
sleep_prop2=(100)
sleep_prop3=(100)
acceptor_delays=(2300 2400 2500 2600 2700 2800 2900 3000 3100 3200 3300)
drop_value=(1 2 3 4 5 6 7 8 9)

erl -make

for i in "${sleep_prop1[@]}"; do
    for j in "${sleep_prop2[@]}"; do
        for k in "${sleep_prop3[@]}"; do
#            for d in "${acceptor_delays[@]}"; do
#                echo $i, $j, $k, $d;
#                filename=paxy"$i"_"$j"_"$k"_delay"$d".out;
#                export delay_shell=$d; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > experiment1/$filename & pid=$!; sleep 35; kill $pid
#                result=$(grep "Total" experiment1/$filename | awk -F '[:]' '{print $2}')
#                echo $i, $j, $k, $d, $result >> experiment1/clean
#            done
            for r in "${drop_value[@]}"; do
                echo $i, $j, $k, $r;
                filename=paxy"$i"_"$j"_"$k"_drop"$r".out;
                export drop=$r; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > experiment1/$filename & pid=$!; sleep 100; kill $pid
                result=$(grep "Total elapsed time" experiment1/$filename | awk -F '[:]' '{print $2}')
                echo drop, $i, $j, $k, $r, $result >> experiment1/clean
            done
        done
    done
done

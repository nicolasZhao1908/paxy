#!/bin/bash

sleep_prop1=(100)
sleep_prop2=(100)
sleep_prop3=(100)
acceptor_delays=(2300 2400 2500 2600 2700 2800 2900 3000 3100 3200 3300)
timeouts=(1900 1800 1700 1600 1500 1400 1300 1200 1100 1000 900 800 700 600 500 400 300)
drop_value=(1 2 3 4 5 6 7 8 9)

directory=experiments
proposers=(1 2 3 4 5 6 7 8 9 10)
acceptors=(1 2 3 4 5 6 7 8 9 10)

erl -make

for i in "${sleep_prop1[@]}"; do
    for j in "${sleep_prop2[@]}"; do
        for k in "${sleep_prop3[@]}"; do
#            for d in "${acceptor_delays[@]}"; do
#                echo $i, $j, $k, $d;
#                filename=paxy"$i"_"$j"_"$k"_delay"$d".out;
#                export delay_shell=$d; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > $directory/$filename & pid=$!; sleep 35; kill $pid
#                result=$(grep "Total" $directory/$filename | awk -F '[:]' '{print $2}')
#                echo $i, $j, $k, $d, $result >> $directory/clean
#            done
#            for r in "${drop_value[@]}"; do
#                echo $i, $j, $k, $r;
#                filename=paxy"$i"_"$j"_"$k"_drop"$r".out;
#                export drop=$r; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > $directory/$filename & pid=$!; sleep 100; kill $pid
#                result=$(grep "Total elapsed time" $directory/$filename | awk -F '[:]' '{print $2}')
#                echo drop, $i, $j, $k, $r, $result >> $directory/clean
#            done
            #for r in "${timeouts[@]}"; do
            #    echo $i, $j, $k, $r;
            #    filename=paxy"$i"_"$j"_"$k"_timeout"$r".out;
            #    export timeout=$r; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > $directoy/$filename & pid=$!; sleep 100; kill $pid
            #    result=$(grep "Total elapsed time" $directoy/$filename | awk -F '[:]' '{print $2}')
            #    echo timeout, $i, $j, $k, $r, $result >> $directoy/clean
            #done
            mkdir -p $directory/accep_prop
            for p in "${proposers[@]}"; do
                for a in "${acceptors[@]}"; do
                echo $p proposers $a acceptors;
                filename=paxy_"$p"_prop_"$a"_accept.out;
                export proposer=$p; export acceptor=$a; erl -noshell -pa ebin -eval "paxy:measure()" > $directory/accep_prop/$filename & pid=$!; sleep 100; kill $pid
                round=$(grep "DECIDED" $directory/accep_prop/$filename | tail -n 1 | awk -F"round {|,'" '{print $2}')
                echo $p Proposers $a Acceptors: $round Rounds >> $directory/accep_prop/summary
                done
            done
        done
    done
done

#!/bin/bash

#sleep_prop1=(100)
#sleep_prop2=(100)
#sleep_prop3=(100)
#drop_value=(1 2 3 4 5 6 7 8 9)
#iterations=10


#for i in "${sleep_prop1[@]}"; do
#    for j in "${sleep_prop2[@]}"; do
#        for k in "${sleep_prop3[@]}"; do
#            for r in "${drop_value[@]}"; do
#                echo $i, $j, $k, $r;
#                filename="drop_$i"_"$j"_"$k"_drop_"$r".out;
#                export drop=$r; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > experiments/$filename & pid=$!; sleep 100; kill $pid
#                result=$(grep "Total elapsed time" experiments/$filename | awk -F '[:]' '{print $2}')
#                echo drop, $i, $j, $k, $r, $result >> experiments/summary
#            done
#        done
#    done
#done


measure_drop(){
    erl -make
    iterations=10
    drop_values=(10 20 30 40 50 60 70 80 90)
    output_dir=experiments/drops
    mkdir -p $output_dir
    echo $r drop $it iter;
    for r in "${drop_values[@]}"; do
        for it in $(seq 1 $iterations); do
            filename=drop_"$r"."$it".out;
            export drop=$r; erl -noshell -pa ebin -eval "paxy:start([100, 100, 100])" > $output_dir/$filename & pid=$!; sleep 100; kill $pid
            round=$(grep "DECIDED" $output_dir/$filename | tail -n 1 | awk -F"round {|,'" '{print $2}')
            echo Drop $r: $round >> $output_dir/summary
        done
    done
}

measure_delays(){
    erl -make
    iterations=10
    acceptor_delays=(2300 2400 2500 2600 2700 2800 2900 3000 3100 3200 3300 3400 3500 3600 3700 3800 3900 4000 4100 4200 4300 4400 4500 4600 4700 4800 4900 5000 5100 5200 5300 5400 5500 5600 5700 5800 5900 6000 6100 6200)
    output_dir=experiments/delays
    mkdir -p $output_dir
    for d in "${acceptor_delays[@]}"; do
        for it in $(seq 1 $iterations); do
            echo 2000 timeout $d delay $it iter;
            filename=delay_"$d"."$it".out;
            export delay=$d; erl -noshell -pa ebin -eval "paxy:start([100, 100, 100])" > $output_dir/$filename & pid=$!; sleep 100; kill $pid
            round=$(grep "DECIDED" $output_dir/$filename | tail -n 1 | awk -F"round {|,'" '{print $2}')
            echo Delay $d: $round >> $output_dir/summary
        done
    done
}

measure_timeouts(){
    erl -make
    iterations=10
    timeouts=(3000 2900 2800 2700 2600 2500 2400 2300 2200 2100 2000 1900 1800 1700 1600 1500 1400 1300 1200 1100 1000 900 800 700 600 500 400 300)
    output_dir=experiments/timeouts
    mkdir -p $output_dir
    for r in "${timeouts[@]}"; do
        for it in $(seq 1 $iterations); do
            echo $r timeout and 2000 delay $it iter;
            filename=timeout_"$r"."$it".out;
            export timeout=$r; erl -noshell -pa ebin -eval "paxy:start([100, 100, 100])" > $output_dir/$filename & pid=$!; sleep 100; kill $pid
            round=$(grep "DECIDED" $output_dir/$filename | tail -n 1 | awk -F"round {|,'" '{print $2}')
            echo Timeout $r: $round >> $output_dir/summary
        done
    done
}

measure_accept_vs_prop(){
    erl -make
    iterations=10
    proposers=(1 2 3 4 5 6 7 8 9 10)
    acceptors=(1 2 3 4 5 6 7 8 9 10)
    output_dir=experiments/accep_prop
    mkdir -p $output_dir
    for p in "${proposers[@]}"; do
        for a in "${acceptors[@]}"; do
            for it in $(seq 1 $iterations); do
                echo $p proposers $a acceptors $it iter;
                filename=prop_"$p"_accept_"$a"."$it".out;
                export proposer=$p; export acceptor=$a; erl -noshell -pa ebin -eval "paxy:measure()" > $output_dir/$filename & pid=$!; sleep 100; kill $pid
                round=$(grep "DECIDED" $output_dir/$filename | tail -n 1 | awk -F"round {|,'" '{print $2}')
                echo $p Proposers $a Acceptors: $round Rounds >> $output_dir/summary
            done
        done
    done
}

#measure_drop
#measure_delays
#measure_timeouts
measure_accept_vs_prop

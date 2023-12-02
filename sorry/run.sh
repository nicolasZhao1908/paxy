#!/bin/bash

measure_delays(){
    erl -make
    iterations=3
    acceptor_delays=(2300 2400 2500 2600 2700 2800 2900 3000 3100 3200 3300 3400 3500 3600 3700 3800 3900 4000 4100 4200 4300 4400 4500 4600 4700 4800 4900 5000 5100 5200 5300 5400 5500 5600 5700 5800 5900 6000 6100 6200)
    output_dir=experiments/delays
    mkdir -p $output_dir
    for d in "${acceptor_delays[@]}"; do
        for it in $(seq 1 $iterations); do
            echo 2000 timeout $d delay $it iter;
            filename=delay_"$d"."$it".out;
            export delay=$d; erl -noshell -pa ebin -eval "paxy:start([100, 100, 100])" > $output_dir/$filename & pid=$!; sleep 10; kill $pid
            time=$(grep "Total elapsed time" $output_dir/$filename | tr -dc '0-9')
            round=$(grep "LAST ROUND" $output_dir/$filename | grep "LAST ROUND" | awk '{print $NF}' | sort -n | tail -n 1)
            is_finished="NO CONSENSUS"
            count=$(grep -c "LAST ROUND" "$output_dir/$filename")
            if [ $count -ge 3 ]; then
                is_finished="FINISHED"
            fi
            echo Delay $d: $round Rounds in $time ms $is_finished >> $output_dir/summary
        done
    done
    unset delay
}

measure_delays

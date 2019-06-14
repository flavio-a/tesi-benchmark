#!/bin/bash

# Names of benchmarks defined in Main.hs
BENCHMARKS=(queens minimax matmultV coins nbody sphere gatesim transclos)
# BENCHMARKS=(queens matmultV)
# Time to let the processor sleep between build and run
SLEEP=3
# Path to Main.hs
MAIN="app/Main.hs"

RUN=$1

# Backups initial Main.hs file
cp $MAIN $MAIN.bak

mkdir "results/$RUN"

BENCHMARKS+=""
for bench in "${BENCHMARKS[@]}"; do
    echo "------- Starting benchmark $bench -------"
    sed -e "s/main = defaultMain \[  \]/main = defaultMain [ $bench ]/" "$MAIN.bak" > "$MAIN"
    stack build
    sleep $SLEEP
    stack exec tesi-benchmark -- --template json --output "results/$RUN/$bench.json"
    sed 's/&quot;/\"/g' "results/$RUN/$bench.json" > "results/$RUN/$bench.json.tmp"
    mv "results/$RUN/$bench.json"{.tmp,}
    echo "-------------- Finished run --------------"
done

mv "$MAIN"{.bak,}
echo "=================== DONE ==================="

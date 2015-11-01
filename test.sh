#!/bin/bash 

echo "Starting switch test."

./switch/.cabal-sandbox/bin/Switch &> switch.log &
switchpid=$!
echo "Starting switch with PID:$switchpid"

for i in `seq 0 7`;
do
    echo "Starting client $i"
    ./client/.cabal-sandbox/bin/client localhost 4242 $i &> client$i.log &
done

sleep 60s

echo "Killing switch process."
kill $switchpid

echo "The end!"

exit 0

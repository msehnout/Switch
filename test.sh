#!/bin/bash 

echo "Starting switch test."

./.cabal-sandbox/bin/switch &> log/switch.log &
switchpid=$!
echo "Starting switch with PID:$switchpid"

for i in `seq 0 7`;
do
    echo "Starting client $i"
    ./.cabal-sandbox/bin/client localhost 4242 $i &> log/client$i.log &
done

sleep 90s

echo "Killing switch process."
kill $switchpid

echo "The end!"

exit 0

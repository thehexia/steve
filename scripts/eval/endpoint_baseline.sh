# Input file.
input=$PWD/input/bigFlows.pcap

# Flowcap
flowcap_dir=../../build/freeflow/flowcap

echo "Starting 'sink' (netcat)"
/usr/bin/time -f "%e seconds" -o time.txt netcat -l -d 5000 &> /dev/null &

echo "Starting 'source' (flowcap forward)"
# Start the source.
$flowcap_dir/flowcap forward $input 127.0.0.1 5000 10 &

wait

echo "netcat runtime: `cat time.txt`"

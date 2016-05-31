# Input file.
input=$PWD/input/bigFlows.pcap

# Flowcap
flowcap_dir=../../build/freeflow/flowcap

echo "Starting 'sink' (flowcap fetch)"
# Start the sink.
taskset -c 1 $flowcap_dir/flowcap expect $input 127.0.0.1 5001 10 &

mkfifo local
echo "Starting 'wire' (netcat | netcat)"
/usr/bin/time -f "%e seconds" -o wire_in.txt taskset -c 1 netcat -l 5000 | netcat localhost 5001 &> /dev/null & 
#/usr/bin/time -f "%e seconds" -o wire_out.txt taskset -c 1 netcat localhost 5001 < local &> /dev/null &

echo "Starting 'source' (flowcap forward)"
# Start the source.
taskset -c 1 $flowcap_dir/flowcap forward $input 127.0.0.1 5000 10 &

wait
rm local
echo "netcat runtime: `cat wire_in.txt wire_out.txt`"

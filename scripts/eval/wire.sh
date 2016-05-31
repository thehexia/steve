# Input file.
input=$PWD/input/bigFlows.pcap

# Compile the steve app.
echo "Compiling 'lwire' Steve application"
cd ..
./steve_compile wire
app_dir=$PWD/apps/

# Driver.
driver=../build/freeflow/fp-lite/drivers/wire/fp-wire-select-st

# Flowcap
flowcap_dir=../build/freeflow/flowcap

echo "Starting 'wire' app"
# Start the freeflow server running the app.
taskset -c 0 $driver "once" $app_dir &

sleep 1

echo "Starting 'sink' (flowcap fetch)"
# Start the sink.
taskset -c 1 $flowcap_dir/flowcap fetch $input 127.0.0.1 5000 10 &

sleep 1

echo "Starting 'source' (flowcap forward)"
# Start the source.
taskset -c 1 $flowcap_dir/flowcap forward $input 127.0.0.1 5000 10 &

wait

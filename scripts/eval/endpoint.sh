# Input file.
input=$PWD/input/bigFlows.pcap

# Compile the steve app.
echo "Compiling 'endpoint' Steve application"
cd ..
./steve_compile endpoint
app_dir=$PWD/apps/

# Driver.
driver=../build/freeflow/fp-lite/drivers/endpoint/fp-endpoint

# Flowcap
flowcap_dir=../build/freeflow/flowcap

echo "Starting 'sink' (fp-endpoint)"
# Start the freeflow server running the app.
taskset -c 1 $driver "once" $app_dir &

echo "Starting 'source' (flowcap forward)"
# Start the source.
taskset -c 1 $flowcap_dir/flowcap forward $input 127.0.0.1 5000 10 &

wait

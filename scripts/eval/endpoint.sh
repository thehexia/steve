# Input file.
input=$PWD/input/smallFlows.pcap

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
$driver "once" $app_dir &

# Wait for server setup.
sleep 1

echo "Starting 'source' (flowcap forward)"
# Start the source.
$flowcap_dir/flowcap forward $input 127.0.0.1 5000 

wait


#include "buffer.hpp"
#include "system.hpp"
#include "port_table.hpp"
#include "timer.hpp"

using namespace fp;

int main(int argc, char* argv[])
{
  try {
    long long pkt_no = atoll(argv[2]);

    // Goes somewhere, IDC where.
    fp::Port* p1 = fp::create_port(fp::Port::Type::tcp, ":5000", "p1");
    std::cerr << "Created port 'p1' with id '" << p1->id() << "'\n";

    fp::Port* p2 = fp::create_port(fp::Port::Type::tcp, ":5001", "p2");
    std::cerr << "Created port 'p2' with id '" << p2->id() << "'\n";

    assert(p1);

    // Load the application library
    fp::load_application(argv[1]);
    std::cerr << "Loaded application\n";

    // Create the dataplane with the loaded application library.
    fp::Dataplane* dp = fp::create_dataplane("dp1", argv[1]);
    std::cerr << "Created data plane '" << dp->name() << "'\n";

    // Add all ports
    dp->add_port(p1);
    std::cerr << "Added port 'p1' to data plane 'dp1'\n";

    // Configure the data plane based on the applications needs.
    dp->configure();
    std::cerr << "Data plane configured\n";


    dp->up();
    std::cerr << "Data plane is up\n";

    std::cout << "Sending " << pkt_no << " packets.\n";

    { // block

      long long i = 0;
      Timer t;

      Byte* data1 = new Byte[64]{
        // src bytes
        0x12, 0x34, 0x56, 0x78, 0x90, 0xab,
        // dst bytes
        0xab, 0x12, 0x34, 0x56, 0x78, 0x90,
        // type bytes
        0x08, 0x80
      };

      Packet* pkt1 = packet_create(data1, 1500, 0, nullptr, FP_BUF_ALLOC);
      dp->process(p1, pkt1);

      while(i < pkt_no) {

        Byte* data2 = new Byte[64]{
          // src bytes
          0xab, 0x12, 0x34, 0x56, 0x78, 0x90,
          // dst bytes
          0x12, 0x34, 0x56, 0x78, 0x90, 0xab,
          // type bytes
          0x08, 0x00,
          // version IHL bytes
          0,
          // dscp ecn
          0,
          // len
          0, 0x0a,
          // id
          0, 0,
          // frag
          0, 0,
          // ttl,
          0x0a,
          // protocol
          0,
          // checksum
          0, 0,
          // src
          0b11111111, 0b11111111, 0b11111111, 0b11111111,
          // dst
          0b11111111, 0b11111111, 0b11111111, 0b11111011
        };

        Packet* pkt2 = packet_create(data2, 1500, 0, nullptr, FP_BUF_ALLOC);

        dp->process(p1, pkt2);
        ++i;
      }
      // timer dtor should print time here

    } // block
  }
  catch(std::string s)
  {
    std::cout << s;
  }

  std::cout << "Done\n";

  return 0;
}

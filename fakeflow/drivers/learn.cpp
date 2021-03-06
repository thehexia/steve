
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
    assert(p2);

    // Load the application library
    fp::load_application(argv[1]);
    std::cerr << "Loaded application\n";

    // Create the dataplane with the loaded application library.
    fp::Dataplane* dp = fp::create_dataplane("dp1", argv[1]);
    std::cerr << "Created data plane '" << dp->name() << "'\n";

    // Add all ports
    dp->add_port(p1);
    std::cerr << "Added port 'p1' to data plane 'dp1'\n";
    dp->add_port(p2);
    std::cerr << "Added port 'p2' to data plane 'dp1'\n";
    dp->add_reserved_ports();

    // Configure the data plane based on the applications needs.
    dp->configure();
    std::cerr << "Data plane configured\n";

    // Notify the application of ports.
    Application* app = dp->app();
    int p1id = p1->id();
    int p2id = p2->id();
    // app->lib().exec("port_changed", &p1id);
    // app->lib().exec("port_changed", &p2id);

    dp->up();
    std::cerr << "Data plane is up\n";

    std::cout << "Sending " << pkt_no << " packets.\n";


    { // block

      long long i = 0;
      Timer t;

      // Send this to make the table learn this ones src and inport
      Byte data1[1500]{
        // dst bytes
        0x12, 0x34, 0x56, 0x78, 0x90, 0xab,
        // src bytes
        0xab, 0x12, 0x34, 0x56, 0x78, 0x90,
        // type bytes
        0x08, 0x00,
        // version IHL bytes
        // version: 4
        // ihl: 5
        0x45,
        // dscp ecn
        0,
        // len
        0, 0x40,
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
        0b11111111, 0b11111111, 0b11111111, 0b11111011,
        // dst
        0b11111111, 0b11111111, 0b11111111, 0b11111111,
        // udp src
        0, 0,
        // udp dst
        0, 0,
        // udp len
        0x00, 0x0b,
        // checksum
        0, 0
      };

      Packet pkt1(&data1[0], 1500, 0, nullptr, FP_BUF_ALLOC);
      dp->process(p1, &pkt1);

      while(i < pkt_no) {

        // The dst address for this packet should be learned by now.
        Byte data2[1500]{
          // dst bytes
          0xab, 0x12, 0x34, 0x56, 0x78, 0x90,
          // src bytes
          0x12, 0x34, 0x56, 0x78, 0x90, 0xab,
          // type bytes
          0x08, 0x00,
          // version IHL bytes
          // version: 4
          // ihl: 5
          0x45,
          // dscp ecn
          0,
          // len
          0, 0x40,
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
          0b11111111, 0b11111111, 0b11111111, 0b11111011,
          // udp src
          0, 0,
          // udp dst
          0, 0,
          // udp len
          0x00, 0x0b,
          // checksum
          0, 0
        };

        Packet pkt2(&data2[0], 1500, 0, nullptr, FP_BUF_ALLOC);

        dp->process(p2, &pkt2);
        ++i;
      }

      // Send a second copy of the first packet to confirm the second packet
      // caused the first src to have been learned.
      pkt1 = Packet(&data1[0], 1500, 0, nullptr, FP_BUF_ALLOC);
      dp->process(p1, &pkt1);
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

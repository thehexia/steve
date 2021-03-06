#include <algorithm>

#include "dataplane.hpp"
#include "context.hpp"
#include "timer.hpp"
#include "system.hpp"
#include "port_table.hpp"
// #include "fakeapps.hpp"

#include <iostream>
#include <cassert>

namespace fp {

extern Module_table module_table;


// Data plane ctor.
Dataplane::Dataplane(std::string const& name, std::string const& app_name)
  : name_(name), ports_(), portmap_()
{
  auto app = module_table.find(app_name);
  if (app != module_table.end())
    app_ = app->second;
  else
    throw std::string("Unknown application name '" + app_name + "'");
}

Dataplane::~Dataplane()
{
}


// Adds the port to the local list.
void
Dataplane::add_port(Port* p)
{
  ports_.push_back(p);
  portmap_.emplace(p->id(), p);
}


// Removes the port from the local list if it exists.
void
Dataplane::remove_port(Port* p)
{
	auto iter = std::find(ports_.begin(), ports_.end(), p);
  portmap_.erase(p->id());
  ports_.erase(iter);
  delete p;
}


// Add an explicit drop port to the dataplane.
void
Dataplane::add_drop_port()
{
  drop_ = new Port_udp(0xfffffff0, ":8673", "drop");
  ports_.push_back(drop_);
  portmap_.emplace(drop_->id(), drop_);
}


// Add an explicit all port to the dataplane.
void
Dataplane::add_all_port()
{
  all_ = new Port_udp(0xffffffef, ":8674", "all");
  ports_.push_back(all_);
  portmap_.emplace(all_->id(), all_);
}


// Add an explicit all port to the dataplane.
void
Dataplane::add_flood_port()
{
  flood_ = new Port_udp(0xffffffee, ":8675", "flood");
  ports_.push_back(flood_);
  portmap_.emplace(flood_->id(), flood_);
}


// Add an explicit reflow port to the dataplane.
void
Dataplane::add_reflow_port()
{
  reflow_ = new Port_udp(0xffffffed, ":8676", "reflow");
  ports_.push_back(reflow_);
  portmap_.emplace(reflow_->id(), reflow_);
}


// Add all reserved ports.
void
Dataplane::add_reserved_ports()
{
  add_drop_port();
  add_all_port();
  add_flood_port();
  add_reflow_port();
}


// Starts the data plane packet processors. If the application has configured
// the data plane, it will install the application in the thread pool and start
// it. Otherwise it reports errors.
void
Dataplane::up()
{
  // if (!app_)
  //   throw std::string("No applicaiton is installed.");
  // else if (app_->state() == Application::State::READY) {
  //   thread_pool.install(app());
  //   thread_pool.start();
  // }
  // else if (app_->state() == Application::State::NEW)
  //   throw std::string("Data plane has not been configured, unable to start");
}



// For manually passing in packets to the data plane.
void
Dataplane::process(Port* port, Packet* pkt)
{
  // std::cout << "PROCESSING\n";
  Context cxt(*pkt, this, port->id(), port->id(), 0);
  assert(cxt.dataplane());
  // thread_pool.assign(new Task("pipeline", c));

  try
  {
    app_->lib().exec("process", &cxt);
  }
  catch (std::exception& e)
  {
    // Drop the packet.
    Port* p = get_drop_port();
    p->send(&cxt);
  }

  // Apply actions
  cxt.apply_actions();

  // Forward
  if (cxt.output_port() != 0 && cxt.output_port() != get_drop_port()->id()) {
    Port* p = get_port(cxt.output_port());
    p->send(&cxt);

    ++throughput;
    throughput_bytes += pkt->size();
  }
}


// Stops the data plane packet processors, if they are running.
void
Dataplane::down()
{
 //  if (app_->state() == Application::State::RUNNING) {
 //   thread_pool.stop();
 //   thread_pool.uninstall();
 // }
 // else
 //   throw std::string("Data plane is not running.");
}


// Configures the data plane based on the application.
void
Dataplane::configure()
{
  assert(app_);

  if (app_->state() == Application::State::NEW) {
    std::cout << "RUNNING CONFIG\n";

    app_->lib().exec("load", this);
    app_->state_ = Application::State::READY;
  }
  else
    throw std::string("Data plane has already been configured.");
}


// Gets the data plane name.
std::string
Dataplane::name() const
{
  return name_;
}


// Gets the data planes application.
Application*
Dataplane::app()
{
  return app_;
}


// Gets the data planes tables.
std::vector<Table*>
Dataplane::tables() const
{
  return tables_;
}


// Gets the table at the given index.
Table*
Dataplane::table(int idx)
{
  return tables_.at(idx);
}


} // end namespace fp

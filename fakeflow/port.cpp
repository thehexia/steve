#include "port.hpp"
#include "port_table.hpp"

#include <climits>
#include <netinet/in.h>
#include <cassert>
#include <iostream>

namespace fp
{

// Port definitions.
//


// Port constructor that sets ID.
Port::Port(Port::Id id, Port::Label name)
  : id_(id), name_(name)
{ }


// Port dtor.
Port::~Port()
{
	if (addr_)
		delete addr_;
}


// Comparators.
//
bool
Port::operator==(Port& other)
{
  return this->id_ == other.id_;
}


bool
Port::operator==(Port* other)
{
  return this->id_ == other->id_;
}


bool
operator==(Port* p, std::string const& name)
{
  return p->name_ == name;
}


bool
Port::operator!=(Port& other)
{
  return !(*this == other);
}


bool
Port::operator!=(Port* other)
{
  return !(this == other);
}


// Changes the port configuration to 'up'; that is, there are
// no flags set that would indicate the port is not able to
// function.
void
Port::up()
{
  // Clear the bitfield (uint8_t).
  *(uint8_t*)(&config_) = 0;
}


// Change the port to a 'down' configuration.
void
Port::down()
{
  config_.down = 1;
}


// Enqueues context in the ports transmit queue.
void
Port::send(Context* cxt)
{
  // packet_destroy(cxt->packet_);
}

// Gets the port id.
Port::Id
Port::id() const
{
  return id_;
}


} // end namespace FP

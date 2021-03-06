#ifndef FP_PACKET_HPP
#define FP_PACKET_HPP

#include "buffer.hpp"
#include "types.hpp"

#include <algorithm>
#include <cstdint>
#include <cassert>

namespace fp
{

// Buffer type enumeration.
enum Buff_t {
  FP_BUF_NADK,
  FP_BUF_NETMAP,
  FP_BUF_ALLOC,
};


// The Packet type. This is a view on top of an externally allocated
// buffer. The packet does not manage that buffer.
//
// TODO: Should we take the timestamp as a constructor argument or
// create the timestamp inside the packet constructor?
// That is, should it be timestamp(time) or timestamp(get_time()) ?
//
// FIXME: The size of a packet's buffer is almost certainly
// larget than its payload.
struct Packet
{
  Packet(Byte*, int);
  Packet(Byte*, int, uint64_t, void*, Buff_t);

  template<int N>
  Packet(Byte (&buf)[N])
    : Packet(buf, N)
  { }

  ~Packet()
  {
    if (buf_)
      delete [] buf_;
  }

  // Returns a pointer to the raw buffer.
  Byte const* data() const { return buf_; }
  Byte*       data()       { return buf_; }
  int         size() const { return size_; }

  void limit(int n);

  // Data members.
  Byte*     buf_;        // Packet buffer.
  int       size_;       // Total buffer size.
  int       bytes_;      // Total bytes in the packet.
  uint64_t  timestamp_;  // Time of packet arrival.

  // TODO: What is this used for?
  void*     buf_handle_; // [optional] port-specific buffer handle.
  Buff_t    buf_dev_;    // [optional] owner of buffer handle (dev*).
};


inline
Packet::Packet(Byte* data, int size)
	: buf_(data)
  , size_(size)
  , timestamp_(0)
  , buf_handle_(nullptr)
  , buf_dev_(FP_BUF_ALLOC)
{ }


inline
Packet::Packet(Byte* data, int size, uint64_t time, void* buf_handle, Buff_t buf_dev)
	: buf_(data)
  , size_(size)
  , timestamp_(time)
  , buf_handle_(buf_handle)
  , buf_dev_(buf_dev)
{ }


// Set the number of bytes to a smaller value.
inline void
Packet::limit(int n)
{
  assert(n <= size_);
  size_ = n;
}


Packet*   packet_create(Byte*, int, uint64_t, void*, Buff_t);
void      packet_destroy(Packet*);

} // namespace fp


#endif

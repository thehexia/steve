#include <exception>
#include <unordered_map>
#include <cstdarg>

#include "system.hpp"
#include "port_table.hpp"
#include "application.hpp"
#include "endian.hpp"


namespace fp
{


Module_table     module_table;     // Flowpath module table.
Dataplane_table  dataplane_table;  // Flowpath data plane table.
Port_table       port_table;       // Flowpath port table.
// Thread_pool      thread_pool(0, true);      // Flowpath thread pool.


// Creates a new port, adds it to the master port table, and
// returns a pointer to the new port.
Port*
create_port(Port::Type port_type, std::string const& args, std::string const& name)
{
  // Create the port of given type with args in the master port table.
  Port* p = port_table.alloc(port_type, args, name);

  // Return a pointer to it.
  return p;
}


// Deletes the given port from the system port table.
void
delete_port(Port::Id id)
{
  if (port_table.find(id))
    port_table.dealloc(id);
}


// Creates a new data plane and returns a pointer to it. If the
// name already exists it throws an exception.
Dataplane*
create_dataplane(std::string const& name, std::string const& app)
{
  // Check if a dataplane with this name already exists.
  if (dataplane_table.find(name) != dataplane_table.end())
    throw std::string("Data plane name already exists");

  // Allocate the new data plane in the master data plane table.
  dataplane_table.insert({name, new Dataplane(name, app)});

  return dataplane_table.at(name);
}


// Deletes the given data plane from the system data plane table.
void
delete_dataplane(std::string const& name)
{
  auto dp = dataplane_table.find(name);
  if (dp != dataplane_table.end())
    dataplane_table.erase(dp);
  else
    throw std::string("Data plane name not in use");
}


// Loads the application at the given path. If it exists, throws a message.
// If the application does not exist, it creates the module and adds it to
// the module table.
void
load_application(std::string const& path)
{
  // Check if library from this path has already been loaded.
  if (module_table.find(path) != module_table.end())
    throw std::string("Application at '" + path + "' has already been loaded");

  // Register the path with the Application_library object.
  module_table.insert({path, new Application(path)});
}


// Unloads the given application. If it does not exist, throws a message.
void
unload_application(std::string const& path)
{
  auto app = module_table.find(path);
  // Check if library from this path has already been loaded.
  if (app != module_table.end())
    throw std::string("Application at '" + path + "' is not loaded.");

  // Remove the application from the module table.
  module_table.erase(app);
}


} // end namespace fp


//////////////////////////////////////////////////////////////////////////
//                    External Runtime System Calls                     //
//////////////////////////////////////////////////////////////////////////
//
// These are the set of system calls that an application can expect
// to be able to call at runtime.


extern "C"
{


// -------------------------------------------------------------------------- //
// Control instructions


// Send the packet through the drop port.
void
fp_drop(fp::Context* cxt)
{
  // std::cout << "drop\n";
  fp::Port* drop = cxt->dataplane()->get_drop_port();
  assert(drop);
  cxt->set_output_port(drop->id());
}


void
fp_flood(fp::Context* cxt)
{
  // FIXME: Make a flood port.
  fp::Port* flood = cxt->dataplane()->get_flood_port();
  fp_context_set_output_port(cxt, flood);
  assert(flood);
  // flood->send(cxt);
}


// Outputs the contexts packet on the port with the matching name.
void
fp_output_port(fp::Context* cxt, fp::Port::Id id)
{
  // std::cout << "ID: " << id << '\n';
  //
  // for (auto s : cxt->strings_)
  //   std::cout << s << " ";
  cxt->set_output_port(id);

  // fp::Port* p = fp::port_table.find(id);
  // p->send(cxt);
}


// Apply the given action to the context.
void
fp_apply(fp::Context* cxt, fp::Action a)
{
  cxt->apply_action(a);
}


// Write the given action to the context's action list.
void
fp_write(fp::Context* cxt, fp::Action a)
{
  cxt->write_action(a);
}


// // Clear the context's action list.
// void
// fp_clear(fp::Context* cxt)
// {
//   cxt->clear_actions();
// }


// Dispatches the given context to the given table, if it exists.
// Accepts a variadic list of fields needed to construct a key to
// match against the table.
void
fp_goto_table(fp::Context* cxt, fp::Table* tbl, int n, ...)
{
  va_list args;
  va_start(args, n);
  fp::Key key = fp_gather(cxt, tbl->key_size(), n, args);
  va_end(args);

  // std::cout << "KEY: ";
  // for (int i = 0; i < tbl->key_size(); ++i)
  //   std::cout << std::hex << (int) key.data[i];
  // std::cout << '\n';

  fp::Flow flow = tbl->search(key);
  // execute the flow function
  flow.instr_(&flow, tbl, cxt);

  // testing find times
  // static fp::Byte b[fp::key_size];
  // static fp::Key key(b, 8);
  // fp::Flow const& flow = tbl->search(key);
  // execute the flow function
  // flow.instr_(tbl, cxt);

  // static fp::Flow const& flow = dynamic_cast<fp::Hash_table*>(tbl)->begin()->second;
  // flow.instr_(tbl, cxt);
}


// -------------------------------------------------------------------------- //
// Port and table operations

// Returns the port matching the given name.
// fp::Port::Id
// fp_get_port_by_name(char const* name)
// {
//   // std::cout << "GETTING PORT\n";
//   fp::Port* p = fp::port_table.find(name);
//   // std::cout << "FOUND PORT\n";
//   assert(p);
//   return p->id();
// }


// Returns the port matching the given id or error otherwise.
fp::Port::Id
fp_get_port_by_id(fp::Dataplane* dp, unsigned int id)
{
  // std::cout << "GETTING PORT\n";
  fp::Port* p = dp->get_port(id);
  // std::cout << "FOUND PORT\n";
  assert(p);
  return id;
}

// Returns whether or not the port is up or down
bool
fp_port_id_is_up(fp::Dataplane* dp, fp::Port::Id id)
{
  assert(dp);
  fp::Port* p = dp->get_port(id);
  return !p->config_.down;
}

// Returns whether or not the given id exists.
bool
fp_port_id_is_down(fp::Dataplane* dp, fp::Port::Id id)
{
  assert(dp);
  fp::Port* p = dp->get_port(id);
  return p->config_.down;
}

// Returns the port id to the all port;
fp::Port::Id
fp_get_all_port(fp::Dataplane* dp)
{
  assert(dp);
  fp::Port* p = dp->get_all_port();
  return p->id();
}


fp::Port::Id
fp_get_reflow_port(fp::Dataplane* dp)
{
  assert(dp);
  fp::Port* p = dp->get_reflow_port();
  return p->id();
}


fp::Port::Id
fp_get_flood_port(fp::Dataplane* dp)
{
  assert(dp);
  fp::Port* p = dp->get_flood_port();
  return p->id();
}


// Copies the values within 'n' fields into a byte buffer
// and constructs a key from it.
fp::Key
fp_gather(fp::Context* cxt, int key_width, int n, va_list args)
{
  // FIXME: We're using a fixed size key of 128 bytes right now
  // apparently. This should probably be dynamic.
  fp::Byte buf[fp::key_size];
  // Iterate through the fields given in args and copy their
  // values into a byte buffer.
  int i = 0;
  int j = 0;
  int in_port;
  int in_phy_port;
  while (i < n) {
    int f = va_arg(args, int);
    fp::Binding b;
    fp::Byte* p = nullptr;

    // Check for "Special fields"
    switch (f) {
      // Looking for "in_port"
      case 255:
        in_port = cxt->in_port();
        p = reinterpret_cast<fp::Byte*>(&in_port);
        // Copy the field into the buffer.
        std::copy(p, p + sizeof(in_port), &buf[j]);
        j += sizeof(in_port);
        break;

      // Looking for "in_phys_port"
      case 256:
        in_phy_port = cxt->in_phy_port();
        p = reinterpret_cast<fp::Byte*>(&in_phy_port);
        // Copy the field into the buffer.
        std::copy(p, p + sizeof(in_phy_port), &buf[j]);
        j += sizeof(in_phy_port);
        break;

      // Regular fields
      default:
        // Lookup the field in the context.
        b = cxt->get_field_binding(f);
        p = cxt->get_field(b.offset);
        // Copy the field into the buffer.
        std::copy(p, p + b.length, &buf[j]);
        // Then reverse the field in place.
        fp::network_to_native_order(&buf[j], b.length);
        j += b.length;
        break;
    }
    ++i;
  }

  return fp::Key(buf, key_width);
}

// fp::Port::Id
// fp_get_packet_in_port(fp::Context* c)
// {
//   return c->in_port;
// }
//
//
// fp::Port::Id
// fp_get_packet_in_phys_port(fp::Context* c)
// {
//   return c->in_phy_port;
// }


// Creates a new table in the given data plane with the given size,
// key width, and table type.
fp::Table*
fp_create_table(fp::Dataplane* dp, int id, int key_width, int size, fp::Table::Type type)
{
  fp::Table* tbl = nullptr;
  std::cout << "Create table\n";

  switch (type)
  {
    case fp::Table::Type::EXACT:
    // Make a new hash table.
    tbl = new fp::Hash_table(id, size, key_width);
    assert(tbl);
    dp->tables_.push_back(tbl);
    break;
    case fp::Table::Type::PREFIX:
    // Make a new prefix match table.
    break;
    case fp::Table::Type::WILDCARD:
    // Make a new wildcard match table.
    break;
    default:
    throw std::string("Unknown table type given");
  }

  std::cout << "Returning table\n";
  return tbl;
}


// Creates a new flow rule from the given key and function pointer
// and adds it to the given table.
//
// FIXME: Currently ignoring timeout.
void
fp_add_init_flow(fp::Table* tbl, void* fn, void* key, unsigned int timeout, unsigned int egress)
{
  // std::cout << "Adding flow to " << tbl->id() << '\n';
  //
  // get the length of the table's expected key
  int key_size = tbl->key_size();
  // std::cout << "Key size: " << key_size << '\n';
  // cast the key to Byte*
  fp::Byte* buf = reinterpret_cast<fp::Byte*>(key);
  // construct a key object
  fp::Key k(buf, key_size);
  // cast the flow into a flow instruction
  fp::Flow_instructions instr = reinterpret_cast<fp::Flow_instructions>(fn);
  fp::Flow flow(0, fp::Flow_counters(), instr, fp::Flow_timeouts(), 0, 0, egress);

  tbl->add(k, flow);
}


// FIXME: Ignoring timeouts.
void
fp_add_new_flow(fp::Table* tbl, void* fn, void* key, unsigned int timeout, unsigned int egress)
{
  int key_size = tbl->key_size();
  // cast the key to Byte*
  fp::Byte* buf = reinterpret_cast<fp::Byte*>(key);
  // construct a key object
  fp::Key k(buf, key_size);
  // cast the flow into a flow instruction
  fp::Flow_instructions instr = reinterpret_cast<fp::Flow_instructions>(fn);
  fp::Flow flow(0, fp::Flow_counters(), instr, fp::Flow_timeouts(), 0, 0, egress);

  tbl->add(k, flow);
}


fp::Port::Id
fp_get_flow_egress(fp::Flow* f)
{
  assert(f);
  assert(f->egress_ != 0);
  return f->egress_;
}


// Adds the miss case for the table.
//
// FIXME: Ignoring timeout value.
void
fp_add_miss(fp::Table* tbl, void* fn, unsigned int timeout, unsigned int egress)
{
  // cast the flow into a flow instruction
  fp::Flow_instructions instr = reinterpret_cast<fp::Flow_instructions>(fn);
  fp::Flow flow(0, fp::Flow_counters(), instr, fp::Flow_timeouts(), 0, 0, egress);
  tbl->insert_miss(flow);
}


// Removes the given key from the given table, if it exists.
void
fp_del_flow(fp::Table* tbl, void* key)
{
  // get the length of the table's expected key
  int key_size = tbl->key_size();
  // cast the key to Byte*
  fp::Byte* buf = reinterpret_cast<fp::Byte*>(key);
  // construct a key object
  fp::Key k(buf, key_size);
  // delete the key
  tbl->rmv(k);
}

// Removes the miss case from the given table and replaces
// it with the default.
void
fp_del_miss(fp::Table* tbl)
{
  tbl->rmv_miss();
}


// Raise an event.
// TODO: Make this asynchronous on another thread.
void
fp_raise_event(fp::Context* cxt, void* handler)
{
  // Cast the handler back to its appropriate function type
  // of void (*)(Context*)
  void (*event)(fp::Context*);
  event = (void (*)(fp::Context*)) (handler);
  // Invoke the event.
  // FIXME: This should produce a copy of the context and process it
  // seperately.
  event(cxt);
}


// -------------------------------------------------------------------------- //
// Header and field bindings



// // Advances the current header offset by 'n' bytes.
// void
// fp_advance_header(fp::Context* cxt, std::uint16_t n)
// {
//   // std::cout << "ADV: " << n << std::endl;
//   cxt->advance(n);
// }
//
//
// // Binds the current header offset to given identifier.
// void
// fp_bind_header(fp::Context* cxt, int id)
// {
//   cxt->bind_header(id);
// }
//
//
// // Binds a given field index to a section in the packet contexts raw
// // packet data. Using the current cxt offset, relative field offset, and field
// // length we can grab exactly what we need.
// //
// // Returns the pointer to the byte at that specific location
// void
// fp_bind_field(fp::Context* cxt, int id, std::uint16_t off, std::uint16_t len)
// {
//   // std::cout << "BINDING FIELD\n";
//   // Get field requires an absolute offset which is the context's current offset
//   // plus the relative offset passed to this function.
//   int abs_off = cxt->offset() + off;
//   // We bind fields using their absolute offset since this is the only way we
//   // can recover the absolute offset when we need to look up the binding later.
//   //
//   // FIXME: There needs to be a way to store the relative offset instead of the
//   // absolute offset.
//   cxt->bind_field(id, abs_off, len);
// }
//
//
// // Bind twice: once with the original field and again with its aliased name.
// void
// fp_alias_bind(fp::Context* cxt, int original, int alias,
//               std::uint16_t off, std::uint16_t len)
// {
//   // Get field requires an absolute offset which is the context's current offset
//   // plus the relative offset passed to this function.
//   int abs_off = cxt->offset() + off;
//   // We bind fields using their absolute offset since this is the only way we
//   // can recover the absolute offset when we need to look up the binding later.
//   //
//   // FIXME: There needs to be a way to store the relative offset instead of the
//   // absolute offset.
//   cxt->bind_field(original, abs_off, len);
//   cxt->bind_field(alias, abs_off, len);
// }
//
//
//
// fp::Byte*
// fp_read_field(fp::Context* cxt, int fld, fp::Byte* ret)
// {
//   // std::cout << "READING FIELD";
//   // Lookup the field in the context.
//   fp::Binding b = cxt->get_field_binding(fld);
//   fp::Byte* p = cxt->get_field(b.offset);
//   // Convert to native byte ordering.
//   // Copy the value to a temporary.
//   std::copy(p, p + b.length, ret);
//   fp::network_to_native_order(ret, b.length);
//
//   return ret;
// }
//
//
// void
// fp_set_field(fp::Context* cxt, int fld, int len, fp::Byte* val)
// {
//   fp::Binding& b = cxt->get_field_binding(fld);
//
//   // Copy the new data into the packet at the appropriate location.
//   fp::Byte* p = cxt->get_field(b.offset);
//   // Convert native to network order after copying.
//   std::copy(val, val + len, p);
//   fp::native_to_network_order(p, len);
//   // Update the length if it changed (which it shouldn't).
//   b.length = len;
// }
//
} // extern "C"

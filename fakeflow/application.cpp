#include <dlfcn.h>
#include <algorithm>
#include <iostream>
#include "application.hpp"

namespace fp
{


//
Application::Application(std::string const& name)
  : name_(name), state_(NEW), lib_(get_app_handle(name)), num_ports_(0)
{
  // lib_.exec("ports", &num_ports_);
}


//
Application::~Application()
{
  dlclose(lib_.app_);
}


//
void
Application::start()
{
  for (auto port : ports_)
    if (port->open() == -1)
      std::cerr << "Error: open port " << port->id() << '\n';

  state_ = RUNNING;
}


//
void
Application::stop()
{
  for (auto port : ports_)
    port->close();
  state_ = STOPPED;
}


//
void
Application::add_port(Port* p)
{
  if (ports_.size() == num_ports_)
    throw std::string("All ports have been added");
  else if (std::find(ports_.begin(), ports_.end(), p) == ports_.end()) {
    ports_.push_back(p);
  }
  else
    throw std::string("Port already exists");
}


//
void
Application::remove_port(Port* p)
{
  auto idx = std::find(ports_.begin(), ports_.end(), p);
  if (idx != ports_.end())
    ports_.erase(idx);
  else
    throw std::string("Port not found");
}


//
std::string
Application::name() const
{
	return name_;
}


//
Application::State
Application::state() const
{
	return state_;
}


//
std::vector<Port*>
Application::ports() const
{
	return ports_;
}


//
int
Application::num_ports() const
{
	return num_ports_;
}


//
Library
Application::lib() const
{
  return lib_;
}


//
Library::Library(App_handle app)
{
  app_ = app;
  pipeline_ = (void (*)(Context*))get_sym_handle(app, "process");
  config_ = (void (*)(Dataplane*))get_sym_handle(app, "load");
  port_ = (int (*)())get_sym_handle(app, "ports");
  changed_ = (Port_change_fn)get_sym_handle(app, "port_changed");
}


//
Library::~Library()
{ }


// Executes the function matching the given string name. Throws if not defined.
void
Library::exec(std::string const& cmd, void* arg)
{
  if (cmd == "process")
    pipeline_((Context*)arg);
  else if (cmd == "load")
    config_((Dataplane*)arg);
  else if (cmd == "port_changed")
  {
    int val = *(int*)(arg);
    changed_(val);
  }
  // else if (cmd == "ports")
  //   arg = (void*)&port_();
  else
    throw std::string("Application library error: Unknown command '" +
      cmd + "'");
}


// Returns a handle to the given symbol from the given application handle.
void*
get_sym_handle(void* app_hndl, std::string const& sym)
{
  dlerror();
  void* sym_hndl = dlsym(app_hndl, sym.c_str());
  const char* err = dlerror();
  if (err)
    throw std::runtime_error(err);
  else
    return sym_hndl;
}


// Returns a handle to the application at the given path, if it exists.
void*
get_app_handle(std::string const& path)
{
  void* hndl = dlopen(path.c_str(), RTLD_LOCAL | RTLD_LAZY);
  if (hndl)
    return hndl;
  else
    throw std::runtime_error(dlerror());
}


} // end namespace fp

// Copyright (c) 2015 Flowgrammable.org
// All rights reserved

#include "steve/lookup.hpp"
#include "steve/decl.hpp"
#include "steve/print.hpp"

#include "lingo/symbol.hpp"
#include "lingo/error.hpp"

#include <forward_list>
#include <unordered_map>
#include <iostream>


namespace steve
{

namespace
{


// The (name) environment provides a global mapping of 
// names to declarations.
struct Environment : std::unordered_map<String const*, Scope::Binding*>
{
  Overload* push(String const*, Scope*, Decl*);
  void      pop(String const*);

  Scope::Binding* binding(String const*);
};


// The scope stack is a stack of scopes.
struct Stack : std::forward_list<Scope*>
{
  Scope&       top()       { return *front(); }
  Scope const& top() const { return *front(); }
};


// A mapping of forward declarations used but never defined
// if this isn't empty by the time compilation is finished
// then there is an error.
struct Active_forwards : std::unordered_map<String const*, Forward_decl const*>
{
  using std::unordered_map<String const*, Forward_decl const*>::unordered_map;

  void push(String const* n, Forward_decl const* d) { insert({n, d}); }
  void pop(String const* n) { erase(n); }
};


// The global binding environment and scope stack.
Environment env_;
Stack       stack_;
Active_forwards fwd_;


// Push a new name binding into the context. This creates
// a new overload set for the binding.
Overload*
Environment::push(String const* n, Scope* s, Decl* d)
{
  auto ins = insert({n, nullptr});
  Scope::Binding*& b = ins.first->second;

  // Ensure that we haven't created a new overload set for the 
  // same scope.
  lingo_assert(!ins.second ? b->scope != s : true);

  // Chain the new binding to the previous and update.
  b = new Scope::Binding{new Overload{d}, s, b};
  return b->ovl;
}


// Pop the innermost binding for this name. If this we pop
// all bindings, then remove the entry altogether.
void
Environment::pop(String const* n)
{
  auto iter = find(n);
  Scope::Binding* e = iter->second;
  if (e->prev)
    iter->second = e->prev;
  else
    erase(iter);
  delete e;
}


// Returns the binding for the name `n`. If `n` has no current
// binding, returns nullptr.
Scope::Binding*
Environment::binding(String const* n)
{
  auto iter = find(n);
  if (iter != end())
    return iter->second;
  else
    return nullptr;
}


} // namespace


// When constructing a scope, place it on the scope stack.
Scope::Scope(Scope_kind k)
  : kind_(k)
{
  stack_.push_front(this);
}


// Pop all scope bindings and leave the scope whenever
// a scope is destroyed.
Scope::~Scope()
{
  for (String const* s : *this)
    env_.pop(s);
  stack_.pop_front();
}


// Push a new, innermost binding for the given name.
Overload const*
Scope::bind(String const* n, Decl* d)
{
  push_back(n);
  return env_.push(n, this, d);
}


// Returns the innermost declaration bound to `s` or 
// nullptr if no such declaration exists.
Overload const*
Scope::lookup(String const* s) const
{
  if (Scope::Binding* b = env_.binding(s)) {
    // check if its a forward declaration
    if (Forward_decl const* f = as<Forward_decl>(b->ovl->front()))
      fwd_.push(f->name(), f);

    return b->ovl;
  }
  else
    return nullptr;
}


// Returns the innermost declaration bound to `s` or 
// nullptr if no such declaration exists.
Scope::Binding*
Scope::binding(String const* s) const
{
  if (Scope::Binding* b = env_.binding(s))
    return b;
  else
    return nullptr;
}


// -------------------------------------------------------------------------- //
//                           Scope management

// Returns the current stack.
Scope& 
current_scope()
{
  return stack_.top();
}


// -------------------------------------------------------------------------- //
//                             Declarations


<<<<<<< HEAD
=======
// Create a forward declaration with a name binding
// This binding should be overwritten later.
// However, we only care if the name is used, otherwise
// a hanging forward declaration makes no difference.
Overload const*
declare_forward(Decl const* d)
{
  Scope* s = &current_scope();

  Scope::Binding* b = env_.binding(d->name());
  // if we have a previous declaration of this name
  // then it should produce an error.
  if (b && b->scope == s) {
    error(d->location(), "'{}' is a forward declaration which redeclares an existing declaration.", d);
    return nullptr;  
  }

  // bind the name
  return s->bind(d->name(), d);
}


// Allows redeclaration of a binding for a forward declaration
// Find the binding within the current scope
// Remove it and replace it with a new binding
Overload const*
define(String const* n, Decl const* d)
{
  // can't replace a forward decl with another one
  if (is<Forward_decl>(d)) {
    error(d->location(), "Duplicate forward declaration '{}'", d);
    return nullptr;
  }

  // pop the current declaration off the environment
  env_.pop(n);

  // remove it from forward decls that havent been defined
  fwd_.pop(n);

  // declare the new declaration
  return declare(n, d);
}


>>>>>>> parent of 5ccbcb5... Revert "towards foward declarations."
// Create a name binding for the declaration.
//
// If we've already found a declaration in this scope,
// then try to overload it.
//
// Note that `str` must be a unique object in the system. This
// can be guaranteed by getting the string as an identifier from
// the symbol table.
Overload const*
declare(String const* n, Decl* d)
{
  Scope* s = &current_scope();

  // check whether not it is a forward declaration
  if (is<Forward_decl>(d))
    return declare_forward(d);
  
  // If we already have a binding in this scope, then
  // try to overload the given declaration. Note that
  // this will emit diagnostics on failure.
  Scope::Binding* b = env_.binding(n);
  if (b && b->scope == s) {
<<<<<<< HEAD
=======
    // check if the first declaration is a forward decl
    // if it is, then add a definition
    if (is<Forward_decl>(b->ovl->front()))
      return define(n, d);
>>>>>>> parent of 5ccbcb5... Revert "towards foward declarations."
    if (overload_decl(b->ovl, d))
      return b->ovl;
    else
      return nullptr;
  }
  
  // Otherwise, create a new binding.
  return s->bind(n, d);
}


// Declare the variable with the given name into
// the current scope.
Overload const*
declare(Decl* d)
{
  return declare(d->name(), d);
}


// Register bindings for a sequence of declarations.
// Returns true if all declarations were successful.
// bool
// declare(Decl_seq const& ds)
// {
//   for (Decl const* d : ds)
//     if (!declare(d))
//       return false;
//   return true;
// }


// -------------------------------------------------------------------------- //
//                             Name lookup


// Returns the declaration bound `name` or nullptr if
// no such declaration exists.
Overload const*
lookup(String const* n)
{
  return current_scope().lookup(n);
}


// Returns the declaration bound to the given name, or nullptr
// if no such declaration exists.
Overload const*
lookup(char const* n)
{
  return lookup(get_string(n));
}


// Looks up the single declaration corresponding to the given
// name. If there are multiple declarations associated with
// the name, emit a diagnostic and return nullptr.
Decl const*
lookup_decl(String const* n)
{
  if (Overload const* ovl = lookup(n)) {
    if (ovl->is_singleton())
      return ovl->front();

    // FIXME: Write out declarations.
    error("resolution of '{}' is ambiguous", n);
  }
  return nullptr;
}


// -------------------------------------------------------------------------- //
//                       Printing and debugging


// Print all of the declarations whose visibility does not
// exceed `s`.
void
print(Printer& p, Scope const* s)
{
  for (String const* n : *s) {
    Scope::Binding const* b = env_.binding(n);
    print(p, b->ovl);
  }
}


void
print_name_bindings()
{
  Printer p(default_print_stream());
  print_name_bindings(p);
}


// Print all of the declarations visible from `s`.
void
print_name_bindings(Printer& p)
{
  for (auto const& x : env_) {
    Scope::Binding const* b = x.second;
    print(p, b->ovl);
  }
}


} // namespace steve

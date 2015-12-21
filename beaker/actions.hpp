#ifndef ACTIONS_HPP
#define ACTIONS_HPP

#include "beaker/expr.hpp"
#include "beaker/type.hpp"
#include "beaker/stmt.hpp"


// ------------------------------------------------ //
//      Required Actions


struct Action : Stmt
{
  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }
};


struct Drop : Action
{
  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }
};


// Output the packet to a given
// port.
struct Output : Action
{
  Output(Expr* e)
    : port_(e)
  { }

  Expr* port() const { return port_; }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Expr* port_;
};


struct Set_field : Action
{
  Set_field(Expr* f, Expr* v)
    : field_(f), value_(v)
  { }

  Expr* field() const { return field_; }
  Expr* value() const { return value_; }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Expr* field_;
  Expr* value_;
};


struct Copy_field : Action
{
  Copy_field(Expr* f, Expr* v)
    : field_(f), target_(v)
  { }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Expr* field_;
  Expr* target_;
};


struct Add_field : Action
{
};


struct Del_field : Action
{
};


struct Get_field : Action
{
};


// Goto a group table
struct Group : Action
{
};


// Write an output to port acttion
// to the context
struct Write_output : Action
{
};


// Write set field
struct Write_set_field : Action
{
};


// Write add field
struct Write_add_field : Action
{
};


// Write rmv field
struct Write_del_field : Action
{
};


struct Write_copy_field : Action
{
};


struct Write_group : Action
{
};


#endif
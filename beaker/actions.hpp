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


// Call to the next decoder and pass it the context
// FIXME: This should be migrated to an action.
struct Decode_stmt : Action
{
  Decode_stmt(Expr* e, Expr* a)
    : decoder_identifier_(e), decoder_(nullptr), advance_(a)
  { }

  Expr* decoder_identifier() const { return decoder_identifier_; }
  Decl const* decoder() const { return decoder_; }
  Expr* advance() const { return advance_; }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Expr* decoder_identifier_;
  Decl const* decoder_;
  Expr* advance_;
};


// Goto a table.
// FIXME: This should be migrated to an action.
struct Goto_stmt : Action
{
  Goto_stmt(Expr* e, Expr* a)
    : table_identifier_(e), table_(nullptr), advance_(a)
  { }

  Expr* table_identifier() const { return table_identifier_; }
  Decl const* table() const { return table_; }
  Expr* advance() const { return advance_; }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Expr* table_identifier_;
  Decl const* table_;
  Expr* advance_;
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

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Expr* port() const { return port_; }

  Expr* port_;
};


// Clear the set of actions stored within the packet.
struct Clear : Action
{
  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }
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


// Add a flow entry to a table.
struct Insert_flow : Action
{
  Insert_flow(Decl* f, Expr* t)
    : flow_(f), table_id_(t)
  { }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Decl* flow()  const { return flow_; }
  Decl* table() const;
  Expr* table_identifier() const { return table_id_; }

  Decl* flow_;
  Expr* table_id_;
};


inline Decl*
Insert_flow::table() const
{
  assert(is<Decl_expr>(table_id_));
  return as<Decl_expr>(table_id_)->declaration();
}


// Remove a flow entry from a table.
struct Remove_flow : Action
{
  Remove_flow(Expr_seq const& k, Expr* t)
    : keys_(k), table_id_(t)
  { }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Expr_seq const& keys()  const { return keys_; }
  Decl*           table() const;
  Expr*           table_identifier() const { return table_id_; }

  Expr_seq keys_;
  Expr* table_id_;
};


inline Decl*
Remove_flow::table() const
{
  assert(is<Decl_expr>(table_id_));
  return as<Decl_expr>(table_id_)->declaration();
}


// Remove miss case.
struct Remove_miss : Action
{
  Remove_miss(Expr* t)
    : table_id_(t)
  { }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Decl* table() const;
  Expr* table_identifier() const { return table_id_; }

  Expr* table_id_;
};


inline Decl*
Remove_miss::table() const
{
  assert(is<Decl_expr>(table_id_));
  return as<Decl_expr>(table_id_)->declaration();
}


// Raise an event.
struct Raise : Action
{
  Raise(Expr* e)
    : event_id_(e), advance_(nullptr)
  { }

  Raise(Expr* e, Expr* a)
    : event_id_(e), advance_(a)
  { }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Expr* event_identifier() const { return event_id_; }
  Decl* event()            const { return event_; }

  Expr* event_id_;
  Decl* event_;
  Expr* advance_;
};


// Goto a group table
struct Group : Action
{
};


// Write an output to port acttion
// to the context
struct Write_output : Action
{
  Write_output(Stmt* a)
    : first(a)
  { }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Output* output() const { return cast<Output>(first); }

  Stmt* first;
};


// Write set field
struct Write_set_field : Action
{
  Write_set_field(Stmt* a)
  : first(a)
{ }

  void accept(Visitor& v) const { return v.visit(this); }
  void accept(Mutator& v)       { return v.visit(this); }

  Set_field* set_field() const { return cast<Set_field>(first); }

  Stmt* first;
};


struct Write_copy_field : Action
{
};


struct Write_group : Action
{
};


// Returns true iff the statement is a pipeline terminator action.
// Terminators are:
//    decode-stmts
//    goto-stmt
//    drop-stmt
//    flood-stmt
//    output-stmt
inline bool
is_terminator(Stmt* s)
{
  return is<Decode_stmt>(s)
      || is<Goto_stmt>(s)
      || is<Drop>(s);
      // || is<Flood>(s)
      // || is<Output>(s);
      // FIXME: Is raise a terminating action? I don't think it is.
      // Raise should cause a copy of the context to be passed to an
      // asynchronous event handler and allow the continuation of processing
      // on the current packet.
      // || is<Raise>(s);
}


// Returns true iff at least 1 statement in a statement
// sequence contains a terminator.
inline bool
has_terminator_action(Stmt_seq const& body)
{
  for (auto s : body) {
    if (is_terminator(s))
      return true;
  }

  return false;
}


// Returns true iff more than 1 statement in a statement sequence
// is a terminator.
inline bool
has_multiple_terminators(Stmt_seq const& body)
{
  int c = 0;
  for (auto s : body) {
    if (is_terminator(s))
      c++;
  }

  return (c > 1) ? true : false;
}


// Returns true iff a statement is an action.
inline bool
is_action(Stmt* s)
{
  // FIXME: Get rid of this once Decode_stmt and Goto_stmt are made actions.
  return is<Action>(s)
      || is<Decode_stmt>(s)
      || is<Goto_stmt>(s)
      // FIXME: Remove this, currently for debugging purposes only.
      || is<Stmt>(s);
}


// Returns true iff an action occurs within the context of a
// decoder, flow, or event.
inline bool
is_valid_action_context(Stmt* s)
{
  return is<Decode_decl>(s->context())
      || is<Flow_decl>(s->context())
      || is<Event_decl>(s->context());
}


inline bool
is_valid_pipeline_context(Decl* d)
{
  return is<Decode_decl>(d)
      || is<Flow_decl>(d)
      || is<Event_decl>(d);
}


#endif

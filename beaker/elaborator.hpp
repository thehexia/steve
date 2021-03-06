// Copyright (c) 2015 Andrew Sutton
// All rights reserved

#ifndef BEAKER_ELABORATOR_HPP
#define BEAKER_ELABORATOR_HPP

#include <beaker/prelude.hpp>
#include <beaker/location.hpp>
#include <beaker/scope.hpp>

#include <unordered_map>
#include <stack>
#include <unordered_set>
#include <vector>

struct Pipeline;
struct Lowerer;
struct Flow_properties;

// Maintains a sequence of declarations
// which comprise a pipeline and retain
// the module in which the pipeline resides
struct Pipeline_decls : std::unordered_set<Decl*>
{
  Pipeline_decls(Module_decl* m)
    : module_(m)
  { }

  Module_decl const* module() const { return module_; }

  Module_decl* module_;
};


struct Pipeline_stack : std::vector<Pipeline_decls>
{
  void insert(Decl* d) { this->back().insert(d); }
  void new_pipeline(Module_decl* m) { this->push_back(Pipeline_decls(m)); }
};



// Track defined declarations.
using Decl_set = std::unordered_set<Decl*>;


// Track recursive definitions of records.
using Decl_stack = std::vector<Decl*>;


// The elaborator is responsible for the annotation of
// an AST with type and other information.
class Elaborator
{
  struct Scope_sentinel;
  friend class Pipeline_checker;
  friend class Lowerer;
  struct Defining_sentinel;

public:
  Elaborator(Location_map&, Symbol_table&);

  Type const* elaborate_type(Type const*);
  Type const* elaborate_def(Type const*);
  Type const* elaborate(Type const*);
  Type const* elaborate(Id_type const*);
  Type const* elaborate(Boolean_type const*);
  Type const* elaborate(Character_type const*);
  Type const* elaborate(Integer_type const*);
  Type const* elaborate(Function_type const*);
  Type const* elaborate(Array_type const*);
  Type const* elaborate(Block_type const*);
  Type const* elaborate(Reference_type const*);
  Type const* elaborate(Record_type const*);
  Type const* elaborate(Void_type const*);
  Type const* elaborate(Opaque_type const*);
  Type const* elaborate(Varargs_type const*);

  // network specific types
  Type const* elaborate(Layout_type const*);
  Type const* elaborate(Context_type const*);
  Type const* elaborate(Table_type const*);
  Type const* elaborate(Flow_type const*);
  Type const* elaborate(Port_type const*);
  Type const* elaborate(Key_type const*);

  Expr* elaborate(Expr*);
  Expr* elaborate(Literal_expr*);
  Expr* elaborate(Id_expr*);
  Expr* elaborate(Decl_expr*);
  Expr* elaborate(Port_expr*);
  Expr* elaborate(Add_expr* e);
  Expr* elaborate(Sub_expr* e);
  Expr* elaborate(Mul_expr* e);
  Expr* elaborate(Div_expr* e);
  Expr* elaborate(Rem_expr* e);
  Expr* elaborate(Lshift_expr* e);
  Expr* elaborate(Rshift_expr* e);
  Expr* elaborate(Bitwise_and_expr* e);
  Expr* elaborate(Bitwise_or_expr* e);
  Expr* elaborate(Xor_expr* e);
  Expr* elaborate(Neg_expr* e);
  Expr* elaborate(Pos_expr* e);
  Expr* elaborate(Eq_expr* e);
  Expr* elaborate(Ne_expr* e);
  Expr* elaborate(Lt_expr* e);
  Expr* elaborate(Gt_expr* e);
  Expr* elaborate(Le_expr* e);
  Expr* elaborate(Ge_expr* e);
  Expr* elaborate(And_expr* e);
  Expr* elaborate(Or_expr* e);
  Expr* elaborate(Not_expr* e);
  Expr* elaborate(Call_expr* e);
  Expr* elaborate(Dot_expr* e);
  Expr* elaborate(Field_expr* e);
  Expr* elaborate(Method_expr* e);
  Expr* elaborate(Index_expr* e);
  Expr* elaborate(Value_conv* e);
  Expr* elaborate(Block_conv* e);
  Expr* elaborate(Promotion_conv* e);
  Expr* elaborate(Demotion_conv* e);
  Expr* elaborate(Sign_conv* e);
  Expr* elaborate(Integer_conv* e);
  Expr* elaborate(Variadic_conv* e);
  Expr* elaborate(Default_init* e);
  Expr* elaborate(Copy_init* e);
  Expr* elaborate(Reference_init* e);
  Expr* elaborate(Reinterpret_cast* e);
  Expr* elaborate(Void_cast* e);
  Expr* elaborate(Field_name_expr* e);
  Expr* elaborate(Field_access_expr* e);
  Expr* elaborate(Inport_expr* e);
  Expr* elaborate(Inphysport_expr* e);
  Expr* elaborate(All_port* e);
  Expr* elaborate(Controller_port* e);
  Expr* elaborate(Reflow_port* e);
  Expr* elaborate(Flood_port* e);
  Expr* elaborate(Egress_port* e);

  Decl* elaborate(Decl*);
  Decl* elaborate(Variable_decl*);
  Decl* elaborate(Function_decl*);
  Decl* elaborate(Parameter_decl*);
  Decl* elaborate(Record_decl*);
  Decl* elaborate(Field_decl*);
  Decl* elaborate(Method_decl*);
  Decl* elaborate(Module_decl*);

  // network declarations
  Decl* elaborate(Layout_decl*);
  Decl* elaborate(Decode_decl*);
  Decl* elaborate(Table_decl*);
  Decl* elaborate(Key_decl*);
  Decl* elaborate(Inport_key_decl*);
  Decl* elaborate(Inphysport_key_decl*);
  Decl* elaborate(Flow_decl*);
  Decl* elaborate(Port_decl*);
  Decl* elaborate(Extracts_decl*);
  Decl* elaborate(Rebind_decl*);
  Decl* elaborate(Event_decl*);

  // Support for two-phase elaboration.
  Decl* elaborate_decl(Decl*);
  Decl* elaborate_decl(Variable_decl*);
  Decl* elaborate_decl(Function_decl*);
  Decl* elaborate_decl(Parameter_decl*);
  Decl* elaborate_decl(Record_decl*);
  Decl* elaborate_decl(Field_decl*);
  Decl* elaborate_decl(Method_decl*);

  Decl* elaborate_decl(Layout_decl*);
  Decl* elaborate_decl(Port_decl*);
  Decl* elaborate_decl(Decode_decl*);
  Decl* elaborate_decl(Table_decl*);
  Decl* elaborate_decl(Event_decl*);
  Decl* elaborate_decl(Module_decl*);

  Decl* elaborate_def(Decl*);
  Decl* elaborate_def(Variable_decl*);
  Decl* elaborate_def(Function_decl*);
  Decl* elaborate_def(Parameter_decl*);
  Decl* elaborate_def(Record_decl*);
  Decl* elaborate_def(Field_decl*);
  Decl* elaborate_def(Method_decl*);

  Decl* elaborate_def(Layout_decl*);
  Decl* elaborate_def(Port_decl*);
  Decl* elaborate_def(Decode_decl*);
  Decl* elaborate_def(Table_decl*);
  Decl* elaborate_def(Event_decl*);
  Decl* elaborate_def(Module_decl*);

  Stmt* elaborate(Stmt*);
  Stmt* elaborate(Empty_stmt*);
  Stmt* elaborate(Block_stmt*);
  Stmt* elaborate(Assign_stmt*);
  Stmt* elaborate(Return_stmt*);
  Stmt* elaborate(Return_void_stmt*);
  Stmt* elaborate(If_then_stmt*);
  Stmt* elaborate(If_else_stmt*);
  Stmt* elaborate(Match_stmt*);
  Stmt* elaborate(Case_stmt*);
  Stmt* elaborate(While_stmt*);
  Stmt* elaborate(Break_stmt*);
  Stmt* elaborate(Continue_stmt*);
  Stmt* elaborate(Expression_stmt*);
  Stmt* elaborate(Declaration_stmt*);
  Stmt* elaborate(Decode_stmt*);
  Stmt* elaborate(Goto_stmt*);

  Stmt* elaborate(Action*);
  Stmt* elaborate(Drop*);
  Stmt* elaborate(Output*);
  Stmt* elaborate(Clear*);
  Stmt* elaborate(Set_field*);
  Stmt* elaborate(Insert_flow*);
  Stmt* elaborate(Remove_flow*);
  Stmt* elaborate(Remove_miss*);
  Stmt* elaborate(Raise*);
  Stmt* elaborate(Write_output*);
  Stmt* elaborate(Write_set_field*);

  // Helper functions.
  Decl*                 elaborate_added_flow_body(Flow_decl*, Table_decl*);
  Decl*                 elaborate_added_flow_decl(Flow_decl*, Table_decl*);
  std::string           build_field_name(Dot_expr*);
  Symbol const*         get_field_name(Dot_expr*);
  bool                  is_field_access(Dot_expr*);
  Decl*                 check_field_path(Dot_expr*, Decl_seq&, Expr_seq&);
  Field_name_expr*      elaborate_field_name(Dot_expr*);
  Field_access_expr*    elaborate_field_access(Dot_expr*);
  Flow_properties       elaborate_flow_properties(Flow_decl*);
  bool                  is_valid_property(String, Expr*, Flow_properties&);

  void declare(Decl*);
  void redeclare(Decl*);
  void overload(Overload&, Decl*);

  Expr* call(Function_decl*, Expr_seq const&);
  Expr* resolve(Overload_expr*, Expr_seq const&);

  Overload* unqualified_lookup(Symbol const*);
  Overload* qualified_lookup(Scope*, Symbol const*);
  Overload* member_lookup(Record_decl*, Symbol const*);

  Symbol const* get_qualified_name(Expr_seq const&);

  // Diagnostics
  void check_valid_action_context(Stmt*);
  void on_call_error(Expr_seq const&, Expr_seq const&, Type_seq const&);
  void locate(void const*, Location);
  Location locate(void const*);

  bool is_defining(Decl const*) const;

  // Found symbols.
  Function_decl* main = nullptr;

private:
  Location_map& locs;
  Symbol_table& syms;

  // maintain a list of pipeline decls per module
  Pipeline_stack pipelines;
  // Maintain added flows.
  std::vector<Flow_decl*> added_flows_;

  Scope_stack   stack;
  Decl_set      defined;
  Decl_stack    defining;
};


inline
Elaborator::Elaborator(Location_map& loc, Symbol_table& s)
  : locs(loc), syms(s)
{ }


inline void
Elaborator::locate(void const* p, Location l)
{
  locs.emplace(p, l);
}


inline Location
Elaborator::locate(void const* p)
{
  auto iter = locs.find(p);
  if (iter != locs.end())
    return iter->second;
  else
    return {};
}


// An RAII class that helps manage the scope stack.
// When constructed, a new scope is pushed on to
// the stack. On exit, that scope is removed.
struct Elaborator::Scope_sentinel
{
  // Initialize the new scope sentinel. A declaration
  // `d` can be associated with the new scope.
  Scope_sentinel(Elaborator& e, Decl* d = nullptr)
    : elab(e), take(false)
  {
    elab.stack.push(d);
  }

  // Push an existing scope onto the stack. Note that
  // this is not destroyed when the sentinel goes out
  // of scope.
  Scope_sentinel(Elaborator& e, Scope* s)
    : elab(e), take(true)
  {
    elab.stack.push(s);
  }

  ~Scope_sentinel()
  {
    if (take)
      elab.stack.take();
    else
      elab.stack.pop();
  }

  Elaborator& elab;
  bool        take;
};


// An RAII class used to manage a stack of definitions.
// This helps to prevent loops in recursive elaborations.
struct Elaborator::Defining_sentinel
{
  Defining_sentinel(Elaborator& e, Decl* d)
    : elab(e)
  {
    elab.defining.push_back(d);
  }

  ~Defining_sentinel()
  {
    elab.defining.pop_back();
  }

  Elaborator& elab;
};


#endif

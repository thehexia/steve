#ifndef LOWER_HPP
#define LOWER_HPP

#include "beaker/builder.hpp"
#include "beaker/builtin.hpp"
#include "beaker/elaborator.hpp"
#include "beaker/pipeline.hpp"
#include "beaker/length.hpp"
#include "beaker/offset.hpp"

struct Lowerer
{
  struct Scope_sentinel;

  Lowerer(Elaborator& elab, Pipeline_checker checker)
    : elab(elab), stack(elab.stack), builtin(elab.syms), checker(checker),
      port_count(0)
  { }

  Expr* lower(Expr*);
  Expr* lower(Port_expr*);
  Expr* lower(Value_conv* e);
  Expr* lower(Promotion_conv* e);
  Expr* lower(Demotion_conv* e);
  Expr* lower(Sign_conv* e);
  Expr* lower(Call_expr* e);
  Expr* lower(Field_access_expr* e);
  Expr* lower(Inport_expr* e);
  Expr* lower(Inphysport_expr* e);
  Expr* lower(All_port* e);
  Expr* lower(Controller_port* e);
  Expr* lower(Reflow_port* e);
  Expr* lower(Flood_port* e);
  Expr* lower(Egress_port* e);

  template <typename T>
  Expr* lower_unary_expr(T*);

  template <typename T>
  Expr* lower_binary_expr(T*);

  Decl* lower_global_decl(Decl*);
  Decl* lower_global_decl(Decode_decl*);
  Decl* lower_global_decl(Table_decl*);
  Decl* lower_global_decl(Port_decl*);
  Decl* lower_global_decl(Event_decl*);

  Decl* lower_global_def(Decl*);
  Decl* lower_global_def(Decode_decl*);
  Decl* lower_global_def(Table_decl*);
  Decl* lower_global_def(Port_decl*);
  Decl* lower_global_def(Event_decl*);

  Decl* lower(Decl*);
  Decl* lower(Module_decl*);
  Decl* lower(Variable_decl*);

  // network declarations
  Decl* lower(Layout_decl*);
  Decl* lower(Decode_decl*);
  Decl* lower(Table_decl*);
  Decl* lower(Key_decl*);
  Decl* lower(Flow_decl*);
  Decl* lower(Port_decl*);

  Flow_properties lower_flow_properties(Flow_decl*);
  Function_decl*  lower_init_flow(Table_decl*, Flow_decl*);
  void     add_init_flows(Table_decl*);
  Expr_seq lower_flow_keys(Decl_seq const&);
  Stmt*    lower_flow_body(Table_decl*, Stmt*);
  Decl*    lower_miss_case(Table_decl*);
  Stmt_seq lower_extracts_decl(Extracts_decl*);
  Stmt_seq lower_rebind_decl(Rebind_decl*);
  void     produce_key_function(Table_decl*);
  Decl*    construct_added_flow(Table_decl*, Flow_decl*);
  Expr*    lower_advance_clause(Expr*);

  Stmt_seq lower(Stmt*);
  Stmt_seq lower(Assign_stmt*);
  Stmt_seq lower(Block_stmt*);
  Stmt_seq lower(If_then_stmt*);
  Stmt_seq lower(If_else_stmt*);
  Stmt_seq lower(Match_stmt*);
  Stmt_seq lower(Case_stmt*);
  Stmt_seq lower(While_stmt*);
  Stmt_seq lower(Expression_stmt*);
  Stmt_seq lower(Declaration_stmt*);
  Stmt_seq lower(Decode_stmt*);

  // helper functions for handling
  // table gotos.
  Stmt* goto_advance(Decl const*);
  Stmt* goto_get_key(Decl const*);
  Stmt* goto_match(Goto_stmt*);
  Stmt_seq lower(Goto_stmt*);

  Stmt_seq lower(Action*);
  Stmt_seq lower(Drop*);
  Stmt_seq lower(Output*);
  Stmt_seq lower(Clear*);
  Stmt_seq lower(Set_field*);
  Stmt_seq lower(Insert_flow*);
  Stmt_seq lower(Remove_flow*);
  Stmt_seq lower(Remove_miss*);
  Stmt_seq lower(Raise*);
  Stmt_seq lower(Write_output*);
  Stmt_seq lower(Write_set_field*);

  // application interface
  Function_decl* load_function();
  Function_decl* process_function();
  Function_decl* port_number_function();
  Function_decl* start_function();
  Function_decl* port_changed_function();

  Variable_decl* dataplane_pointer();

  // builtin handling
  void add_builtin_ports();
  void add_builtin_functions();
  void add_builtin_variables();
  void add_prelude();

  void declare(Decl*);
  void redeclare(Decl*);
  void overload(Overload&, Decl*);
  Symbol const* get_identifier(std::string);
  Overload* unqualified_lookup(Symbol const*);
  Overload* qualified_lookup(Scope*, Symbol const*);

  Elaborator& elab;
  Scope_stack& stack;
  Builtin builtin;
  Pipeline_checker checker;

  // The new program
  Decl_seq module_decls;

private:

  // Maintain the first function to call in the pipeline
  Decl* start_fn;

  // Maintain the number of ports
  int port_count;

  // Runtime declarations
  Decl_seq prelude;

  // Key forming functions
  Decl_seq key_functions;

  // load function body
  Stmt_seq load_body;

  // All ports.
  Decl_seq all_ports;
};


struct Lowerer::Scope_sentinel
{
  Scope_sentinel(Lowerer& e, Decl* d = nullptr)
    : lower(e)
  {
    lower.stack.push(d);
  }

  ~Scope_sentinel()
  {
    lower.stack.pop();
  }

  Lowerer& lower;
};

#endif

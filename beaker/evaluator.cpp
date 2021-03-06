// Copyright (c) 2015 Andrew Sutton
// All rights reserved

#include "beaker/evaluator.hpp"
#include "beaker/type.hpp"
#include "beaker/expr.hpp"
#include "beaker/decl.hpp"
#include "beaker/stmt.hpp"
#include "beaker/error.hpp"

#include <iostream>


Value
Evaluator::eval(Expr const* e)
{
  struct Fn
  {
    Evaluator& ev;

    Value operator()(Literal_expr const* e) { return ev.eval(e); }
    Value operator()(Id_expr const* e) { return ev.eval(e); }
    Value operator()(Decl_expr const* e) { return ev.eval(e); }
    Value operator()(Add_expr const* e) { return ev.eval(e); }
    Value operator()(Sub_expr const* e) { return ev.eval(e); }
    Value operator()(Mul_expr const* e) { return ev.eval(e); }
    Value operator()(Div_expr const* e) { return ev.eval(e); }
    Value operator()(Rem_expr const* e) { return ev.eval(e); }
    Value operator()(Lshift_expr const* e) { return ev.eval(e); }
    Value operator()(Rshift_expr const* e) { return ev.eval(e); }
    Value operator()(Bitwise_and_expr const* e) { return ev.eval(e); }
    Value operator()(Bitwise_or_expr const* e) { return ev.eval(e); }
    Value operator()(Xor_expr const* e) { return ev.eval(e); }
    Value operator()(Neg_expr const* e) { return ev.eval(e); }
    Value operator()(Pos_expr const* e) { return ev.eval(e); }
    Value operator()(Eq_expr const* e) { return ev.eval(e); }
    Value operator()(Ne_expr const* e) { return ev.eval(e); }
    Value operator()(Lt_expr const* e) { return ev.eval(e); }
    Value operator()(Gt_expr const* e) { return ev.eval(e); }
    Value operator()(Le_expr const* e) { return ev.eval(e); }
    Value operator()(Ge_expr const* e) { return ev.eval(e); }
    Value operator()(And_expr const* e) { return ev.eval(e); }
    Value operator()(Or_expr const* e) { return ev.eval(e); }
    Value operator()(Not_expr const* e) { return ev.eval(e); }
    Value operator()(Call_expr const* e) { return ev.eval(e); }
    Value operator()(Dot_expr const* e) { return ev.eval(e); }
    Value operator()(Field_expr const* e) { return ev.eval(e); }
    Value operator()(Method_expr const* e) { return ev.eval(e); }
    Value operator()(Index_expr const* e) { return ev.eval(e); }
    Value operator()(Value_conv const* e) { return ev.eval(e); }
    Value operator()(Block_conv const* e) { return ev.eval(e); }
    Value operator()(Promotion_conv const* e) { return ev.eval(e); }
    Value operator()(Demotion_conv const* e) { return ev.eval(e); }
    Value operator()(Sign_conv const* e) { return ev.eval(e); }
    Value operator()(Integer_conv const* e) { return ev.eval(e); }
    Value operator()(Variadic_conv const* e) { return ev.eval(e); }
    Value operator()(Default_init const* e) { return ev.eval(e); }
    Value operator()(Copy_init const* e) { return ev.eval(e); }
    Value operator()(Reference_init const* e) { return ev.eval(e); }
    Value operator()(Reinterpret_cast const* e) { throw Eval_error(); }
    Value operator()(Void_cast const* e) { throw Eval_error(); }

    Value operator()(Field_name_expr const* e) { throw Eval_error(); }
    Value operator()(Field_access_expr const* e) { throw Eval_error(); }
    Value operator()(Inport_expr const* e) { throw Eval_error(); }
    Value operator()(Inphysport_expr const* e) { throw Eval_error(); }
    Value operator()(All_port const* e) { throw Eval_error(); }
    Value operator()(Controller_port const* e) { throw Eval_error(); }
    Value operator()(Reflow_port const* e) { throw Eval_error(); }
    Value operator()(Flood_port const* e) { throw Eval_error(); }
    Value operator()(Egress_port const* e) { throw Eval_error(); }
  };

  return apply(e, Fn {*this});
}


Value
Evaluator::eval(Literal_expr const* e)
{
  return e->value();
}


Value
Evaluator::eval(Id_expr const* e)
{
  throw Eval_error();
}


Value
Evaluator::eval(Decl_expr const* e)
{
  throw Eval_error();
  // return &stack.lookup(e->name())->second;
}


// TODO: Detect overflow.
Value
Evaluator::eval(Add_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return v1.get_integer() + v2.get_integer();
}


// TODO: Detect overflow.
Value
Evaluator::eval(Sub_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return v1.get_integer() - v2.get_integer();
}


// TODO: Detect overflow.
Value
Evaluator::eval(Mul_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return v1.get_integer() * v2.get_integer();
}


Value
Evaluator::eval(Div_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  if (v2.get_integer() == 0)
    throw std::runtime_error("division by 0");
  return v1.get_integer() / v2.get_integer();
}


Value
Evaluator::eval(Rem_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  if (v2.get_integer() == 0)
    throw std::runtime_error("division by 0");
  return v1.get_integer() / v2.get_integer();
}

// TODO: Detect overflow.
Value
Evaluator::eval(Lshift_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return v1.get_integer() << v2.get_integer();
}


// TODO: Detect overflow.
Value
Evaluator::eval(Rshift_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return v1.get_integer() >> v2.get_integer();
}


// TODO: Detect overflow.
Value
Evaluator::eval(Bitwise_and_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return v1.get_integer() & v2.get_integer();
}


// TODO: Detect overflow.
Value
Evaluator::eval(Bitwise_or_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return v1.get_integer() | v2.get_integer();
}


// TODO: Detect overflow.
Value
Evaluator::eval(Xor_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return v1.get_integer() ^ v2.get_integer();
}


Value
Evaluator::eval(Neg_expr const* e)
{
  Value v = eval(e->operand());
  return -v.get_integer();
}


Value
Evaluator::eval(Pos_expr const* e)
{
  return eval(e->operand());
}


// Compare two integer or function values.
template<typename F>
bool
compare_equal(Value const& v1, Value const& v2, F fn)
{
  // See through references.
  Value const& a = v1.is_reference() ? *v1.get_reference() : v1;
  Value const& b = v2.is_reference() ? *v2.get_reference() : v2;

  // Perform comparison.
  if (a.kind() == b.kind()) {
    if (a.is_integer())
      return fn(a.get_integer(), b.get_integer());
    if (a.is_function())
      return fn(a.get_function(), b.get_function());
  }
  throw Evaluation_error({}, "invalid operands");
}


Value
Evaluator::eval(Eq_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return compare_equal(v1, v2, std::equal_to<>());
}


// Compare two integer or function values.
Value
Evaluator::eval(Ne_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return compare_equal(v1, v2, std::not_equal_to<>());
}


// Compare two integer or function values.
template<typename F>
bool
compare_less(Value const& v1, Value const& v2, F fn)
{
  Value const& a = v1.is_reference() ? *v1.get_reference() : v1;
  Value const& b = v2.is_reference() ? *v2.get_reference() : v2;

  if (a.kind() == b.kind()) {
    if (a.is_integer())
      return fn(a.get_integer(), b.get_integer());
  }
  throw Evaluation_error({}, "invalid operands");
}

// Order two integer values.
Value
Evaluator::eval(Lt_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return compare_less(v1, v2, std::less<>());
}


Value
Evaluator::eval(Gt_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return compare_less(v1, v2, std::greater<>());
}


Value
Evaluator::eval(Le_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return compare_less(v1, v2, std::less_equal<>());
}


Value
Evaluator::eval(Ge_expr const* e)
{
  Value v1 = eval(e->left());
  Value v2 = eval(e->right());
  return compare_less(v1, v2, std::greater_equal<>());
}


Value
Evaluator::eval(And_expr const* e)
{
  Value v = eval(e->left());
  if (!v.get_integer())
    return v;
  else
    return eval(e->right());
}


Value
Evaluator::eval(Or_expr const* e)
{
  Value v = eval(e->left());
  if (v.get_integer())
    return v;
  else
    return eval(e->right());
}


Value
Evaluator::eval(Not_expr const* e)
{
  Value v = eval(e->operand());
  return !v.get_integer();
}


Value
Evaluator::eval(Call_expr const* e)
{
  // Evaluate the function expression.
  Value v = eval(e->target());
  Function_decl const* f = v.get_function();

  // Evaluate each argument in turn.
  Value_seq args;
  args.reserve(e->arguments().size());
  for (Expr const* a : e->arguments())
    args.push_back(eval(a));

  // Build the new call frame by pushing bindings
  // from each parameter to the corresponding argument.
  //
  // FIXME: Since everything type-checked, these *must*
  // happen to magically line up. However, it would be
  // a good idea to verify.
  Store_sentinel frame(*this);
  for (std::size_t i = 0; i < args.size(); ++i) {
    Decl* p = f->parameters()[i];
    Value& v = args[i];
    stack.top().bind(p->name(), v);
  }

  // Evaluate the function definition.
  //
  // TODO: Check result in case we've thrown
  // an exception (for example).
  Value result;
  Control ctl = eval(f->body(), result);
  if (ctl != return_ctl)
    throw std::runtime_error("function evaluation failed");

  return result;
}


Value
Evaluator::eval(Dot_expr const* e)
{
  lingo_unreachable();
}


Value
Evaluator::eval(Field_expr const* e)
{
  Value obj = eval(e->container());
  Value* ref = obj.get_reference();
  return &ref->get_tuple().data[e->field()->index()];
}


Value
Evaluator::eval(Method_expr const* e)
{
  lingo_unimplemented();
}


// Return a reference to nth element of an array.
Value
Evaluator::eval(Index_expr const* e)
{
  Value arr = eval(e->array());
  Value* ref = arr.get_reference();
  Value ix = eval(e->index());
  return &ref->get_array().data[ix.get_integer()];
}


// Apply an object-to-value conversion by dereferencing
// the reference value. Note that the source must evaluate
// to a reference.
Value
Evaluator::eval(Value_conv const* e)
{
  Value v = eval(e->source());
  return *v.get_reference();
}


// Apply an array-to-block conversion by dereferencing
// the reference value. Note that the source must evaluate
// to a reference.
Value
Evaluator::eval(Block_conv const* e)
{
  throw std::runtime_error("not implemented");
}


// Return the evaluation of the integer
Value
Evaluator::eval(Promotion_conv const* e)
{
  Value v = eval(e->source());
  return v.get_integer();
}


// Return the evaluation of the integer
Value
Evaluator::eval(Demotion_conv const* e)
{
  Value v = eval(e->source());
  return v.get_integer();
}


// Return the evaluation of the integer
// FIXME: It should be the signed/unsigned evaluation of the integer.
// FIXME: This isn't right. There should be some explicit chage
// of the underlying integer.
Value
Evaluator::eval(Sign_conv const* e)
{
  Value v = eval(e->source());
  return v.get_integer();
}


Value
Evaluator::eval(Integer_conv const* e)
{
  Value v = eval(e->source());
  return v.get_integer();
}


Value
Evaluator::eval(Variadic_conv const* e)
{
  Value v = eval(e->source());
  return v.get_integer();
}


// FIXME: This is wrong. We should be calling a function
// that default initializes the created object.
Value
Evaluator::eval(Default_init const* e)
{
  lingo_unimplemented();
}


// FIXME: This should be calling a function that
// default iniitializes the created object.
Value
Evaluator::eval(Copy_init const* e)
{
  lingo_unimplemented();
}


Value
Evaluator::eval(Reference_init const* e)
{
  lingo_unimplemented();
}


Value
Evaluator::eval(Field_name_expr const* e)
{
  throw std::runtime_error("not implemented");
}


// -------------------------------------------------------------------------- //
// Evaluation of declarations

void
Evaluator::eval(Decl const* d)
{
  struct Fn
  {
    Evaluator& ev;

    void operator()(Variable_decl const* d) { ev.eval(d); }
    void operator()(Function_decl const* d) { ev.eval(d); }
    void operator()(Parameter_decl const* d) { ev.eval(d); }
    void operator()(Record_decl const* d) { ev.eval(d); }
    void operator()(Field_decl const* d) { ev.eval(d); }
    void operator()(Method_decl const* d) { ev.eval(d); }
    void operator()(Module_decl const* d) { ev.eval(d); }

    void operator()(Layout_decl const* d) { ev.eval(d); }
    void operator()(Decode_decl const* d) { ev.eval(d); }
    void operator()(Table_decl const* d) { ev.eval(d); }
    void operator()(Key_decl const* d) { ev.eval(d); }
    void operator()(Flow_decl const* d) { ev.eval(d); }
    void operator()(Port_decl const* d) { ev.eval(d); }
    void operator()(Extracts_decl const* d) { ev.eval(d); }
    void operator()(Rebind_decl const* d) { ev.eval(d); }
    void operator()(Event_decl const* d) { ev.eval(d); }

  };

  return apply(d, Fn{*this});
}


namespace
{

// Allocate a value whose shape is determined
// by the type. No guarantees are made about the
// contents of the resulting value.
Value
get_value(Type const* t)
{
  struct Fn
  {
    Value operator()(Id_type const*) { lingo_unreachable(); }

    // Produce an integer value.
    Value operator()(Boolean_type const*) { return 0; }
    Value operator()(Character_type const*) { return 0; }
    Value operator()(Integer_type const*) { return 0; }

    // Produce a function value.
    Value operator()(Function_type const*)
    {
      return Function_value(nullptr);
    }

    // Recursively construct an array whose values are
    // shaped by the element type.
    Value operator()(Array_type const* t)
    {
      Array_value v(t->size());
      for (std::size_t i = 0; i < v.len; ++i)
        v.data[i] = get_value(t->type());
      return v;
    }


    // FIXME: What kind of value is this?
    Value operator()(Block_type const*)
    {
      throw std::runtime_error("not implemented");
    }


    Value operator()(Reference_type const*)
    {
      return Reference_value(nullptr);
    }


    Value operator()(Record_type const* t)
    {
      Record_decl const* d = t->declaration();
      Decl_seq const& f = d->fields();
      Tuple_value v(f.size());
      for (std::size_t i = 0; i < v.len; ++i)
        v.data[i] = get_value(f[i]->type());
      return v;
    }

    Value operator()(Void_type const* t) { return 0; }
    Value operator()(Opaque_type const* t) { return 0; }
    Value operator()(Varargs_type const* t) { return 0; }

    Value operator()(Layout_type const* t) { return 0; }
    Value operator()(Context_type const* t) { return 0; }
    Value operator()(Port_type const* t) { return 0; }
    Value operator()(Table_type const* t) { return 0; }
    Value operator()(Flow_type const* t) { return 0; }
    Value operator()(Key_type const* t) { return 0; }
  };
  return apply(t, Fn{});
}

} // namespace


void
Evaluator::eval(Variable_decl const* d)
{
  // Create an uninitialized object and bind it
  // to the symbol. Keep a reference so we can
  // initialize it directly.
  Value v0 = get_value(d->type());
  Value& v1 = stack.top().bind(d->name(), v0).second;

  // Handle initialization.
  //
  // FIXME: The initializer should hold a function
  // that can be evalated to perform the initialization
  // procedure. We shouldn't be doing this explicitly.
  Expr const* e = d->init();

  // Perform default initialization.
  if (is<Default_init>(e))
    zero_init(v1);

  // Perfor copy initialization. We should guarantee
  // that v1 and the evaluation of i produce values
  // of the same shape.
  else if (Copy_init const* i = as<Copy_init>(e))
    v1 = eval(i->value());
  else
    throw std::runtime_error("unhandled initializer");
}


// Bind the symbol to a function value.
void
Evaluator::eval(Function_decl const* d)
{
  stack.top().bind(d->name(), d);
}


// This function should never be called.
void
Evaluator::eval(Parameter_decl const*)
{
  return;
}


// There is no evaluation for a record.
void
Evaluator::eval(Record_decl const*)
{
  return;
}


// There is no evaluation for a field.
void
Evaluator::eval(Field_decl const*)
{
  return;
}


// There is no evaluation for a method.
void
Evaluator::eval(Method_decl const*)
{
  return;
}


// Evaluate the declarations in the module.
void
Evaluator::eval(Module_decl const* d)
{
  Store_sentinel store(*this);
  for (Decl const* d1 : d->declarations())
    eval(d1);
}


// There is no evaluation for a layout.
void
Evaluator::eval(Layout_decl const*)
{
  lingo_unreachable();
}


void
Evaluator::eval(Decode_decl const* d)
{
  lingo_unreachable();
}


void
Evaluator::eval(Table_decl const* d)
{
  lingo_unreachable();
}


// No evaluation on key decl
void
Evaluator::eval(Key_decl const* d)
{
  lingo_unreachable();
}


void
Evaluator::eval(Flow_decl const* d)
{
  lingo_unreachable();
}


void
Evaluator::eval(Port_decl const* d)
{
  lingo_unreachable();
}


void
Evaluator::eval(Extracts_decl const* d)
{
  lingo_unreachable();
}


void
Evaluator::eval(Rebind_decl const* d)
{
  lingo_unreachable();
}


void
Evaluator::eval(Event_decl const* d)
{
  lingo_unreachable();
}


// -------------------------------------------------------------------------- //
// Evaluation of statements

// Evaluate the given statement, returning a
// control instruction, which determines how
// the evaluation proceeds. Storage is provided
// for a return value as an output argument.
Control
Evaluator::eval(Stmt const* s, Value& r)
{
  struct Fn
  {
    Evaluator& ev;
    Value& r;

    Control operator()(Empty_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Block_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Assign_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Return_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Return_void_stmt const* s) { return ev.eval(s, r); }
    Control operator()(If_then_stmt const* s) { return ev.eval(s, r); }
    Control operator()(If_else_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Match_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Case_stmt const* s) { return ev.eval(s, r); }
    Control operator()(While_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Break_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Continue_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Expression_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Declaration_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Decode_stmt const* s) { return ev.eval(s, r); }
    Control operator()(Goto_stmt const* s) { return ev.eval(s, r); }


    Control operator()(Action const* s) { lingo_unreachable(); }
    Control operator()(Drop const* s) { lingo_unreachable(); }
    Control operator()(Output const* s) { lingo_unreachable(); }
    Control operator()(Clear const* s) { lingo_unreachable(); }
    Control operator()(Set_field const* s) { lingo_unreachable(); }
    Control operator()(Insert_flow const* s) { lingo_unreachable(); }
    Control operator()(Remove_flow const* s) { lingo_unreachable(); }
    Control operator()(Remove_miss const* s) { lingo_unreachable(); }
    Control operator()(Raise const* s) { lingo_unreachable(); }
    Control operator()(Write_output const* s) { lingo_unreachable(); }
    Control operator()(Write_set_field const* s) { lingo_unreachable(); }
  };

  return apply(s, Fn{*this, r});
}


Control
Evaluator::eval(Empty_stmt const* s, Value& r)
{
  return next_ctl;
}


Control
Evaluator::eval(Block_stmt const* s, Value& r)
{
  Store_sentinel store(*this);
  for (Stmt const* s1 : s->statements()) {

    // Evaluate each statement in turn. If the
    Control ctl = eval(s1, r);
    switch (ctl) {
      case return_ctl:
      case break_ctl:
      case continue_ctl:
        return ctl;
      default:
        break;
    }
  }
  return next_ctl;
}


Control
Evaluator::eval(Assign_stmt const* s, Value& r)
{
  Value lhs = eval(s->object());
  Value rhs = eval(s->value());
  *lhs.get_reference() = rhs;
  return next_ctl;
}


Control
Evaluator::eval(Return_stmt const* s, Value& r)
{
  r = eval(s->value());
  return return_ctl;
}


Control
Evaluator::eval(Return_void_stmt const* s, Value& r)
{
  return return_ctl;
}


// If the condition evaluates to true, then the body
// is evaluated.
Control
Evaluator::eval(If_then_stmt const* s, Value& r)
{
  Value c = eval(s->condition());
  if (c.get_integer())
    return eval(s->body(), r);
  return next_ctl;
}


// If the condition evaluates to true, the true branch
// is evaluated. Otherwise the false branch is evaluated.
// Note that control stops if either branch returns,
// breaks, or continues. In all other cases, control
// flows to the next statement.
Control
Evaluator::eval(If_else_stmt const* s, Value& r)
{
  Value c = eval(s->condition());
  if (c.get_integer())
    return eval(s->true_branch(), r);
  else
    return eval(s->false_branch(), r);
}


Control
Evaluator::eval(Match_stmt const* s, Value& r)
{
  return eval(s, r);
}


Control
Evaluator::eval(Case_stmt const* s, Value& r)
{
  return eval(s, r);
}


// Continue evaluationg the body while the condition
// evaluates to true.
Control
Evaluator::eval(While_stmt const* s, Value& r)
{
  while (true) {
    Value c = eval(s->condition());
    if (!c.get_integer())
      break;

    // Evaluate the body. Stop iterating if we got
    // a break, or return if we got a return.
    // Otherwise, continue to the next iteration.
    Control ctl = eval(s->body(), r);
    if (ctl == break_ctl)
      break;
    if (ctl == return_ctl)
      return ctl;
  }
  return next_ctl;
}


Control
Evaluator::eval(Break_stmt const* s, Value& r)
{
  return break_ctl;
}


Control
Evaluator::eval(Continue_stmt const* s, Value& r)
{
  return continue_ctl;
}


Control
Evaluator::eval(Expression_stmt const* s, Value& r)
{
  eval(s->expression());
  return next_ctl;
}


Control
Evaluator::eval(Declaration_stmt const* s, Value& r)
{
  eval(s->declaration());
  return next_ctl;
}


// Not implemented
Control
Evaluator::eval(Decode_stmt const* s, Value& r)
{
  return eval(s, r);
}



// Not implemented
Control
Evaluator::eval(Goto_stmt const* s, Value& r)
{
  return eval(s, r);
}


// -------------------------------------------------------------------------- //
// Expression reduction

// Return a literal corresponding to the evaluation
// of e. If e invokes undefined behavior or is not
// otherwise a constant expression, this returns nullptr.
Expr*
reduce(Expr const* e)
{
  // If the expression is already a literal, then just
  // return it.
  if (is<Literal_expr>(e))
    return const_cast<Expr*>(e);

  // Otherwise, try evaluating.
  try {
    Value v = evaluate(e);
    return new Literal_expr(e->type(), v);
  } catch (...) {
    return nullptr;
  }
}


// -------------------------------------------------------------------------- //
// Program execution

// Execute the given function.
//
// TODO: What if there are operands?
Value
Evaluator::exec(Function_decl const* fn)
{
  // Evaluate all of the top-level declarations in
  // order to re-establish the evaluation context.
  Store_sentinel store(*this);
  Module_decl const* m = cast<Module_decl>(fn->context());
  for (Decl const* d : m->declarations())
    eval(d);

  // TODO: Check the result code.
  Value result;
  Control ctl = eval(fn->body(), result);
  if (ctl != return_ctl)
    throw std::runtime_error("function error");

  return result;
}

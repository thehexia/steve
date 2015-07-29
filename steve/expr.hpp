// Copyright (c) 2015 Flowgrammable.org
// All rights reserved

#ifndef STEVE_EXPR_HPP
#define STEVE_EXPR_HPP

// The expr module defines the expressions in the language
// and various tools for working with them.

#include "steve/prelude.hpp"
#include "steve/value.hpp"
#include "steve/op.hpp"
#include "steve/convert.hpp"

#include "lingo/integer.hpp"
#include "lingo/node.hpp"


namespace steve
{

// -------------------------------------------------------------------------- //
//                              Expression kinds

// Different kinds of expressions in the core language.
enum Expr_kind
{
  id_expr,       // references to declarations
  value_expr,    // A literal or computed constant expression
  unary_expr,    // unary operations: op e
  binary_expr,   // binary operations: e1 op e2
  call_expr,     // function call: f(e1, ..., en)
  tuple_expr,    // tuples: {e1, e2, ..., en}
  index_expr,    // member access: e.n (n is a number)
  member_expr,   // member access: e.m (m is a member id)
  convert_expr,  // An implict conversion
  lengthof_expr, // lengthof e
  offsetof_expr, // offsetof e
  action_expr,   // one of the open flow actions
};


char const* get_expr_name(Expr_kind);


// Adapt the generic node-kinding template to this node type.
template<Expr_kind K>
using Expr_impl = lingo::Kind_base<Expr_kind, K>;


// -------------------------------------------------------------------------- //
//                                Expressions

// The Expr class represents the set of all expressions
// in the language.
//
// Every expression has a type. The type of an expression
// determines whether or not that expression is a valid
// sub-expression of a larger expression.
struct Expr
{
  Expr(Expr_kind k, Location l, Type const* t)
    : kind_(k), loc_(l), type_(t)
  { }

  virtual ~Expr()
  { }

  char const* node_name() const { return get_expr_name(kind_); }
  Expr_kind   kind() const      { return kind_; }
  Location    location() const  { return loc_; }
  Type const* type() const      { return type_; }

  Expr_kind   kind_;
  Location    loc_;
  Type const* type_;
};


// A reference to a declaration. The type of the expression
// is that of the referenced declaration.
struct Id_expr : Expr, Expr_impl<id_expr>
{
  Id_expr(Location, Decl const*);

  String const* name() const;
  Decl const*   decl() const { return decl_; }

  Decl const* decl_;
};


// An expression that holds a value. Note that the "shape" of
// the value (it's layout and properties) is determined by
// its type.
struct Value_expr : Expr, Expr_impl<value_expr>
{
  Value_expr(Location loc, Type const* t, Value const& v)
    : Expr(node_kind, loc, t), first(v)
  { }

  Value const& value() const { return first; }

  Value first;
};


// A unary expression applies an operator to a single argument.
//
// Note that the source location for a unary operation is the
// location of its operator.
struct Unary_expr : Expr, Expr_impl<unary_expr>
{
  Unary_expr(Location loc, Type const* t, Unary_op op, Expr const* e)
    : Expr(node_kind, loc, t), first(op), second(e)
  { }

  Unary_op    op() const  { return first; }
  Expr const* arg() const { return second; }

  Unary_op    first;
  Expr const* second;
};


// A helper class for constructing binary expressions. A binary
// expression contains the operator token and its two operands.
//
// Note that the source location of a binary expression is the
// location of its operator.
struct Binary_expr : Expr, Expr_impl<binary_expr>
{
  Binary_expr(Location loc, Type const* t, Binary_op op, Expr const* l, Expr const* r)
    : Expr(node_kind, loc, t), first(op), second(l), third(r)
  { }

  Binary_op   op() const    { return first; }
  Expr const* left() const  { return second; }
  Expr const* right() const { return third; }

  Binary_op   first;
  Expr const* second;
  Expr const* third;
};


// A function call.
//
// Note that the target of a function call is an expression.
// This is required to be resolved to a declaration during
// translation.
struct Call_expr : Expr, Expr_impl<call_expr>
{
  Call_expr(Location loc, Type const* t, Expr const* f, Expr_seq const& a)
    : Expr(node_kind, loc, t), first(f), second(a)
  { }

  Expr const*     fn() const   { return first; }
  Expr_seq const& args() const { return second; }

  Expr const* first;
  Expr_seq    second;
};


// A function call.
//
// Note that the target of a function call is an expression.
// This is required to be resolved to a declaration during
// translation.
struct Tuple_expr : Expr, Expr_impl<tuple_expr>
{
  Tuple_expr(Location loc, Type const* t, Expr_seq const& e)
    : Expr(node_kind, loc, t), first(e)
  { }

  Expr_seq const& exprs() const { return first; }

  Expr_seq    first;
};


// An index expression selects the nth element from a
// composite object. For example:
//
//    e.0 // selects the first element from e
//
// Here, `e` must have either record or tuple type with
// at least one member.
struct Index_expr : Expr, Expr_impl<index_expr>
{
  Index_expr(Location loc, Type const* t, Expr const* e, Expr const* n)
    : Expr(node_kind, loc, t), first(e), second(n)
  { }

  Expr const* object() const { return first; }
  Expr const* index() const  { return second; }

  Expr const* first;
  Expr const* second;
};


// A member expression denotes the offset (or index) within 
// the composite value of some other object. For example:
//
//    (r1.m1).m2 // record member access
//    (t1.1).2   // tuple member access
//
// We represent a member access expression as a pair of subexpressions:
// one that computes the object being accessed (possibly another
// member expression) and an id expression
struct Member_expr : Expr, Expr_impl<member_expr>
{
  Member_expr(Location loc, Type const* t, Expr const* e, Expr const* s)
    : Expr(node_kind, loc, t), first(e), second(s)
  { 
    lingo_assert(lingo::is<Id_expr>(s));
  }

  Expr const*    object() const   { return first; }
  Id_expr const* selector() const { return cast<Id_expr>(second); }

  Expr const* first;
  Expr const* second;
};


// An implicit conversion expression conerts a value in a source 
// type to a value in a target type. Conversions are not represented
// directly in the source language.
//
// The type of the expression is that of the target type.
//
// TODO: Model this as a unary expression?
struct Convert_expr : Expr, Expr_impl<convert_expr>
{
  Convert_expr(Location loc, Type const* t, Conversion_kind k, Expr const* e)
    : Expr(node_kind, loc, t), first(k), second(e)
  { }

  Conversion_kind conv() const { return first; }
  Expr const*     arg() const  { return second; }

  Conversion_kind first;
  Expr const*     second;
};


// Lengthof expression. Represents the length in bytes of an
// expression of type T. The type of a lengthof expression is
// `uint`.
//
// TODO: This expression wouldn't actually stay in a lowered
// representation of a program. It should be fully reduced
// by inlining into a single expression. It would be useful
// in a template, where we neeed to preserve non-resolvable
// expressions, but not in non-dependent contexts.
struct Lengthof_expr : Expr, Expr_impl<convert_expr>
{
  Lengthof_expr(Location loc, Type const* t, Expr const* e)
    : Expr(node_kind, loc, t), first(e)
  { }

  Expr const* arg() const  { return first; }

  Expr const* first;
};


// Offsetof expression. Represents the offset from a base
// pointer in an expression of type T. For example:
//
//    offsetof R.m
//
// Returns the offset of m in R. This is computed statically.
//
// TODO: Should we support dynamic computation? Given that
// we have a lengthof operator, we could always generate an
// expression that computes the value.
//
// TODO: This expression wouldn't actually stay in a lowered
// representation of a program. 
struct Offsetof_expr : Expr, Expr_impl<convert_expr>
{
  Offsetof_expr(Location loc, Type const* t, Expr const* e)
    : Expr(node_kind, loc, t), first(e)
  { }

  Expr const* arg() const  { return first; }

  Expr const* first;
};


enum Action_kind {
  output,
  set_queue,
  drop,
  group,
  push_tag,
  pop_tag,
  set_field
};


struct Action_expr : Expr, Expr_impl<action_expr> 
{
  Action_expr(Location loc, Type const* t, Action_kind k, Expr_seq const& args)
    : Expr(node_kind, loc, t), action_(k), first(args)
  { }

  Expr_seq const& args() const { return first; }
  Action_kind action_kind() const { return action_; }

  Action_kind action_;
  Expr_seq const first;
};


// -------------------------------------------------------------------------- //
//                           Concepts and dispatch

// True when T is models the Expression concept. 
//
// Note that we assume that a Expression is already known 
// to be Node, so we skip the explicit check.
template<typename T>
constexpr bool
is_expr()
{
  return std::is_base_of<Expr, T>::value;
}


// Apply the function to the expression `e`.
template<typename T, typename F>
auto
apply(T const* e, F fn) 
  -> typename std::enable_if<is_expr<T>(), decltype(fn((Value_expr*)0))>::type
{
  lingo_assert(is_valid_node(e));
  switch (e->kind()) {
    case id_expr: return fn(cast<Id_expr>(e));
    case value_expr: return fn(cast<Value_expr>(e));
    case unary_expr: return fn(cast<Unary_expr>(e));
    case binary_expr: return fn(cast<Binary_expr>(e));
    case call_expr: return fn(cast<Call_expr>(e));
    case tuple_expr: return fn(cast<Tuple_expr>(e));
    case index_expr: return fn(cast<Index_expr>(e));
    case member_expr: return fn(cast<Member_expr>(e));
    case convert_expr: return fn(cast<Convert_expr>(e));
    case lengthof_expr: return fn(cast<Lengthof_expr>(e));
    case offsetof_expr: return fn(cast<Offsetof_expr>(e));
    case action_expr:  return fn(cast<Action_expr>(e));
  }
  lingo_unreachable("unhandled expression '{}'", e->node_name());
}


// -------------------------------------------------------------------------- //
//                             Expression builders
//
// These functions create new expressions from their arguments. 
// To the greatest extent possible, these functions attempt to 
// resolve the type of the expression from those arguments.

Value_expr*    make_bool_expr(Location, bool);
Value_expr*    make_int_expr(Location, Integer const&);
Value_expr*    make_value_expr(Location, Type const*, Value const&);
Id_expr*       make_id_expr(Location, Decl const*);
Unary_expr*    make_unary_expr(Location, Unary_op, Expr const*);
Binary_expr*   make_binary_expr(Location, Binary_op, Expr const*, Expr const*);
Call_expr*     make_call_expr(Location, Expr const*, Expr_seq const&);
Tuple_expr*    make_tuple_expr(Location, Expr_seq const&);
Index_expr*    make_index_expr(Location, Expr const*, Expr const*);
Index_expr*    make_index_expr(Location, Member_expr const*);
Member_expr*   make_member_expr(Location, Expr const*, Expr const*);
Convert_expr*  make_convert_expr(Location, Type const*, Conversion_kind, Expr const*);
Expr*          make_lengthof_expr(Location, Expr const*);
Expr*          make_offsetof_expr(Location, Expr const*);


// Returns the boolean literal `true`.
inline Value_expr*
make_bool_expr(bool b)
{
  return make_bool_expr(Location::none, b);
}


// Returns the boolean literal `true`.
inline Value_expr*
make_true_expr()
{
  return make_bool_expr(true);
}


// Returns the boolean literal `false`.
inline Value_expr*
make_false_expr()
{
  return make_bool_expr(false);
}


// Returns an integer literal expression.
inline Value_expr*
make_int_expr(Integer const& n)
{
  return make_int_expr(Location::none, n);
}


// Returns an integer literal expression.
inline Value_expr*
make_value_expr(Type const* t, Value const& v)
{
  return make_value_expr(Location::none, t, v);
}


// Returns a declaration expression.
inline Id_expr*
make_id_expr(Decl const* d)
{
  return make_id_expr(Location::none, d);
}


// Returns a unary expression.
inline Unary_expr*
make_unary_expr(Unary_op op, Expr const* e)
{
  return make_unary_expr(Location::none, op, e);
}


// Returns a binary expression.
inline Binary_expr*
make_binary_expr(Binary_op op, Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, op, e1, e2);
}


// Make the expression `e1 + e2`.
inline Binary_expr*
make_add_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, num_add_op, e1, e2);
}


// Make the expression `e1 - e2`.
inline Binary_expr*
make_sub_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, num_sub_op, e1, e2);
}


// Make the expression `e1 * e2`.
inline Binary_expr*
make_mul_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, num_mul_op, e1, e2);
}


// Make the expression `e1 / e2`.
inline Binary_expr*
make_div_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, num_div_op, e1, e2);
}


// Make the expression `e1 % e2`.
inline Binary_expr*
make_mod_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, num_mod_op, e1, e2);
}


// Make the expression `-e`.
inline Unary_expr*
make_neg_expr(Expr const* e)
{
  return make_unary_expr(Location::none, num_neg_op, e);
}


// Make the expression '`+e`
inline Unary_expr*
make_pos_expr(Expr const* e)
{
  return make_unary_expr(Location::none, num_pos_op, e);
}


// Bitwise expressions.

inline Binary_expr*
make_bitwise_and_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, bit_and_op, e1, e2);
}


inline Binary_expr*
make_bitwise_or_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, bit_or_op, e1, e2);
}


inline Binary_expr*
make_bitwise_xor_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, bit_xor_op, e1, e2);
}


inline Unary_expr*
make_bitwise_not_expr(Expr const* e)
{
  return make_unary_expr(Location::none, bit_not_op, e);
}


inline Binary_expr*
make_bitwise_lsh_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, bit_lsh_op, e1, e2);
}


inline Binary_expr*
make_bitwise_rsh_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, bit_rsh_op, e1, e2);
}


// Relational expressions

// Make the expression `e1 == e2`.
inline Binary_expr*
make_eq_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, rel_eq_op, e1, e2);
}


inline Binary_expr*
make_ne_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, rel_ne_op, e1, e2);
}


inline Binary_expr*
make_lt_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, rel_lt_op, e1, e2);
}


inline Binary_expr*
make_gt_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, rel_gt_op, e1, e2);
}


inline Binary_expr*
make_le_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, rel_le_op, e1, e2);
}


inline Binary_expr*
make_ge_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, rel_ge_op, e1, e2);
}


// Logical expressions

// Make the expression `e1 && e2`.
inline Binary_expr*
make_logical_and_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, log_and_op, e1, e2);
}


// Make the expression `e1 || e2`.
inline Binary_expr*
make_logical_or_expr(Expr const* e1, Expr const* e2)
{
  return make_binary_expr(Location::none, log_or_op, e1, e2);
}


// Make the expression `!e`.
inline Unary_expr*
make_logical_not_expr(Expr const* e)
{
  return make_unary_expr(Location::none, log_not_op, e);
}


// TODO: Finish building out constructors for these expressions.
// Don't forget to add overloads that take constructors.


inline Call_expr*
make_call_expr(Expr const* f, Expr_seq const& a)
{
  return make_call_expr(Location::none, f, a);
}


inline Tuple_expr*
make_tuple_expr(Expr_seq const& e)
{
  return make_tuple_expr(Location::none, e);
}


// Make an index expression.
inline Index_expr*
make_index_expr(Expr const* e, Expr const* n)
{
  return make_index_expr(Location::none, e, n);
}


// Make an index expression from a member expression.
inline Index_expr*
make_index_expr(Member_expr const* m)
{
  return make_index_expr(Location::none, m);
}


// Make a member expression.
inline Member_expr*
make_member_expr(Expr const* e, Expr const* s)
{
  return make_member_expr(Location::none, e, s);
}


inline Convert_expr*
make_convert_expr(Type const* t, Conversion_kind k, Expr const* e)
{
  return make_convert_expr(Location::none, t, k, e);
}


inline Expr*
make_lengthof_expr(Expr const* e)
{
  return make_lengthof_expr(Location::none, e);
}


inline Expr*
make_offsetof_expr(Expr const* e)
{
  return make_offsetof_expr(Location::none, e);
}


// -------------------------------------------------------------------------- //
//                                 Queries

bool is_integer_expr(Expr const*);
bool is_boolean_expr(Expr const*);

bool has_record_type(Expr const*);
bool has_enum_type(Expr const*);


// -------------------------------------------------------------------------- //
//                                  Facilities

// Garbage collection
void mark(Expr const*);


} // namespace steve

#endif

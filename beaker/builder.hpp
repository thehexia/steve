#ifndef BUILDER_HPP
#define BUILDER_HPP

#include "token.hpp"
#include "expr.hpp"
#include "type.hpp"
#include "stmt.hpp"


inline Id_expr*
id(Decl* d)
{
  assert(d->type());
  return new Id_expr(d->name());
}


inline Decl_expr*
decl_id(Decl* d)
{
  assert(d->type());
  return new Decl_expr(d->type(), d);
}


// block statements
inline Block_stmt*
block(Stmt_seq const& s)
{
  return new Block_stmt(s);
}


// Literal zero
inline Expr*
zero()
{
  return new Literal_expr(get_integer_type(), 0);
}


// Literal one
inline Expr*
one()
{
  return new Literal_expr(get_integer_type(), 1);
}


// Make an arbitrary integer literal
inline Expr*
make_int(int n)
{
  return new Literal_expr(get_integer_type(), n);
}


// ----------------------------------------------------- //
//      Expression building

// Add
inline Expr*
add(Expr* a, Expr* b)
{
  return new Add_expr(a, b);
}


// subtract
inline Expr*
sub(Expr* a, Expr* b)
{
  return new Sub_expr(a, b);
}


// divide
inline Expr*
div(Expr* a, Expr* b)
{
  return new Div_expr(a, b);
}


// multiply
inline Expr*
mul(Expr* a, Expr* b)
{
  return new Mul_expr(a, b);
}


// ----------------------------------------------------- //
//      Function building




#endif

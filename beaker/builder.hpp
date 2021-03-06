#ifndef BUILDER_HPP
#define BUILDER_HPP

#include "beaker/token.hpp"
#include "beaker/expr.hpp"
#include "beaker/type.hpp"
#include "beaker/stmt.hpp"


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


inline Return_void_stmt*
return_void()
{
  static Return_void_stmt s;
  return &s;
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


// make an arbitrary c string literal
inline Expr*
make_cstr(char const* str)
{
  Array_value v {
     str,
     strlen(str)
  };

  // Create the extent of the literal array. This is
  // explicitly more than the length of the string,
  // and includes the null character.
  Type const* z = get_integer_type();
  Expr* n = new Literal_expr(z, v.len + 1);

  // Create the array type.
  Type const* c = get_character_type();
  Type const* t = get_array_type(c, n);

  return new Literal_expr(t, v);
}


inline Expr*
invalid_port()
{
  static Literal_expr port0(get_port_type(), 0);
  return &port0;
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


inline Expr*
expr_to_void_block(Expr* v)
{

  Expr* cast = new Void_cast(v);
  cast->type_ = get_character_type()->ref();
  return cast;
}

// Value conversion
inline Value_conv*
value_conv(Type const* t, Expr* e)
{
  return new Value_conv(t, e);
}


// Variadic conv.
inline Variadic_conv*
variadic_conv(Type const* t, Expr* e)
{
  return new Variadic_conv(t, e);
}

// ----------------------------------------------------- //
//      Decl building


inline Variable_decl*
temp_var(Symbol_table& syms, Type const* t, Expr* init)
{
  static int count = 0;
  std::string s = "reserved." + std::to_string(count++);
  Symbol const* name = syms.put<Identifier_sym>(s, identifier_tok);
  return new Variable_decl(name, t, new Copy_init(t, init));
}


inline Variable_decl*
var(Symbol const* n, Type const* t, Expr* init)
{
  return new Variable_decl(n, t, new Copy_init(t, init));
}


// ----------------------------------------------------- //
//      Statement building

inline Declaration_stmt*
statement(Decl* d)
{
  return new Declaration_stmt(d);
}


inline Expression_stmt*
statement(Expr* e)
{
  return new Expression_stmt(e);
}


// ----------------------------------------------------- //
//      Function building


#endif

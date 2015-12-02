#include "steve/prelude.hpp"
#include "steve/builder.hpp"
#include "steve/builtin.hpp"
#include "steve/lookup.hpp"
#include "steve/net.hpp"
#include "steve/lower.hpp"
#include "steve/translate/translate.hpp"
#include "steve/translate/translate-pipeline.hpp"


namespace steve
{

// Record_decl*
// make_eth_header()
// {
//   Decl_seq mem {
//     make_member("src", get_umsbf_type(48)),
//     make_member("dst", get_umsbf_type(48)),
//     make_member("proto", get_umsbf_type(16)),
//   };

//   return make_record("eth", mem);
// }

Record_decl*
make_eth_header()
{
  Decl_seq mem {
    make_int_member("src"),
    make_int_member("dest"),
    make_int_member("protocol"),
  };

  return make_record("eth", mem);
}

Record_decl*
make_ipv4_header()
{
  Decl_seq mem {
    make_int_member("src"),
    make_int_member("dest"),
    make_int_member("protocol"),
  };

  return make_record("ipv4", mem);
}

Record_decl*
make_real_ipv4_header()
{
  Decl_seq mem {
    make_member("version_ihl", get_umsbf_type(8)),
    make_member("dscp_ecn", get_umsbf_type(8)),
    make_member("len", get_umsbf_type(16)),
    make_member("id", get_umsbf_type(16)),
    make_member("frag", get_umsbf_type(16)),
    make_member("ttl", get_umsbf_type(8)),
    make_member("protocol", get_umsbf_type(8)),
    make_member("checksum", get_umsbf_type(16)),
    make_member("src", get_umsbf_type(32)),
    make_member("dst", get_umsbf_type(32)),
  };

  return make_record("ipv4", mem);
}

Function_decl*
make_fn1()
{
  Decl_seq parms {
    make_parm("n", get_int_type()),
    make_parm("b", get_bool_type())
  };
  return make_fn("f1", parms);
}


Stmt*
make_do(Do_kind k, Decode_decl const* d)
{
  return make_expr_stmt(make_do_expr(k, id(d)));
}


Stmt*
make_do(Do_kind k, Table_decl const* d)
{
  return make_expr_stmt(make_do_expr(k, id(d)));
}


Program
test1()
{
  init_builtins();

  Program program;

  // make the headers
  Record_decl* eth = make_eth_header();
  Record_decl* ipv4 = make_ipv4_header();


  // make some field expressions
  Expr* eth_src = make_field_expr(id(eth), id(eth->members()[0]));
  Expr* eth_dst = make_field_expr(id(eth), id(eth->members()[1]));
  Expr* eth_type = make_field_expr(id(eth), id(eth->members()[2]));

  Expr* ipv4_src = make_field_expr(id(ipv4), id(ipv4->members()[0]));
  Expr* ipv4_dst = make_field_expr(id(ipv4), id(ipv4->members()[1]));
  Expr* ipv4_proto = make_field_expr(id(ipv4), id(ipv4->members()[2]));

  //----------------------------------------------------------------//
  //              Forward Declarations

  // construct with no flows to begin with
  Table_decl* t1 = make_table_decl(get_identifier("t1"), { eth_src, eth_dst, ipv4_dst }, {});
  // Table_decl* t2 = make_table_decl(get_identifier("t2"), { eth_src, eth_dst, ipv4_proto}, {});
  declare(t1->name(), t1);
  // declare(t2->name(), t2);

  // construct two decoders with no definitions yet
  Decode_decl* eth_d = make_decode_decl(get_identifier("eth_d"), get_record_type(eth), nullptr);
  Decode_decl* ipv4_d = make_decode_decl(get_identifier("ipv4_d"), get_record_type(ipv4), nullptr);

  eth_d->set_start();
  declare(eth_d->name(), eth_d);
  declare(ipv4_d->name(), ipv4_d);  

  //----------------------------------------------------------------//
  //              Definitions

  Variable_decl* _header_1 = make_variable_decl(
                                get_identifier("_header_"), 
                                get_record_type(eth));

  Variable_decl* _header_2 = make_variable_decl(
                                get_identifier("_header_"), 
                                get_record_type(ipv4));

  Expr* cond1 = make_member_expr(id(_header_1), id(eth->members()[2]));
  Expr* cond2 = make_member_expr(id(_header_2), id(ipv4->members()[2]));

  Stmt_seq eth_d_body {
    make_decl_stmt(make_extracts_decl(eth_src)),
    make_decl_stmt(make_extracts_decl(eth_dst)),
    make_decl_stmt(make_extracts_decl(eth_type)),
    make_match_stmt(cond1, 
      Stmt_seq {
        make_case(zero(), make_do(Do_kind::decode, ipv4_d)),
        make_case(one(), make_do(Do_kind::decode, ipv4_d)),
      }
    )
  };


  Stmt_seq ipv4_d_body {
    make_decl_stmt(make_extracts_decl(ipv4_src)),
    make_decl_stmt(make_extracts_decl(ipv4_dst)),
    make_match_stmt(cond2, 
      Stmt_seq {
        make_case(zero(), make_do(Do_kind::table, t1)),
        make_case(one(), make_do(Do_kind::decode, eth_d)),
      }
    )
  };

  // define the decoders
  declare(eth_d->name(), make_decode_decl(get_identifier("eth_d"), get_record_type(eth), block(eth_d_body)));
  declare(ipv4_d->name(), make_decode_decl(get_identifier("ipv4_d"), get_record_type(ipv4), block(ipv4_d_body)));

  // define the tables
  Decl_seq flows {
    // cond expr, prio, stmt
    make_flow_decl({one(), two(), one()}, Value(1), make_block_stmt({})),
    make_flow_decl({one(), two(), one()}, Value(1), make_block_stmt({}))
  };
  declare(t1->name(), make_table_decl(get_identifier("t1"), { eth_src, eth_dst, ipv4_dst }, flows));
  // declare(t2->name(), make_table_decl(get_identifier("t2"), { eth_src, eth_dst, ipv4_proto }, flows));

  // lowering has to happen in reverse as well
  // FIXME: every Decode_decl should cause a forward-decl
  // for the lowered function first
  // auto lower_eth_d = lower_decodes(eth_d);
  // auto lower_ipv4_d = lower_decodes(ipv4_d);

  // IMPORTANT: the call expressions in their lowered form
  // may actually point to the decode-decl rather than it's lowered
  // function form. This should still be fine as the call is opaque during
  // translation to C++ and the declaration it points to should not matter
  // as long as the name is correct.

  // make the program
  program.push(statement(eth));
  program.push(statement(ipv4));
  program.push(statement(make_forward_decl(eth_d)));
  program.push(statement(make_forward_decl(ipv4_d)));
  program.push(statement(make_forward_decl(t1)));
  // program.push(statement(make_forward_decl(t2)));
  program.push(statement(t1));
  // program.push(statement(t2));
  program.push(statement(eth_d));
  program.push(statement(ipv4_d));

  return program;
}

void translate_decode1()
{
  Program p = test1();
  Translator t(p);

  t.codegen();
}

void test_headers()
{
  std::string eth = tostring(translate(steve::make_eth_header()));
  std::string ipv4 = tostring(translate(steve::make_real_ipv4_header()));

  std::string fn1 = tostring(translate(steve::make_fn1()));

  std::cout << eth;
  std::cout << ipv4;
  std::cout << fn1;
}

} // namespace steve

int main()
{
  using namespace steve;

  steve::Global_scope global;

  // test_headers();
  // translate_fn();
  translate_decode1();
}


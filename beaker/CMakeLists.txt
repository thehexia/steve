# Copyright (c) 2015 Andrew Sutton
# All rights reserved

# Generate the configuration header.
configure_file(config.hpp.in config.hpp)

# Add the core Beaker library.
add_library(beaker
  file.cpp
  line.cpp
  location.cpp
  symbol.cpp
  expr.cpp
  type.cpp
  decl.cpp
  specifier.cpp
  value.cpp
  integer.cpp
  print.cpp
  less.cpp
  convert.cpp
  error.cpp
  token.cpp
  lexer.cpp
  parser.cpp
  environment.cpp
  scope.cpp
  overload.cpp
  elaborator.cpp
  overload.cpp
  evaluator.cpp
  mangle.cpp
  generator.cpp
  builder.cpp
  length.cpp
  offset.cpp
  gather.cpp
  pipeline.cpp
  builtin.cpp
  lower.cpp
  actions.cpp
  job.cpp
)
target_compile_definitions(beaker PUBLIC ${LLVM_DEFINITIONS})
target_include_directories(
  beaker
    PUBLIC
      "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR};${PROJECT_BINARY_DIR}>"
      ${lingo_SOURCE_DIR}
      ${Boost_INCLUDE_DIRS}
      ${LLVM_INCLUDE_DIRS}
)
target_link_libraries(
  beaker
    PUBLIC
      lingo
      ${Boost_LIBRARIES}
      ${LLVM_LIBRARIES}
)

# The compiler is the main driver for compilation.
# It takes a set of input files and produces linked
# outputs (programs, libraries, archives).
add_executable(beaker-compile driver.cpp beaker-compiler.cpp)
target_link_libraries(beaker-compile beaker)

# The runtime interpreter executes a parsed beaker
# program without compiling to native code.
add_executable(beaker-interpret interpreter.cpp)
target_link_libraries(beaker-interpret beaker)

add_subdirectory(steve-compiler)

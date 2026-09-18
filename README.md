# Ocaml-bytecode-interpreter

An interpreter for a custom stack-based bytecode language, built in OCaml
as an individual project for an Introduction to Programming Languages course.

## Features
- Stack-based execution of push/pop and arithmetic/logical operations
- Immutable variable bindings and lexically scoped `let` blocks
- First-class functions with closures, supporting recursion and
  higher-order functions (e.g. curried functions)
- Type-checked error handling (`:error:`) across all operations

## How it works
The interpreter reads a program (a list of commands) from an input file,
executes it against an internal stack and environment, and writes any
output to a specified output file.

The interpreter is in part1 folder.

# MiniRust

It is a school project. I partially implemented a borrow checker and a backend generating
C code for a subset of Rust. The borrow checker does not perfectly work and the backend is
quite naive.

## Usage

There is a script that means to generate a C version of a given MiniRust program. It runs
the typechecker and the borrow checker before generating anything.

```bash
$ C=1 minirust/minirust.exe your_code.rs
```

## Results

On 94 tests, this implementations passes 91 of them. In the 3 that do not check, 2 do
not return an error but should, and 1 returns an error but should not.

## Extension : C backend

I made a simple C backend that translates MiniMir programs to C programs. The translation
of instructions from MiniMir to C is linear and does not need any specific algorithm. As
a consequence, the generated C program is not idiomatic and uses a lot of labels. But
since the backend assumes that the MiniMir source program is correct, the C target program
should be correct too. The only tricky part of the backend resides in the resolution of
structures direct dependencies with each other.

The algorithm behind is quite straightforward :
  - I compute the structures' dependency graph
  - For each connected component C of it :
    - I find the least dependent structure S in C
    - I perform a postfix DFS on C, starting from S

Every structure that has been already emitted by the backend is marked, in order not to
emit it several times.

The algorithm computes the **direct** dependencies between structures. So if one structure
uses another structure that has not been defined yet, through an indirection, there is
nothing tricky to think of and I just generate the corresponding C structure definition.
However, structures prototypes must be provided at the very beginning of the C program.

Detail : The reference implementation of MiniRust seems to compile when the program
contains cyclic structures dependencies. I took the freedom not to take it into account
in my implementation.

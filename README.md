# MiniRust

It is a school project. I partially implemented a borrow checker and a backend generating
C code for a subset of Rust. The borrow checker does not perfectly work and the backend is
quite naive.

## Results

On 94 tests, this implementations passes 91 of them. In the 3 that do not check, 2 do
not return an error but should, and 1 returns an error but should not.

## Extension : C backend

I made a simple C backend that translates MiniMir programs to C programs. The only tricky
part of the backend resides in the resolution of structures direct dependencies with each
other.

The algorithm behind is quite straightforward :
  - I compute the structures' dependency graph
  - For each connected component C of it :
    - I find the least dependent structure S in C
    - I perform a postfix DFS on C, starting from S

Every structure that has been already emitted by the backend is marked, in order not to
emit it several times.

Detail : The reference implementation of MiniRust seems to compile when the program
contains cyclic structures dependencies. I took the freedom to prohibit it in my
implementation, because I did not see any way of generating it in C with a simple backend.

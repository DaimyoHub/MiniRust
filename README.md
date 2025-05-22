# MiniRust

It is a school project. I partially implemented a borrow checker and a backend generating
C code for a subset of Rust. The borrow checker does not perfectly work and the backend is
quite naive.

## Results

On 94 tests, this implementations passes 91 of them. In the 3 that do not check, 2 do
not return an error but should, and 1 returns an error but should not.

struct D {
  x : B,
  y : A,
  z : E
}

struct E {
  x : D,
  y : B
}

struct B { 
  x : A,
  y : C
}

struct C {
  x : A
}

struct A { }

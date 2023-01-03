var c func() int;

func foo() {
  i := 0;
  c = func() int {
    i++;
    return i;
  };
}

func main() {
  foo();

  println(c());
  println(c());
  println(c());
}

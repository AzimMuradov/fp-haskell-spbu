func main() {
  var n = 5;

  if n == 3 + 2 {
    println(n);
  };

  if n == 3 + 2 {
    println(n);
  } else {
    println(42);
  };

  if n == 3 + 2 {
    println(n);
  } else if n == 2 {
    println(n * n);
  };

  i := 1;

  for i := 0; i < 17; i++ {
    println(i * i * i);
  };

  println(i);

  for false {
    println();
  };

  for true {
    println("true");
    break;
  };
}

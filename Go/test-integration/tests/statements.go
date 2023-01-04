func main() {
  var n = 5;


  /* if - else */

  // should print "5"
  if n == 3 + 2 {
    println(n);
  };

  // should print nothing
  if n == 73 {
    println(n);
  };

  // should print "42"
  if n != 3 + 2 {
    println(n);
  } else {
    println(42);
  };

  // should print "25"
  if n == 3 + 3 {
    println(n);
  } else if n == 6 - 1 {
    println(n * n);
  };


  /* for */

  i := 1;

  for i := 0; i < 17; i++ {
    println(i * i * i);
  };

  println(i);

  i := 2;
  for i < 5 {
    println(i);
    i++;
  };
  println(i);

  for {
    println("true");
    break;
  };
}

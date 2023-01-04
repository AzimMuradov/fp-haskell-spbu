func main() {
  var n = 5;

  println(fact(n - 1));  // should print "24"
  println(fact(n * 2));  // should print "3628800"
  println(fact(n));      // should print "120"
  println(fib(n));       // should print "8"
  println(foo(1000));    // should print "0"
  println(bar(1000));    // should print "0"
  println(fib(fact(3))); // should print "13"
}


/* Factorial */

func fact(n int) int {
  if n == 0 || n == 1 { return 1; };
  return n * fact(n - 1);
}


/* Fibonacci numbers */

func fib(n int) int {
  if n == 0 || n == 1 {
    return 1;
  };
  return fib(n - 1) + fib(n - 2);
}


/* Mutual recursion */

func foo(n int) int {
  if n == 0 { return 0; };
  return bar(n - 1);
}

func bar(n int) int {
  if n == 0 { return 0; };
  return foo(n - 1);
}

func main() {
  var n = 5;

  printlnInt(fact(n - 1));  // should print 24
  printlnInt(fact(n * 2));  // should print 3628800
  printlnInt(fact(n));      // should print 120
  printlnInt(fib(n));       // should print 8
  ret();                    // should print 1 and then 2
  printlnInt(100000);       // should print 100000
  printlnInt(-10000000);    // should print -10000000
  printlnInt(foo(1000));    // should print 0
  printlnInt(bar(1000));    // should print 0
  printlnInt(fib(fact(3))); // should print 13
}


/* Return for short-circuit */

func ret() {
  printlnInt(1);
  printlnInt(2);
  return;
  printlnInt(3);
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

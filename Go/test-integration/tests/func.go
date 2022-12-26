func main() {
  var a = func() int { return 42; };

  println(a);   // should print "function"
  println(a()); // should print "42"
}

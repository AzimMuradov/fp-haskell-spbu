func main() {
  var a = [6] int {4, 8, 15, 16, 23, 42};

  println(len(a)); // should print "6"
  println(a);      // should print "[4 8 15 16 23 42]"
  println(a[3]);   // should print "16"
  println(a[10]);  // should panic with "panic: runtime error: index out of range"
}

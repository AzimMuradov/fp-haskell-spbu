func main() {
  var a = false;
  var b = true;

  println(!a);     // should print "true"
  println(!b);     // should print "false"
  println(a || b); // should print "true"
  println(a && b); // should print "false"
  println(a == b); // should print "false"
  println(a != b); // should print "true"
}

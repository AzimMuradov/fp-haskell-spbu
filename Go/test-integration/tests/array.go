func main() {
  var a = [8] int {1, 2, 3};

  println(len(a)); // should print "8"
  println(a);      // should print "[1 2 3 0 0 0 0 0]"
  println(a[2]);   // should print "3"
  println(a[5]);   // should print "0"

  a[5] = 100;
  println(a[5]);   // should print "100"


  var b = [3][3] int {[3] int {1, 2, 3}, [3] int {4, 5, 6}, [3] int {7, 8, 9}};

  println(len(b));  // should print "3"
  println(b);       // should print "[[1 2 3] [4 5 6] [7 8 9]]"
  println(b[1]);    // should print "[4 5 6]"
  println(b[0][2]); // should print "3"

  b[0] = [3] int {10, 20, 30};
  println(b[0]);    // should print "[10 20 30]"

  b[1][1] = 777;
  println(b[1][1]); // should print "777"


  var c = [6] int {4, 8, 15, 16, 23, 42};

  println(len(c)); // should print "6"
  println(c);      // should print "[4 8 15 16 23 42]"
  println(c[3]);   // should print "16"
  println(c[10]);  // should panic with "panic: runtime error: index out of range [10] with length 6"
}

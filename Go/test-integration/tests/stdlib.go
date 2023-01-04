func main() {
  print(1, " ", "2", 3, " ", 4);                    // should print "1 23 4" (without '\n')
  println(2, [4] string {"x", "y", "z", "."}, nil); // should print "2 [x y z .] nil"
  println(len("alpha"));                            // should print "5"
  println(len([19] int {}));                        // should print "19"
  panic("scary error");                             // should panic with "panic: scary error"
}

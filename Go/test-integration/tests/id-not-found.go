// should fail with "panic: analyzer error: identifier b not found"

func main() {
  var a int;

  {
    println(a); // should print nothing (because of analyzer failure)
  };

  println(a);   // should print nothing (because of analyzer failure)
  println(b);   // should print nothing (because of analyzer failure)
}

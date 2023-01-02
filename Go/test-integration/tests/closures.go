func intSeq() [2] func() int {
  i := 0;
  a := func() int {
    i++;
    return i;
  };
  b := func() int {
    i++;
    return i;
  };
  i++;
  a();
  return [2] func() int {a, b};
}

func main() {
  nextInt := intSeq();

  println(nextInt[0]());
  println(nextInt[1]());
  println(nextInt[1]());

  newInts := intSeq();
  println(newInts[0]());
}

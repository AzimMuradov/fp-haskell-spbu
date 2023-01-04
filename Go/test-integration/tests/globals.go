var i = 5;

var a = [10] int {1, 2, 3};

func main() {
  println(i);

  inc();
  println(i);

  inc();
  println(i);

  inc();
  println(i);


  println(a);

  swap(0, 5);
  println(a);
}

func inc() { i++; }

func swap(i int, j int) {
  var temp = a[i];
  a[i] = a[j];
  a[j] = temp;
}

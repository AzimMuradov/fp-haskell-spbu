var g = 5;

func main() {
  println(g);

  inc();
  println(g);

  inc();
  println(g);

  inc();
  println(g);
}

func inc() { g++; }

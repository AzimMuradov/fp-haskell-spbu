// should fail with "panic: analyzer error: identifier b redeclared"

func main() {
  var a int;

  {
    var a int;
  };

  var b int;
  var b int;
}

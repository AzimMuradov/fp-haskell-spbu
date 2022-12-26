func main() {
  println(-0x8000000000000000);                                 // should print "-9223372036854775808"
  println(-(0x800_00000_00000000));                             // should print "-9223372036854775808"
  println(-(0 + 1) * (0x800_00000_00000000));                   // should print "-9223372036854775808"
  println(-(0 + 1 + 1 - 3 / (2 + 1)) * (0x800_00000_00000000)); // should print "-9223372036854775808"
  // println(0x8000000000000000);                               // should fail
  // println(0xEEE * 0xEEE * 0xEEE * 0xEEE * 0xEEE * 0xEEE);    // should fail
}

func memoize(f func(int) int) func(int) int {
	mem := [10] int {};
	return func(x int) int {
    y := mem[x];
		if y != 0 {
			return y;
		};
		y := f(x);
		mem[x] = y;
		return y;
	};
}

func memoRec2(fOpenRec func(func(int) int, int) int) func(int) int {
	var f func(int) int;
	fRecMemo := memoize(func(x int) int {
		return fOpenRec(f, x);
	});
	f = fRecMemo;
	return fRecMemo;
}

func main() {
	f := memoRec2(func(f func(int) int, x int) int {
		if x == 0 {
			return 0;
		};
		return f(x-1) + 1;
	});
	println(f(5));
}

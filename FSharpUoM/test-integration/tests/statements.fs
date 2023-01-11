let x = 4;
let y = x * 2;
let m = x + y;
m;; // Right: "12"

let f x = x + 1;; // Right: "int -> int"

let f x = begin
    2 + 3;
    let p = x * 2 in 
        p + 2;
    let g m = m * 10 in
        g x
end;
f 10;; // Right: "100"

let rec fib y n = if n <= 1 
    then 1 
    else fib (n - 1) y + fib (n - 2) y; 

fib 1;; // Right: "int -> int"

[<Measure>] type m;; 
// Right: 
// "[<Measure>]
// type m = m"

[<Measure>] type m;
[<Measure>] type sec;
[<Measure>] type speed = m / sec;
3<m> / 2<sec> + 2<speed>;; // Right: "3<m/(sec)>"




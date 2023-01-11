let rec fact n = if n <= 1 then 1 else n * fact (n - 1);
fact 5;; // Right: "120"

let n = 15;
let rec fact n = if n <= 1 then 1 else n * fact (n - 1);
fact (n - 2);; // Right: "6227020800"

let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);
fib 5;; // Right: "8"

let n = 30;
let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);
fib (n - 2);; // Right: "514229"

let rec f x = if x = 0 then 0 else begin
    let g x = if x = 0 then 0 else f (x - 1) in
        g (x - 1)
end;
f 5;; // Right: "0"
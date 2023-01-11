let x = 4;
let f x = x + 1;
f 5;; // Right: "6"

let x = 4;
let f y = begin
    let x = 10 in 
        x * 10
end;
f 0;; // Right: "100"

let x = 4;
let f y = x + 1;
let g x = f x;
g 7;; // Right: "5"

let x = 4;
let x = 5;; // Right: "Duplicate definition of value 'x'"

let x = 4;
let x y = 2;; // Right: "Duplicate definition of value 'x'"

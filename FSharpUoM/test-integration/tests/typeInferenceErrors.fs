let x = 2; 
let x = 10;; // Right: "Duplicate definition of value 'x'"

let x = 2;
let x y = y * 10;; // Right: "Duplicate definition of value 'x'"

[<Measure>] type m;
[<Measure>] type m;; // Right: "Duplicate definition of measure type 'm'"

let f x = y + x;; // Right: "Unbound variable 'y'"

let f x = f (x - 1);; // Right: "Unbound variable 'f'"

1<m> + 1<cm>;; // Right: "Measure 'm' do not define"

1 && (false || true);; // Right: "It is not possible to apply this operation between 'int' and 'bool'"

1 + 2.9;; // Right: "The type 'int' does not match the type 'double'"

[<Measure>] type m;
1.0<m> + 1<m>;; // Right: "The type 'double<m>' does not match the type 'int<m>'"

[<Measure>] type m;
[<Measure>] type cm;
1<m> + 1<cm>;; // Right: "The type 'm' does not match the type 'cm'"

[<Measure>] type m;
[<Measure>] type cm;
1<m> * 1<cm> + 1<m>;; // Right: "The type 'cm m' does not match the type 'm'"
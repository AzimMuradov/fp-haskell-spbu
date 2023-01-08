[<Measure>] type m;;
// Right: 
// [<Measure>]
// type m = m 


[<Measure>] type sec;
[<Measure>] type smt = sec^-3;

let m = (5<sec> * 12<sec> + 44<sec^2>) / 1<sec^5>;
m + 12<smt>;; // Right: "116</(sec^-3)>"

[<Measure>] type kg;
let f (x:int<kg>) = x + 15<kg>;; // Right: "int<kg> -> int<kg>"

// Physical formulas

[<Measure>] type vatt;
[<Measure>] type sec;
[<Measure>] type joule;


let Awork = 10<joule>;
let Ttime = 5<sec>;
let Npower = Awork / Ttime;; // Right: "2<joule/(sec)> "

[<Measure>] type kg;
[<Measure>] type m;
[<Measure>] type H;

let g = 9.8<H/kg>;
let mass = 2542.2423<kg>;
let h = 25687.123454<m>;
let PEnergy = g * mass * h;
let smt = PEnergy / 2.5;; // Right: "2.5598733589551717e8<H m>"
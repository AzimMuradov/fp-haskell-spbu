let x = false;
not x;; // Right: "true"

let x = true;
not x;; // Right: "false"

let x = false;
let y = true;
x && y;; // Right: "false"

let x = false;
let y = true;
x || y;; // Right: "true"

let x = false;
let y = true;
x = y;; // Right: "false"

let x = false;
let y = true;
1 <> 2;; // Right: "true"

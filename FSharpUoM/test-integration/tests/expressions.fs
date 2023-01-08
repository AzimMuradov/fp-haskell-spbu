let x = fun x y -> x + y;
let m = x 5;
let v = m 2;; // Right: "7"

let x = 10;
if x >= 10 then begin
    2 + 4;
    let f x = x * -15 in
        begin 
            if 2 > 4 then 1 else 4 + 2;
            f 10
        end
    end
    else
        begin
            let m = 10 in
                let v = 15 in
                    m * 5 + v;
            155
        end
;; // Right: "-150"

let f (x:int) (y:double) z = y ** x in f 4 12.0 true;; // Right: "20736.0"

let x = (fun (y:int) (z:double) m -> z ** m) in x true 2 5;; 
// Right: "The type 'int' does not match the type 'bool'"

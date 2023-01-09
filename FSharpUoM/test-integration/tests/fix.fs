let rec fix f x = f (fix f) x; 
fix (fun re n -> if n <= 1 then 1 else n * re (n - 1)) 5;;

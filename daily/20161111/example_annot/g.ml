let rec g x r = if x == 0 then r else g (x - 1) (r * x)

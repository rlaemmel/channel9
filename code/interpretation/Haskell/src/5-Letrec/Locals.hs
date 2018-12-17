-- Fixed-point combinator

fix :: (x -> x) -> x
fix f = f (fix f)


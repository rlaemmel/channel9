instance Show Value where
  show (Num i) = show i
  show (Fun _) = "<function>"


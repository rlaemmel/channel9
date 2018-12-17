-- Partial function application

infixr 0 $$
($$) :: (a -> Maybe b) -> Maybe a -> Maybe b
($$) = maybe Nothing


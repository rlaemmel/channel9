termK 
 = Add (Con 1)
       (Callcc "k"
               (Add (Con 2)
                    (App (Var "k") (Con 4))))

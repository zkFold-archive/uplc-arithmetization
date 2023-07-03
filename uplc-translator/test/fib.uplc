(program
  2.0.0
    [ [ (builtin equalsInteger) 
    
        (con integer 55) ]
    
        [ [ [ [   (lam fn [fn fn])
                (lam g (lam f0 (lam f1 (lam i
                        (force [ [ [ (force (builtin ifThenElse)) 
                                [ [ (builtin lessThanEqualsInteger) i] (con integer 0) ] ]
                                (delay f0) ]
                                (delay [ [ [ [ g g ] 
                                                f1 ] 
                                                [ [ (builtin addInteger) f0 ] f1 ] ] 
                                                [ [ (builtin subtractInteger) i ] (con integer 1) ] ]) ])
                ))))
            ]
        (con integer 0) ] 
        (con integer 1) ] 
        (con integer 10) ] 

    ]
)
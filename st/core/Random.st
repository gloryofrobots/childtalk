Random extend [
   
    new [
        ^self basicNew initialize
    ]
    
    
    initialize [
        seed := 0
    ]
    
    
    seed [
        ^seed
    ]
    
    seed: anInteger [
        seed := anInteger
    ]
    
    
    next [
        "This is an implementation of the Lehmer's linear congruential method"
        | temp |
        temp := 0.
        [ temp = 0 ]
            whileTrue: [
                seed := 13849 + (27181 * seed) bitAnd: 65535.
                temp := seed / 65536.0 ].
        ^temp
    ]
]
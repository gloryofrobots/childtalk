"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"


Stream subclass: Random [
     |seed|
     
    (+) new [
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
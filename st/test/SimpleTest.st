Object subclass: A [
    |a1 a2|
    
    (+) new [
        ^ self basicNew initialize:4.
    ]
    
    initialize: aNumber [
        a1 := aNumber.
        a2 := a1 + 4.
        ^ self
    ]
]

A subclass: B [
    |b1 b2|
    
    call [
        b1 := 1.
        b2 := 4.
        ^ a1 + b2 + a2 + b1.
    ]
]

TestCase subclass: SimpleTest [
    test [
        self testSimpleBlock.
      
    ]
   
    testSimpleBlock [
        |res|
        res := [:x |  x + 1] value: 2
        self assertTrue: (res = 3) withMessage: 'should answer 3.'.
    ]
]
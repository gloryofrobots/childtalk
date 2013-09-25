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

Object subclass: ArgTest [
    sum: arg1 with: arg2 with: arg3 [
        ^ (arg1 + arg2 + arg3).
    ]
]


TestCase subclass: CommonTest [
    doTest [
        self testMethods.
        self testBlocks.
        self testPrimitiveNumbers.
        self testExceptions.
        self testMultipleArguments.
    ]
    
    "-------------------------------------"
    testMultipleArguments [
        |argTest|
        argTest := ArgTest new.
        self assertTrue:((argTest sum:40 with:1 with:1) = 42) withMessage: 'ArgTest sum:40 with:1 with 1 = 42'.
        self assertTrue:((argTest perform:#sum:with:with: with:40 with:1 with:1) = 42) withMessage: 'ArgTest perform:#sum:with:with with:40 with:1 with:1 = 42'.
        self assertTrue:((argTest perform:'sum:with:with:' with:40 with:1 with:1) = 42) withMessage: 'ArgTest perform:"sum:with:with" with:40 with:1 with:1 = 42'.
        self assertTrue:((argTest perform:#sum:with:with: withArguments:#(40 1 1)) = 42) withMessage: 'ArgTest perform:#sum:with:with withArguments:#(40 1 1) = 42'.
        self assertTrue:(([:arg1 :arg2 | arg1 - arg2] value:43 value:1) = 42) withMessage: '[:arg1 :arg2 | arg1 - arg2] value:43 value:1 = 42'.
    ]
    
    "------------------------------------"
    
    testExceptions [
        self assertTrue:(self testOnSignalDo) withMessage: 'testOnSignalDo = true'.
        "self assertTrue:(self testOnSignalDo2) withMessage: 'testOnSignalDo2 = true'."
        "self assertTrue:(self testResume) withMessage: 'testResume = true'."
        self assertTrue:(self testEnsure1) withMessage: 'testEnsure1 = true'.
        self assertTrue:(self testEnsure2 = nil) withMessage: 'testEnsure2 = nil'.
        self assertTrue:(self testEnsure3) withMessage: 'testEnsure3 = true'.
        self assertTrue:(self testEnsure4) withMessage: 'testEnsure4 = true'.
        self assertTrue:(self testEnsure5 = false) withMessage: 'testEnsure5 = false'.
    ]
    
    testOnSignalDo [
        ^[Signal signal] on: Signal do: [:ex | true]
    ]
    testOnSignalDo2 [
        ^[[Error signal. 123] on: Error do: [ :ex | ex pass. 213]] on: Exception do: [:ex | true]
    ]
    
    testResume [
        ^[Signal signal. true] on: Signal do: [ :ex | ex resume. 321]
    ]
    
    testEnsure1 [
        ^[Signal signal] ensure:[^true]
    ]
    
    testEnsure2 [
        ^[Signal signal] ensure:[true]
    ]
    
    testEnsure3 [
        ^[^true] ensure:[false]
    ]
    
    testEnsure4 [
        ^[true] ensure:[false]
    ]
     
    testEnsure5 [
        ^[true] ensure:[^false]
    ]
    
    "------------------------------------"
    
    testMethods[
        self assertTrue:(self testMethodAssignment = 559) withMessage: '123 + 16r2AE + -2r100 = 559'.
        self assertTrue:(self testMethodAssignmentAndBlock = 3) withMessage: 'self testAssignmentAndBlock = 3'.
        self assertTrue:(self testMethodIfTrueIfFalse = 123) withMessage: 'self testMethodIfTrueIfFalse = 123'.
    ]
    
    testMethodAssignment [
        | a | 
        ^ a := -123 + 16r2AE + -2r100.
    ]
    
    testMethodAssignmentAndBlock [
        |res|
        res := [:x |  x + 1] value: 2.
        ^res
    ]
  
    testMethodIfTrueIfFalse [
        ^true ifTrue: [ 123 ] ifFalse: [ 321 ]
    ]
    
    
    testPrimitiveNumbers [
        self assertTrue:(((123.321 + 2.2) - 125.521) abs < 0.00001) withMessage: '((123.321 + 2.2) - 125.521) abs < 0.00001'.
        self assertTrue:((16rFFFFFFFFFFFF - 16rFFFFFFFFFFF0) = 15) withMessage: '(16rFFFFFFFFFFFF - 16rFFFFFFFFFFF0) = 15'.
        
        self assertTrue:
                    (2r1111111111111111111111111111111 + 2r1111111111111111111111111111111 = 4294967294)
             withMessage:
                    '2r1111111111111111111111111111111 + 2r1111111111111111111111111111111 = 4294967294'.
      
      
       self assertTrue:
                    (-2r1111111111111111111111111111111 - 2r1111111111111111111111111111111 = -4294967294)
             withMessage:
                    '-2r1111111111111111111111111111111 - 2r1111111111111111111111111111111 = -4294967294'.
        
        self assertTrue:
                    (2r1111111111111111111111111111111 * 2r1111111111111111111111111111111 = 4611686014132420609)
             withMessage:
                    '2r1111111111111111111111111111111 * 2r1111111111111111111111111111111 = 4611686014132420609'.
        
        self assertTrue:
                    (-2r1111111111111111111111111111111 / -1 = 2147483647)
             withMessage:
                    '-2r1111111111111111111111111111111 / -1 = 2147483647'.
    ]
    
    "------------------------------------"
    testBlocks [
        self assertTrue:(self testMethodReturnBlock = 42) withMessage: 'self testMethodReturnBlock = 42'.
        self assertTrue:(self testMethodBlockStackReturn = 42) withMessage: 'self testMethodBlockStackReturn = 42'.
        self assertTrue:(self testMethodBlockSingleArgument = 42) withMessage: 'self testMethodBlockSingleArgument = 42'.
        self assertTrue:(self testMethodTemporaryScope = 42) withMessage: 'self testMethodTemporaryScope = 42'.
        self assertTrue:(self testMethodTemporaryScope2 = 42) withMessage: 'self testMethodTemporaryScope2 = 42'.
        
        self assertTrue:(self testBlockLoop = 42) withMessage: 'self testBlockLoop = 42'.
        self assertTrue:(self testTemporariesInOptimizedBlocks = 42) withMessage: 'self testTemporariesInOptimizedBlocks = 42'.
        self assertTrue:(self testBlocksRecursion2 = 42) withMessage: 'self testBlocksRecursion2 = 42'.
        self assertTrue:(self testBlocksRecursion = 42) withMessage: 'self testBlocksRecursion = 42'.
        
        self assertTrue:(self testNestedBlocks2 = 42) withMessage: 'self testNestedBlocks2 = 42'.
        self assertTrue:(self testNestedBlocks = 42) withMessage: 'self testNestedBlocks = 42'.
    ]
    
    testMethodReturnBlock [
        ^ [42] value
    ]
    
    testMethodBlockStackReturn [
        [^42] value
    ]
    
    
    testMethodBlockSingleArgument [
        [:s | ^s] value: 42.
    ]
    
    testMethodTemporaryScope [
        | tmp | 
        
        tmp := 42. 
        
        [ | tmp | 
            tmp := 24 ] value.
         ^tmp
    ]
    
     testMethodTemporaryScope2 [
        | tmp | 
        
        tmp := 24. 
        
        [ tmp := 42 ] value.
         ^tmp
    ]
    
     testNestedBlocks [
        ^[ :s | [ :s | s ] value: 42] value: 24
    ]
    
    testNestedBlocks2 [
        | b | 
        
        b := [ :a | a ].
        ^ [ :b | 
             ([ :b | 
                ([ :b | b value: 39 + 1 ] value: b) + 1
             ] value: b) + 1
          ] value: b
    ]
    
    
    testBlocksRecursion [
        | b i |
        i := 2.
        
        b := [ :b | (i := i + 10) = 42 ifFalse:
                                     [ b value: b ] ].
        b value: b.
         ^i
    ]
    
    testBlocksRecursion2 [
        | b |
         b := [ :i | 
               | d |
                    d := i + 1.
                   (i < 42 ) ifTrue: [ b value: d] ifFalse: [^i]
             ].
            b value: 5.
    ]
    
    testTemporariesInOptimizedBlocks [
       | tmp |
        tmp := 42.
         true 
            ifTrue: 
                [ | tmp | tmp := 24 ].
        ^tmp
    ]
    
    testBlockLoop [
        | var |
         1 to: 42 do: [ :i | var := i. ].
          ^var
    ]
    
   
    
]








Test extend [
    testArg1: aNumber [
        ^ self testArg1: aNumber * 4 testArg2:2 testArg3:3.
    ]
    
    testArg1: aNum1 testArg2: aNum2 testArg3: aNum3 [
        ^ aNum1  + (aNum2 * aNum3)
    ]
    
]
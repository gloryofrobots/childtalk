TestCase subclass: UndefinedObjectTest [
    
    
     doTest [
        self testTestingMethods.
        self testCopyingMethods.
     ]

    testTestingMethods [
        "Assert UndefinedObject supports each of the testing methods."
        self assertTrue: (nil isNil) withMessage: 'should be true.'.
        self assertFalse: (nil notNil) withMessage: 'should be false.'.
        self assert: (nil ifNotNil: [ true ]) equals: nil withMessage: 'should be nil.'.
        self assertTrue: (nil ifNil: [ true ]) withMessage: 'should be true.'.
    ]
    
    testCopyingMethods  [
        "Assert UndefinedObject supports each of the copying methods."
        self assert: (nil copy) equals: nil withMessage: 'should be nil.'.
    ]
]

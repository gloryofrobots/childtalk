

TestCase subclass: TrueTest [
    doTest [
        self testTrue.
        self testIfFalse.
        self testIfTrue.
        self testLogicalOperations.
        self testControllingMethods.
    ]
    testTrue [
        self assertTrue: true withMessage: 'should be true.'.
    ]
    testIfFalse [
        "Assert True answers nil in response to ifFalse."
        self assert: (true ifFalse: [ true ]) equals: nil withMessage: 'should answer nil.'.
    ]
    testIfTrue [
        "Assert True answers [] value in response to ifTrue."
        self assert: (true ifTrue: [ true ]) equals: true withMessage: 'should answer nil.'.
    ]
    testLogicalOperations [
        "Assert True has expected logical operations."
        self assertFalse: (true not) withMessage: 'should answer false.'.
        self assert: (true bitAnd:nil) equals: nil withMessage: 'should answer true.'.
        self assertTrue: (true bitOr: nil) withMessage: 'should answer true.'.
        self assertTrue: (true == true) withMessage: 'should answer true.'.
        self assertTrue: (true ~= false) withMessage: 'should answer true.'.
    ]
    testControllingMethods [
        "Assert True supports each of the controlling methods."
        self assert: (true ifTrue: [ true ] ifFalse: [ false ]) equals: true withMessage: 'should answer true.'.
        self assert: (true ifTrue: [ true ] ifFalse: [ nil ] ) equals: true withMessage: 'should answer true.'.
        self assertTrue: (true and: [ true ]) withMessage: 'should answer true.'.
        self assertTrue: (true or: [ false ]) withMessage: 'should answer true.'.
        ]
]

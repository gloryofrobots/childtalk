Object subclass: TestCase [
    |message errorFlag|
    
    (+) new [
        ^ self shouldNotImplement.
    ]
    
    
    (+) new: aMessage [
        ^ self basicNew initialize: aMessage.
    ]
    
    initialize: aMessage [
        errorFlag := false.
        message := aMessage.
    ]
    
    test [
        self doTest.
        errorFlag ifFalse: [ Transcript show: 'TEST '; show: message; show:' SUCCESS!';cr; ]
    ]
    
    doTest [
        self subclassResponsibility.
    ]
    
    assert: anObject equals: expectedObject withMessage: aString [
        anObject = expectedObject ifFalse: [ self error: aString ].
    ]
    
    assert: anObject isNotEqualTo: unexpectedObject withMessage: aString [
        anObject = unexpectedObject ifTrue: [ self error: aString ].
    ]
    
    assertTrue: aCondition withMessage: aString [
        aCondition ifFalse: [ self error: aString ].
    ]
    
    assertFalse: aCondition withMessage: aString [
        aCondition ifTrue: [ self error: aString ].
    ]
    
    fail: aString [
        self error: aString.
    ]
    
    error: txt[
        errorFlag := true.
        Transcript 
            show: 'TEST "';
            show: message;
            show:'" FAIL! DETAILES !';show: txt;  show: '!'; cr;
    ]
   ]
nil subclass: Object [
    <instanceVariableNames:'name superclass instanceVariableNames instanceSize methodDictionary subclasses finalizationRequest'>
    
    initialize []
    finalize []
    
    copy [
        ^ self shallowCopy
    ]
    
    shallowCopy [
        <primitive: 'Object_copy'>
        ^self
    ]
    
    size [
        <primitive: 'Object_size'>
        self primitiveFailed
    ]
    
    hash [
        <primitive: 'Object_hash'>
        self primitiveFailed
    ]

    isCharacter [
        ^false
    ]

    
    isBoolean [
        ^false
    ]
    
    isKindOf: aClass [
        <comment: "Answer true if the receiver's class is a subclass of aClass">
        | class |
        aClass ifNil: [ ^false ].
        class := self class.
        class = aClass ifTrue: [ ^true ].
        aClass allSubclassesDo: [ :ea |  ea = class ifTrue: [ ^true ] ].
        ^false
    ]
    isMemberOf: aClass [
        "Answer true whenever the receiver is an instance of the given class"
        ^self class = aClass
    ]
    
    isClass [
        ^false
    ]
    
    isNumber [
        ^false
    ]
    
    isCollection [
        ^false
    ]
    
    isString [
        ^false
    ]
    
    isSymbol [
        ^false
    ]
    
    ifNil: aBlock [
        ^self
    ]
    
    ifNotNil: aBlock [
        ^aBlock value
    ]
    
    isNil [
        ^false
    ]
    
    notNil [
        ^true
    ] 
    
    
    
    class [
        <primitive: 'Object_class'>
        self primitiveFailed
    ]
    
    = anObject [
        <primitive: 'Object_equal'>
        self primitiveFailed
    ]
    
    == anObject [
        <primitive: 'Object_identityEqual'>
        self primitiveFailed
    ]
    
    ~= anObject [
        ^(self = anObject) not
    ]
    
    ~~ anObject [
        ^(self == anObject) not
    ] 
    
    
    error: messageText [
        Error signal: messageText
    ]
    
    doesNotUnderstand: aMessage [
        MessageNotUnderstood
        message: aMessage receiver: self
    ]
    
    shouldNotImplement [
        ShouldNotImplement signal
    ]
    
    subclassResponsibility [
        SubclassResponsibility signal
    ]
    
    notYetImplemented [
        NotYetImplemented signal
    ]
    
    primitiveFailed [
        PrimitiveFailed signal
    ]
    
    errorNotFound [
        NotFound signal
    ]
    
    errorNotFound: aMessage [
        NotFound signal: aMessage
    ] 
    
    
    respondsTo: selector [
        "Answer if the receiver can respond to the given selector"
        ^self class doesUnderstand: selector
    ]
    
    perform: selector [
        <primitive: 'Object_perform'>
        (self respondsTo: selector)
        ifFalse: [ self doesNotUnderstand: selector ]
        ifTrue: [ WrongArgumentCount signal ]
    ]
    
    perform: selector with: argument1 [
        <primitive: 'Object_perform'>
        (self respondsTo: selector)
        ifFalse: [ self doesNotUnderstand: selector ]
        ifTrue: [ WrongArgumentCount signal ]
    ]
    
    perform: selector with: argument1 with: argument2 [
        <primitive: 'Object_perform'>
        (self respondsTo: selector)
        ifFalse: [ self doesNotUnderstand: selector ]
        ifTrue: [ WrongArgumentCount signal ]
    ]
    
    perform: selector with: argument1 with: argument2 with: argument3 [
        <primitive: 'Object_perform'>
        (self respondsTo: selector)
        ifFalse: [ self doesNotUnderstand: selector ]
        ifTrue: [ WrongArgumentCount signal ]
    ]
    
    perform: selector withArguments: arguments [
        "Send a message using the given message selector and the given collection of arguments"
        <primitive: 'Object_performWithArguments'>
        (self respondsTo: selector)
        ifFalse: [ self doesNotUnderstand: selector ]
        ifTrue: [ WrongArgumentCount signal ]
    ]
    
    -> anObject [
        "Create a new association between myself and anObject"
        ^Association key: self value: anObject
    ]
    
    caseOf: anArrayOfAssociations [
        ^self caseOf: anArrayOfAssociations otherwise: [ self errorNotFound ]
    ]
    
    caseOf: anArrayOfAssociations otherwise: aBlock [
        "
         Switch case between the keys of the array. If one of the block evaluation is the same as the receiver
         then the value of the association (a block) is evaluated.
         
         123 caseOf: {
           [123] -> ['This is 123'].
           [321] -> ['This is 321'] }
         "
        anArrayOfAssociations do: [ :assoc |
        assoc key value = self
            ifTrue: [ ^assoc value value ] ].
        ^aBlock value
    ] 
    
    
    yourself [
        "Answer the receiver"
        ^self
    ] 
    
    
    briefInspect [
        ^self
    ] 
    
    
    display [
        Transcript show: self displayString
    ]
    
    displayNl [
        Transcript showCr: self displayString
    ]
    
    print [
        Transcript show: self printString
    ]
    
    printNl [
        Transcript showCr: self printString
    ]
    
    displayOn: aStream [
        "Append to aStream a String representation of the receiver for being displayed"
        self printOn: aStream
    ]
    
    printOn: aStream [
        "Append to aStream a String representation of the receiver"
        aStream
            nextPutAll: self class article;
            space;
            nextPutAll: self class name
    ]
    
    displayString [
        "Answer a String repsenentation of the receiver for being displayed"
        | stream |
        stream := WriteStream on: String new.
        self displayOn: stream.
        ^stream contents
    ]
    
    printString [
        "Answer a String representation of the receiver"
        | stream |
        stream := WriteStream on: String new.
        self printOn: stream.
        ^stream contents
    ]
    
    storeOn: aStream [
        "Append to aStream a String representation from which the receiver can be reconstructed"
        aStream
            nextPut: $(;
            store: self class;
            nextPutAll: ' basicNew)'
    ]
    
    storeString [
        "Answer a String representation from which the receiver can be reconstructed"
        | stream |
        stream := WriteStream on: String new.
        self storeOn: stream.
        ^stream contents
    ] 
    
    species [
        "Answer the same class as that of the receiver.
         Some Collection classes may override it"
        ^self class
    ] 
]



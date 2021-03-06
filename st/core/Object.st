"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"


nil subclass: Object [
    initialize [
        yourself
    ]
    
    finalize [
        yourself
    ]

    copy [
        <primitive: 'Object_copy'>
        self primitiveFailed
    ]
   
    hash [
        <primitive: 'Object_hash'>
        self primitiveFailed
    ]
    
    isKindOf: aClass [
          "Answer true if the receiver's class is a subclass of aClass"
        | class |
        aClass
        ifNil: [ ^false ].
        class := self class.
        class = aClass
        ifTrue: [ ^true ].
    
        aClass allSubclassesDo: [ :ea |
        ea = class
            ifTrue: [ ^true ] ].
        ^false
    ]
    
    
    isMemberOf: aClass [
        "Answer true whenever the receiver is an instance of the given class"
        ^self class = aClass
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
    
    pln [
        <primitive: 'Object_println'>
        self primitiveFailed
    ]
    
    pln: aObject [
        self pln.
        aObject pln.
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
    
    print [
        Transcript show: self printString
    ]
    
    printNl [
        Transcript showCr: self printString
    ]
    
    printOn: aStream [
        "Append to aStream a String representation of the receiver"
        
        aStream
            nextPutAll: self class article;
            space;
            nextPutAll: self class name
    ]
  
    printString [
        "Answer a String representation of the receiver"
        | stream string|
        
        string := String new.
        
        stream := WriteStream on: string.
        self printOn: stream.
        
        ^stream contents
    ]
    species [
        ^ self class
    ]
] 



Block extend [
    
    newProcess  [
        "Return a new Process from self. The Process is suspended"
        <primitive: 'BlockClosure_newProcess'>
    	self primitiveFailed
    ]
    
    fork  [
        "Answer a new resumed Process"
        ^self newProcess resume
    ]
    
    on: anExceptionClass do: aBlock  [
        "If anExceptionClass, or its subclasses, are signaled, aBlock is evaluated with the exception as argument"
        <primitive: 'BlockClosure_on_do'>
        self primitiveFailed
    ]
     
    ifCurtailed: aBlock  [
        "Be sure aBlock is called if an error occurs evaluating the receiver"
        ^self on: Signal do: [ :ex | aBlock value. ex pass. ^nil ]
    ]    
    
    ensure: aBlock [ 
        "Be sure aBlock is called if an error occurs or not"
        | ctx |
        ctx := self asContextWithArguments: #().
        ctx
    	handledException: Signal;
    	handlerBlock: [ :ex | aBlock value. ex pass. ^nil ];
    	ensureBlock: aBlock.
        ^Processor enter: ctx
    ]
    
    value  [
        "Call my block without arguments"
        <primitive: 'BlockClosure_value'>
    	WrongArgumentCount signal
    ]
    
    value: anObject  [
        "Call my block with anObject as single argument"
        <primitive: 'BlockClosure_value'>
    	WrongArgumentCount signal
    ]
    
    value: firstObject value: secondObject  [
        <primitive: 'BlockClosure_value'>
    ]
    
    repeat [
        "Do an infinite loop"
        [true] whileTrue: self
    ]
    
    whileTrue [
        "Loop until self evaluation is false"
        self whileTrue: [nil]
    ]
    
    whileFalse [
        "Loop until self evaluation is true"
        self whileFalse: [nil]
    ]
    
    whileTrue: aBlock [
        "Do aBlock until self is false"
        self whileTrue: aBlock
    ]
    
    whileFalse: aBlock [
        "Do aBlock until self is true"
        self whileFalse: aBlock
    ]
]
"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"


Object subclass: Block [
    
    newProcess  [
        "Return a new Process from self. The Process is suspended"
        <primitive: 'Block_newProcess'>
    	self primitiveFailed
    ]
    
    fork  [
        "Answer a new resumed Process"
        ^self newProcess resume
    ]
    
    on: anExceptionClass do: aBlock  [
       ^ self on: anExceptionClass do: aBlock withEnsured: nil.
    ]
     
     on: anExceptionClass do: aBlock withEnsured: aEnsuredBlock  [
        "If anExceptionClass, or its subclasses, are signaled, aBlock is evaluated with the exception as argument"
        <primitive: 'Block_on_do_ensure'>
        self primitiveFailed
    ]
    
    ifCurtailed: aBlock  [
        "Be sure aBlock is called if an error occurs evaluating the receiver"
        ^self on: Signal do: [ :ex | aBlock value. ex pass. ^nil ]
    ]    
    
    ensure: aBlock [ 
        "Be sure aBlock is called if an error occurs or not"
         ^ self on: Signal do:[ :ex | aBlock value. ^nil] withEnsured:aBlock.
    ]
    
    value  [
        "Call my block without arguments"
        <primitive: 'Block_value'>
    	WrongArgumentCount signal
    ]
    
    value: anObject  [
        "Call my block with anObject as single argument"
        <primitive: 'Block_value'>
    	WrongArgumentCount signal
    ]
    
    value: firstObject value: secondObject  [
        <primitive: 'Block_value'>
        WrongArgumentCount signal
    ]
    
    value: firstObject value: secondObject value: thirdObject [
        <primitive: 'Block_value'>
        WrongArgumentCount signal
    ]
    
    value: firstObject value: secondObject value: thirdObject value: fourthObject[
        <primitive: 'Block_value'>
        WrongArgumentCount signal
    ]
    
    repeat [
        "Do an infinite loop"
        [true] whileTrue: self
    ]
    
    
    whileTrue [
        self whileTrue: []
    ]
    
    whileFalse [
        "Loop until self evaluation is true"
        self whileFalse: []
    ]
    
    whileFalse: aBlock [
        [ self value not ] whileTrue: aBlock
    ]
        
    whileTrue: block [
        ( self value ) ifTrue:
            [   block value. 
                self whileTrue: block ]
    ]
    
]
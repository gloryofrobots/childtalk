Exception extend [    
    description [
        ^'An exception has been raised.'
    ]
]

MessageNotUnderstood extend [
    message: aMessage receiver: anObject [
        ^self new
        message: aMessage receiver: anObject;
        signal
    ]
    
    message [
        ^message
    ] 
    
    receiver [
        ^receiver
    ]
    
    message: aMessage receiver: anObject [
        message := aMessage.
        receiver := anObject.
        self messageText: anObject class printString, ': did not understood ', aMessage printString
    ]
]


SubclassResponsibility extend [
    signal [
        ^self signal: 'This method is not responsibility of this class. A subclass must implement this method.'
    ]
]

WrongArgumentCount extend [
    signal [
        ^self signal: 'Wrong number of arguments.'
    ]
]

NotFound extend [
    what: anObject [
        ^self signal: 'Object not found.'
    ]
]



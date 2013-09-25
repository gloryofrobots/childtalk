Signal extend [
  
    (+) signal: messageText [
        self new signal: messageText
    ]
    
    (+) signal [
        self new signal
    ]
    
    (+) fatal: messageText [
       <primitive: 'Signal_fatal'>
    ]
    
    description [
        ^'A signal has been sent.'
    ]
    
    messageText [
        ^messageText
    ]
    
    messageText: aMessageText [
        messageText := aMessageText
    ]
    
    signal [
        "Raises a signal. If it's not handled, debug the process"
        signalerContext := thisContext parent.
        self raise: self
    ]
    
    raise: aSignal [
        | context |
        context := signalerContext.
        [context = nil]
            whileFalse:
               [ (context handles:aSignal) 
                    ifTrue: [ context handle:aSignal. ^ true]
                    ifFalse: [ context := context parent. ] ].
        
        'Unhandled signal ' print. self print. '\n' print.
        signalerContext die.
    ]
    
    baseRaise: anObject [
         <primitive: 'Signal_raise'>
         self primitiveFailed.
    ]
    
    pass: anObject [
         self raise
    ]
    
    printOn: aStream [
        "Append to aStream a String representation of the receiver"
        super printOn: aStream.
        "self description notNil ifTrue:
                    [aStream
                         nextPutAll: self description;
                                    space].        
        self messageText notNil ifTrue:
                     [aStream 
                        nextPutAll: self messageText.]."
    ]
    
    signal: aMessageText [
        messageText := aMessageText.
        self signal
    ]
]
Signal extend [
  
    (+) new [
        <primitive: 'Signal_new'>
    ]
    
    (+) signal: messageText [
        self new signal: messageText
    ]
    
    (+) signal [
        self new signal
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
        self raise: self
    ]
    
    raise: anObject [
         <primitive: 'Signal_rise'>
    ]
    
    printOn: aStream [
        "Append to aStream a String representation of the receiver"
        super printOn: aStream.
        aStream
             nextPutAll: self description;
             space;
             nextPutAll: self messageText.
    ]
    
    signal: aMessageText [
        messageText := aMessageText.
        self signal
    ]
]
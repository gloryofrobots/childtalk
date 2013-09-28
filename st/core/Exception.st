"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

Signal subclass: Exception [    
    description [
        ^'An exception has been raised.'
    ]
]


Eval [
    
    Exception subclass: #Error
          instanceVariableNames: ''
              classVariableNames: ''.
              
    Error subclass: #NotImplemented
          instanceVariableNames: ''
          classVariableNames: ''.
    
    Error subclass: #ZeroDivide
          instanceVariableNames: ''
          classVariableNames: ''.
    
    Error subclass: #PrimitiveFailed
        instanceVariableNames: ''
            classVariableNames: ''.
    
    PrimitiveFailed subclass: #WrongArgumentCount
            instanceVariableNames: ''
            classVariableNames: ''.
    
    Error subclass: #MessageNotUnderstood
         instanceVariableNames: 'message receiver'
         classVariableNames: ''.
    
    NotImplemented subclass: #NotYetImplemented
               instanceVariableNames: ''
                   classVariableNames: ''.
    
    NotImplemented subclass: #ShouldNotImplement
               instanceVariableNames: ''
                   classVariableNames: ''.
    
    ShouldNotImplement subclass: #SubclassResponsibility
               instanceVariableNames: ''
                       classVariableNames: ''.
    
    Error subclass: #InvalidValue
          instanceVariableNames: ''
          classVariableNames: ''.
    
    InvalidValue subclass: #InvalidArgument
             instanceVariableNames: ''
                 classVariableNames: ''.

    InvalidArgument subclass: #NotFound
            instanceVariableNames: ''
                    classVariableNames: ''.
    
    InvalidArgument subclass: #NotKeyed
                    instanceVariableNames: ''
                    classVariableNames: ''.
]
  
MessageNotUnderstood extend [
   (+) message: aMessage receiver: anObject [
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
   (+) signal [
        ^self signal: 'This method is not responsibility of this class. A subclass must implement this method.'
    ]
]

WrongArgumentCount extend [
   (+) signal [
        ^self signal: 'Wrong number of arguments.'
    ]
]

NotFound extend [
   (+) what: anObject [
        ^self signal: 'Object not found.'
    ]
]



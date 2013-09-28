"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"
SystemSignal extend [
   
    description [
        ^'The application received a signal from the system.'
    ]
    
    
    isHandled [
        ^false
    ]
    
    
    notHandled [
        Transcript showCr: self messageText.
        Smalltalk quit
    ]
    
    
    signal [
        ^self signal: 'User interrupt (SIGINT)'
    ]
    
    
    initialize [
        Smalltalk registerForSystemInitialization: self
    ]
    
    initializeSystem [
        HandlerBlock := nil
    ]
    
    
    handlerBlock: aBlock [
        HandlerBlock := aBlock
    ]
    
    handlerBlock [
        ^HandlerBlock
    ]
    
    
    handle [
        ^HandlerBlock value: self
    ]
    
    
    isHandled [
        ^HandlerBlock notNil
    ]
    
    
    signal [
        ^self signal: 'Floating point exception (SIGFPE)'
    ]
    
    
    signal [
        ^self signal: 'Termination signal (SIGTERM)'
    ]
    
    
    initialize [
        Smalltalk registerForSystemInitialization: self
    ]
    
    initializeSystem [
        HandlerBlock := nil
    ]
    
    
    handlerBlock: aBlock [
        HandlerBlock := aBlock
    ]
    
    handlerBlock [
        ^HandlerBlock
    ]
    
    
    handle [
        ^HandlerBlock value: self
    ]
    
    
    isHandled [
        ^HandlerBlock notNil
    ]
    
    
    signal [
        ^self signal: 'Abnormal termination (SIGABRT)'
    ]
]
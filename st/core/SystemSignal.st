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
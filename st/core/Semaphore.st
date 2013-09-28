"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"
Semaphore extend [
   (+) new [
        ^self basicNew initialize
    ]
    
   (+) forMutualExclusion [
        "Create a new Semaphore and signal it initially. This means the first time a Process waits, it's not blocked."
        ^self new
            signal;
            yourself
    ]
    
    
    critical: aBlock   [
        "Wait the semaphore, call aBlock ensuring to signal the semaphore at the end"
        self wait.
        ^aBlock ensure: [ self signal ]
    ]
    
    
    initialize [
        signals := 0.
        waitingProcesses := Array new
    ]
    
    
    waitingProcesses [
        "The list of process being waiting"
        ^waitingProcesses
    ]
    
    signals [
        "The difference between all signals and the number of waiting processes"
        ^signals - waitingProcesses size
    ]
    
    
    wait: aFD for: aBoolean [
        <primitive: 'Semaphore_waitFor'>
    	self primitiveFailed
    ]
    
    
    waitForRead: aStream [
        "Wait a file descriptor to be ready for reading"
        self wait: aStream handle for: false
    ]
    
    waitForWrite: aStream [
        "Wait a file descriptor to be ready for writing"
        self wait: aStream handle for: true
    ]
    
    signal [
        <primitive: 'Semaphore_signal'>
    	self primitiveFailed
    ]
    
    wait [
        <primitive: 'Semaphore_wait'>
    	self primitiveFailed
    ]
]
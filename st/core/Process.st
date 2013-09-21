Process extend [
    (+) on: aContext [
        ^self new context: aContext; initialize; yourself
    ]
    
    
    initialize [
        self suspend
    ]
    
    suspend [
        suspended := true.
        self isActive ifTrue: [Processor yield]
    ]
    
    resume [
        suspended := false.
        self isActive ifFalse: [Processor yield]
    ]
    
    terminate [
        self isActive
    	ifTrue: [ Processor leaveTo: nil andAnswer: nil]
    	ifFalse: [ suspended := false. "context := nil" ]
    ]
    
    
    isActive [
        "Answer if the receiver is the active process being run"
        ^self == Processor activeProcess
    ]
]
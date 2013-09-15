ContextPart extend [
   
    tracebackString [
        "Shows my single traceback"
        | class receiver |
        receiver := self receiver.
        class := method methodClass = receiver class
    	ifTrue: [ receiver class printString ]
    	ifFalse: [ receiver class printString, '(', method methodClass printString, ')' ].
        ^class, '>>', method selector printString
    ]
    
    handles: anException [
        ^false
    ]
    
    homeContext [
        ^self
    ]
    
    isMethodContext [
        ^true
    ]
    
    
    handles: anException [
        self subclassResponsibility
    ]
    
    showTraceback [
        self subclassResponsibility
    ]
    
    homeContext [
        self subclassResponsibility
    ]
    
    traceback [
        "Recurse trough my parent contexts and show their tracebacks"
        | context |
        context := self.
        [ context notNil ]
    	whileTrue: [
                Transcript showCr: context tracebackString.
    	    context := context parent ]
    ]
    
    
    
    isMethodContext [
        self subclassResponsibility
    ]
    
    method [
        ^method
    ]
    
    receiver [
        <primitive: 'ContextPart_receiver'>
        self primitiveFailed
    ]
    
    parent: aContext [
    "    parent := aContext" [
    ]
    
    parent [
        <primitive: 'ContextPart_parent'>
        self primitiveFailed
    "    ^parent" [
    ]
]
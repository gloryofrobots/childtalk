CompiledMethod extend [
   
    bytecodes [
        ^bytecodes
    ]
    
    stackSize [
        ^stackSize
    ]
    
    methodClass [
        ^class
    ]
    
    argumentCount [
        ^argumentCount
    ]
    
    text [
        ^text
    ]
    
    selector [
        ^nil
    ]
    
    
    initializeParser: aCompilerParser [
        | scope |
        bytecodes
    	ifNotNil: [ ^self error: 'Already initialized' ].
    
        bytecodes := aCompilerParser bytecode contents.
        scope := aCompilerParser argumentScopes peek.
        argumentCount := scope value - scope key + 1.
        stackSize := aCompilerParser bytecode stackSize.
        literals := aCompilerParser bytecode literals.
        text := aCompilerParser lexer text
    ]
    
    
    fromParser: aCompilerParser [
        ^self new initializeParser: aCompilerParser
    ]
    
    
    initializeParser: aCompilerParser [
        super initializeParser: aCompilerParser.
        selector := aCompilerParser methodMessage.
        primitive := aCompilerParser methodPrimitive
    ]
    
    
    runOn: aReceiver withArguments: anArray [
        <primitive: 'CompiledMethod_runOn'>
    	self primitiveFailed
    ]
    
    argumentStackSize [
    "    ^argumentStackSize" 
    ]
    
    temporaryStackSize [
    "    ^temporaryStackSize"
    ]
    
    selector [
        ^selector
    ]
    
    
    initializeParser: aCompilerParser [
        super initializeParser: aCompilerParser.
    "    argumentStackTop := aCompilerParser argumentScopes peek key - 1" 
    ]
]
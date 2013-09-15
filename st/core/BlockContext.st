BlockContext extend [
  
    handlerBlock [
        "Answer the block that handles handledException"
        ^handlerBlock
    ]
    
    handlerBlock: aBlock [
        "Set the block that handles handledException"
        handlerBlock := aBlock
    ]
    
    ensureBlock: aBlock [
        "Set the block to be evaluted"
        ensureBlock := aBlock
    ]
    
    ensureBlock [
        ^ensureBlock
    ]
    
    handledException: anExceptionClass [
        "Set the exception handled by handlerBlock"
        handledException := anExceptionClass
    ]
    
    handledException [
        "Answer the exception handled by handlerBlock"
        ^handledException
    ]
    
    
    handles: anException [
        ^anException isKindOf: handledException
    ]
    
    tracebackString [
        "Shows my single traceback"
        ^self homeContext tracebackString, '[]'
    ]
    
    homeContext [
        ^self outerContext homeContext
    ]
    
    outerContext [
        <primitive: 'BlockContext_outerContext'>
        self primitiveFailed
    ]
    
    
    isMethodContext [
        ^false
    ]
]
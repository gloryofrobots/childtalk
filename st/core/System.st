System extend [
    at: aSymbol put: anObject [
        (aSymbol isKindOf: Symbol)
    	ifFalse: [ self error: 'system dictionary may have only symbol keys' ].
        ^super at: aSymbol put: anObject.
    ]
    
    at: aSymbol [
        (aSymbol isKindOf: Symbol)
        ifFalse: [ self error: 'system dictionary may have only symbol keys' ].
        ^super at: aSymbol.
    ]
    
    getEnvironmentVariable: aName [
        <primitive: 'System_getEnv'>
        self primitiveFailed.
    ]
    
    getProperty: aName [
        <primitive: 'System_getProperty'>
        self primitiveFailed.
    ]
    
    quit: exitStatus [
        "Quit Syx with exitStatus"
        <primitive: 'System_quit'>
        self primitiveFailed
    ]
    
    quit [
        "Quit Syx"
        ^self quit: 0
    ]
    
    version [
        ^'0.0.1'
    ]
]
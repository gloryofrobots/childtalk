SystemDictionary extend [
    at: aSymbol put: anObject [
        aSymbol isSymbol
    	ifFalse: [ self error: 'system dictionary may have only symbol keys' ].
        ^super at: aSymbol put: anObject
    ]
    
    quit: exitStatus [
        "Quit Syx with exitStatus"
        <primitive: 'Smalltalk_quit'>
    	self primitiveFailed
    ]
    
    quit [
        "Quit Syx"
        ^self quit: 0
    ]
    
    registerForSystemInitialization: anObject [
        systemInitializationRequests add: anObject
    ]
    
    initializeFirstSystem [
        "This method is called on image creation"
        systemInitializationRequests := Set new.
    
        Object allSubclassesDo: [ :ea |
    	ea initialize ].
    ]
    
    startupObject: anObject [
        startupObject := anObject
    ]
    
    initializeSystem: commandLineArguments [
        commandLine := CommandLine new: commandLineArguments.
    
        self
    	at: #stdin put: (StdIOStream on: #stdin for: FileStream read);
    	at: #stdout put: (StdIOStream on: #stdout for: FileStream write);
    	at: #stderr put: (StdIOStream on: #stderr for: FileStream write);
    	at: #Transcript put: (TextCollector proxyMessage: stdout -> #nextPutAll:).
    
        systemInitializationRequests do: [ :ea |
    	ea initializeSystem ].
    ]
    
    startupSystem [
        | continueStartup evalString |
        commandLine parse: #(c e:).
        continueStartup := commandLine options at: #c ifAbsent: [ false ].
        evalString := commandLine options at: #e: ifAbsent: [ nil ].
        evalString
            ifNotNil: [ evalString doIt ].
    
        commandLine arguments do: [ :fileName |
            FileStream fileIn: fileName ].
    
        commandLine commandLineArguments isEmpty | continueStartup
            ifFalse: [ ^self ].
    
        startupObject isNil
            ifTrue: [
    	     Console startup ]
    	ifFalse: [
    	     startupObject startup ]
    ]
    
    
    rehashClass [
        ^self class superclass
    ]
    
    
    commandLine [
        ^commandLine
    ]
    
    
    plugin: pluginName cCall: functionName withArguments: anArray [
        <primitive: 'Smalltalk_pluginCall'>
    	self primitiveFailed
    ]
    
    cCall: functionName withArguments: anArray [
        ^self plugin: nil cCall: functionName withArguments: anArray
    ]
    
    plugin: pluginName cSymbol: symbolName [
        <primitive: 'Smalltalk_pluginSymbol'>
    	self primitiveFailed
    ]
    
    loadPlugin: name [
        <primitive: 'Smalltalk_loadPlugin'>
    	self primitiveFailed
    ]
    
    unloadPlugin: name [
        <primitive: 'Smalltalk_unloadPlugin'>
    	self primitiveFailed
    ]
    
    
    environmentVariableAt: aStringOrSymbol [
        <primitive: 'Smalltalk_environmentVariableAt'>
    	self primitiveFailed
    ]
    
    version [
        ^'Smalltalk YX 0.1.7'
    ]
]
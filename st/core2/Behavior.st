
Object subclass: Behaviour [
    |name superclass instanceVariableNames  subclasses finalizationRequest|
    
    isClass [
        ^true
    ]
    
    name [
        ^name
    ]
    
    subclasses [
        "A list of direct subclasses of the receiver"
        ^subclasses
    ]
    
    superclass [
        "Answer my superclass"
        ^superclass
    ]
    
    instanceVariableNames [
        "Answer a copy of the names of the instance variables"
        ^instanceVariableNames copy
    ]
    
    basicNew [
        <primitive: 'Behavior_new'>
    	^self primitiveFailed
    ]
    
    new [
        <primitive: 'Behavior_new'>
    	^self primitiveFailed
    ]
    
    new: anInteger [
        self shouldNotImplement
    ]
    
    doesUnderstand: selector [
        "Answer true if the receiver understand the given message selector, else false"
         <primitive: 'Behavior_doesUnderstand'>
        ^self primitiveFailed
    ]
    
    allSubclasses [
        "Answer a list of all subclasses of the receiver"
        | ret |
        ret := OrderedCollection new.
        self allSubclassesDo: [ :ea |
    	ret add: ea ].
        ^ret
    ]
    
    allSubclassesDo: aBlock [
        "Call aBlock recursing trough each subclass of this class"
        self subclasses do: [ :ea |
    	aBlock value: ea.
    	ea allSubclassesDo: aBlock ]
    ]
    
    allSuperclassesDo: aBlock  [
        "Call aBlock for each superclass of this class"
        | class |
        class := self.
        [ (class := class superclass) notNil ]
    	whileTrue: [ aBlock value: class ]
    ]
    
    article [
        "Answer the article to put before my name"
        self name first isVowel
            ifTrue: [ ^'an' ]
            ifFalse: [ ^'a' ]
    ]
    
    printOn: aStream [
        "My string representation is my name itself"
        aStream nextPutAll: self name
    ]
    
    storeOn: aStream [
        "My string representation is my name itself"
        aStream nextPutAll: self name
    ]
    
    classVariables [
        ^classVariables
    ]
]
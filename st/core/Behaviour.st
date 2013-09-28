"
Childtalk standard library is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

Object subclass: Behaviour [ 
    |className classSuperclass instanceVariableNames instanceSize methodDictionary classSubclasses finalizationRequest|
    
    subclass: name [
        "Create a new subclass of the receiver. This is a convenience message that delegates to the default."
        ^ self subclass: name instanceVariableNames: '' classVariableNames: ''.
    ]
    
    subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames [
        <primitive: 'Behaviour_createSubclass'>
        self primitiveFailed
    ]
    
    inheritsFrom: aClass [
        "Answer whether the argument, aClass, is on the receiver's superclass chain."
        | aSuperclass |
        aSuperclass := self superclass.
        [ aSuperclass == nil ]
          whileFalse: [
            aSuperclass == aClass ifTrue: [ ^ true ].
            aSuperclass := aSuperclass superclass.
          ].
        ^ false.
    ]
    
    name [
        ^ className
    ]
    
    subclasses [
        "A list of direct subclasses of the receiver"
        ^ classSubclasses
    ]
    
    superclass [
        "Answer my superclass"
        ^ classSuperclass
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
         ^ self basicNew
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
        |sb|
        "Call aBlock recursing trough each subclass of this class"
        sb := self subclasses.
        self subclasses do: [ :ea |
        aBlock value: ea.
        ea allSubclassesDo: aBlock]
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

Behaviour subclass: Class [ |classVariables| ]


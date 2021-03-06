"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

    
           
Stream subclass: PositionableStream [
    
    |collection curPos endPos|
    
   (+) on: aCollection [
        ^self new initializeOn: aCollection
    ]
    
   (+) with: aCollection [
        ^self new initializeOn: aCollection; setToEnd
    ]
    
    
    initializeOn: aCollection [
        self initialize.
        collection := aCollection.
        curPos := 0.
        endPos := collection size    
    ]
    
    
    size [
        ^endPos
    ]
    
    collectionClass [
        ^collection class
    ]
    
    contents [
        ^collection copyFrom: 1 to: endPos
    ]
    
    upToEnd [
        ^collection copyFrom: (curPos + 1 min: endPos) to: endPos
    ]
    
    position [
        ^curPos
    ]
    
    position: anInteger [
        (anInteger between: 0 and: endPos)
    	ifTrue: [ curPos := anInteger ]
    	ifFalse: [ self error: 'Position out of bounds' ]
    ]
    
    peek [
        self ensureReadable.
        self atEnd
    	ifTrue: [ ^nil ].
        ^collection at: curPos + 1
    ]
    
    skipTo: anObject [
        self ensureReadable.
        self do: [ :ea |
    	ea = anObject
    	    ifTrue: [ ^true ] ].
        ^false
    ]
    
    next [
        self ensureReadable.
        self atEnd
    	ifTrue: [ ^nil ].
        ^collection at: self skipOne
    ]
    
    nextPut: anObject [
        self ensureWriteable.
        curPos := curPos + 1.
        
        self atEnd
    	ifTrue: [ (endPos := curPos) >= collection size
    		      ifTrue: [ collection growTo: endPos + 10 ] ].
       
        ^collection at: curPos put: anObject
    ]
    
    nextPutAll: aCollection [
        | pos |
        
        
        self ensureWriteable.
        pos := curPos + 1.
        curPos := curPos + aCollection size.
        
        self atEnd
    	ifTrue: [ (endPos := curPos) >= collection size
    		      ifTrue: [ collection growTo: endPos + 10 ] ].
        
        collection replaceFrom: pos to: curPos with: aCollection.
        ^aCollection
    ]
    
    collection [
        self ensureReadable.
        ^collection
    ]
    
    
    errorAtEnd [
        self atEnd
    	ifTrue: [ self error: 'End of stream' ]
    ]
    
    
    skip: anInteger [
        ^curPos := (curPos + anInteger) min: endPos
    ]
    
    truncate [
        endPos := curPos
    ]
    
    reset [
        curPos := 0
    ]
    
    skipOne [
        ^self skip: 1
    ]
    
    atEnd [
        ^curPos >= endPos
    ]
    
    setToEnd [
        curPos := endPos
    ]
    
    
    isPositionable [
        ^true
    ]
    
    isEmpty [
        ^endPos = 0
    ]
]

    
PositionableStream subclass: WriteStream [
   
    initialize [
        super initialize.
        self allowWriteOnly.
        self truncate
    ]
]

PositionableStream subclass: ReadStream [
    initialize [
        super initialize.
        self allowReadOnly
    ]
]

 WriteStream subclass: ReadWriteStream [
    initialize [
        super initialize.
        self allowReadAndWrite
    ]
]


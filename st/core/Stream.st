"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

   
Object subclass: Stream [

    |accessMode|

    (+) new [
        ^self basicNew initialize
    ]
    
    
    initialize [
        self allowReadOnly
    ]
    
    
    allowReadOnly [
        accessMode := 0
    ]
    
    allowReadAndWrite [
        accessMode := 1
    ]
    
    allowWriteOnly [
        accessMode := 2
    ]
    
    canRead [
        ^accessMode <= 1
    ]
    
    canWrite [
        ^accessMode >= 1
    ]
    
    ensureReadable [
        self canRead
    	ifFalse: [ ^self error: 'the stream is not readable' ]
    ]
    
    ensureWriteable [
        self canWrite
    	ifFalse: [ ^self error: 'the stream is not writeable' ]
    ]
    
    next [
        self subclassResponsibility
    ]
    
    next: amount [
        "Returns a collection with the given number of objects next in the stream"
        | i stream |
        self ensureReadable.
        stream := WriteStream on: (self collectionClass new: amount).
        1 to: amount do: [ :i |
    	stream nextPut: self next ].
        ^stream contents
    ]
    
    peek [
        self subclassResponsibility
    ]
    
    contents [
        self subclassResponsibility
    ]
    
    upToEnd [
        "Answer the contents of the stream from current position"
        | stream |
        stream := WriteStream on: (self collectionClass new: 10).
        self do: [ :ea | stream nextPut: ea ].
        ^stream contents
    ]
    
    nextPut: anObject [ 
        "Write anObject to the receiver"
        self subclassResponsibility
    ]
    
    nextPutAll: aCollection [
        "Write all the objects in aCollection to the receiver"
        self ensureWriteable.
        aCollection do: [ :element | self nextPut: element ].
        ^aCollection
    ]
    
    
    isPositionable [
        ^false
    ]
    
    isEmpty [
        self subclassResponsibility
    ]
    
    atEnd [
        "Return true if I'm at the end"
        self subclassResponsibility
    ]
    
    
    do: aBlock [
        "Call a block for each element of the stream from the current position, until the end."
        [ self atEnd ] whileFalse:
            [ aBlock value: self next ]
    
    ]
    
    
    collectionClass [
        "Answer the collection class that's used to return the contents of the stream"
        ^Array
    ]
    
    
    close [
        ^self
    ]
    
    nl [
        "Write a new line"
        self nextPut: Character nl
    ]
    
    tab [
        "Write a tab character"
        self nextPut: Character tab
    ]
    
    cr [
        "Write a carriage return"
        self nextPut: Character cr
    ]
    
    space [
        "Write a space character"
        self nextPut: Character space
    ]
    
    
    store: anObject [
        "Store a String representation from which anObject can be reconstructed"
        anObject storeOn: self
    ]
    
    print: anObject [
        "Print a String representation of anObject"
        anObject printOn: self
    ]
]


    
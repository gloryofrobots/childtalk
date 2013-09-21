Collection extend [
    
    (+) newWithSize: aSize [
        <primitive: 'Collection_new'>
        self primitiveFailed
    ]
    
    (+) with: anObject [
    "Answer a collection with a single element"
    ^self new add: anObject; yourself
    ]
     
    (+) with: firstObject with: secondObject [
        "Answer a collection with the two given objects"
        ^self new
            add: firstObject;
            add: secondObject; yourself
    ]
    
    (+) with: firstObject with: secondObject with: thirdObject [
        "Answer a collection with the three given objects"
        ^self new
            add: firstObject;
            add: secondObject;
            add: thirdObject;
            yourself
    ]
    
    (+) with: firstObject with: secondObject with: thirdObject with: fourthObject [
        "Answer a collection with the four given objects"
        ^self new
            add: firstObject;
            add: secondObject;
            add: thirdObject;
            add: fourthObject;
            yourself
    ]
    
    (+) withAll: aCollection [
        "Create a collection whose elements are the same of aCollection"
        ^self new addAll: aCollection
    ]
    
    basicGrowTo: aSize [
        <primitive: 'Collection_growTo'>
        self primitiveFailed
    ]

    basicSize [
        <primitive: 'Collection_size'>
        self primitiveFailed
    ]
    
    size [
        ^ self basicSize.
    ]
    
    at: anIndex [
        <primitive: 'Collection_at'>
        self primitiveFailed
    ]
    
     at: anIndex put: anObject [
        <primitive: 'Collection_at_put'>
        self primitiveFailed
    ]
    
    growTo: anInteger [
        self basicGrowTo: anInteger
    ]
    
    growBy: anInteger [
        self growTo: self basicSize + anInteger
    ]
    
    add: anObject [
        self subclassResponsibility
    ]
    
    addAll: aCollection [
        aCollection do: [ :ea |
        self add: ea ]
    ]
    
    remove: anObject ifAbsent: exceptionHandler [
        "Remove anObject from the collection. Evaluate exceptionHandler if anObject is not found"
        self subclassResponsibility
    ]
    
    remove: anObject [
        "Remove anObject and fail if it's absent"
        ^self remove: anObject ifAbsent: [ self errorNotFound ]
    ]
    
    removeAll: aCollection [
        "Remove each element of aColection"
        aCollection do: [ :ea |
            self remove: ea ].
        ^aCollection
    ]
    
    
    copyEmpty [
        ^self copyEmpty: self size
    ]
    
    copyEmpty: newSize [
        "Create the empty collection of the given size"
        ^self species new: newSize
    ]
    
    copyWithout: oldElement [
        "Answer a collection with the same receiver's elements omitting those equivalent to oldElement"
        ^(self species withAll: self) remove: oldElement; yourself
    ]
    
    copyWith: newElement [
        "Answer a collection which is a copy of the receiver, and adds a newElement"
        ^(self species withAll: self) add: newElement; yourself
    ]
    
    
    do: aBlock [
        "Call aBlock for each element of the collection"
        self subclassResponsibility
    ]
    
    copyEmptyForCollect [
        ^self species new: self size
    ]
    
    collect: transformerBlock [
        "Evaluate the block for each element of the receiver.
         The results of these evaluations are collected into a new collection"
        | coll |
        coll := self copyEmptyForCollect.
        self do: [ :ea |
        coll add: (transformerBlock value: ea) ].
        ^coll
    ]
    
    reject: discriminatorBlock [
        "Return a collection of which elements are the ones that cause discriminatorBlock to return false"
        | coll |
        coll := self copyEmptyForCollect.
        self do: [ :ea |
        (discriminatorBlock value: ea)
            ifFalse: [ coll add: ea ] ].
        ^coll
    ]
    
    select: discriminatorBlock [
        "Return a collection of which elements are the ones that cause discriminatorBlock to return true"
        | coll |
        coll := self copyEmptyForCollect.
        self do: [ :ea |
        (discriminatorBlock value: ea)
            ifTrue: [ coll add: ea ] ].
        ^coll
    ]
    
    detect: discriminatorBlock [
        "Evaluate the block for each element of the receiver.
         Answer the first element that caused the block to return true.
         If no element is found, raise an exception"
        self do: [ :ea |
        | res |
        res := discriminatorBlock value: ea.
        res ifTrue: [ ^ea ] ].
        self errorNotFound
    ]
    
    detect: discriminatorBlock ifNone: exceptionHandler [
        "Evaluate the block for each element of the receiver.
         Answer the first element that caused the block to return true.
         Evaluate exceptionHandler if any error occurs"
        [ self detect: discriminatorBlock ]
        on: Error do: exceptionHandler
    ]
    
    inject: thisValue into: binaryBlock [
        "Evaluate the block for each element of the receiver,
         with the previous returned value of the block as first argument
         and the current element as second argument"
        | ret |
        ret := thisValue.
        self do: [ :ea |
            binaryBlock value: ret value: ea ].
        ^ret
    ]
       
    asArray [
        ^Array withAll: self
    ]
    
    asSet [
        ^Set withAll: self
    ]
    
    asBag [
        ^Bag withAll: self
    ]
    
    asString [
        ^String withAll: self
    ]
    
    asByteArray [
        ^ByteArray withAll: self
    ]
    
    asOrderedCollection [
        ^OrderedCollection withAll: self
    ]

    
    isEmpty [
        ^self size = 0
    ]
    
    notEmpty [
        ^self isEmpty not
    ]
    
    includes: anObject [
        "Return true if the receiver contains anObject, otherwise false"
        
        self do: [ :ea |
        ea = anObject
            ifTrue: [ ^true ] ].
        ^false
    ]
    
    occurrencesOf: anObject [
        "Answer how many times anObject occurred in the collection"
        | occurs |
        occurs := 0.
        self do: [ :ea |
        ea = anObject
            ifTrue: [ occurs := occurs + 1 ] ].
        ^occurs
    ]
    
    
    printOn: aStream [
        | tooMany |
        tooMany := aStream position + self maxPrint.
        aStream
            print: self class;
            nextPutAll: ' ('.
    
        self do: [ :ea |
            aStream position > tooMany
                ifTrue: [
                    aStream nextPutAll: '...etc...)'.
                    ^self ].
            ea == self
                ifTrue: [ '...' printOn: aStream ]
                ifFalse: [ ea printOn: aStream ].
            aStream space ].
        aStream nextPut: $)
    ]
    
    storeOn: aStream [
        aStream
            nextPut: $(;
            store: self class;
            nextPutAll: ' new'.
    
        self do: [ :ea |
            aStream nextPutAll: ' add: ';
            ea == self
                ifTrue: [ nil printOn: aStream ] "FIXME: we should not store nil"
                ifFalse: [ ea storeOn: aStream ].
            aStream nextPut: $; ].
    
        aStream nextPutAll: ' yourself)'
    ]
    
    
    maxPrint [
        ^5000
    ]
    
    
    errorNotKeyed [
        NotKeyed signal
    ]
]
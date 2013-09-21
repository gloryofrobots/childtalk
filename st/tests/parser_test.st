"
Object subclass: Rectangle [
    |origin corner|
    
    /classVar ass dd ass ds/
    
    center [
        ^ origin + corner / 2
    ]
    
    extent: newExtent [
        corner :=  origin + newExtent.
        corner := corner + 34.
    ]
    
    intersects: aRectangle [
        ^ (origin max: aRectangle origin) < (corner min: aRectangle corner)
    ]
    
    testParenthesis [
        ^ 4 + (3 - 6)
    ]
    
    incrementIndex [
        ^ index <- index + 4
    ]
    
    merge:aRectancgle [
        | minPoint maxPoint |
        minPoint := origin min: aRectangle origin.
        maxPoint := origin max: aRectangle corner.
        ^ Rectangle origin: minPoint
                    corner: maxPoint
    ]
    
    includesPoint: aPoint [
        origin <= aPoint
            ifTrue: [^aPoint]
            ifFalse: [^true]
    ]
    
    classes [
        ^ self collect: [ :element | element class]
    ]
    
    containsInstanceOf: aClass [
        |x|
         self do: [ :element | (element isKindOf: aClass) ifTrue: [^ true]].
         ^ false
    ]
]
Rectangle extend [
    goFuck:hour [
        ^ hour
    ]
        
]
EMPTY ARRAY!!!!!!!!"

Eval [
    'sa\n' echo.
]
"
Object subclass: R [

    testClassMembershipMethods [
        self assertTrue: (String isMemberOf: String class.) withMessage: 'should be String.'.
    ]
    
    isKindOf: aClass [
        | class |
        aClass = self ifTrue: [^true].
        aClass ifNil: [ ^false ].
        class := self class.
        
        class = aClass ifTrue: [ ^true ].
        aClass allSubclassesDo: [ :ea |  (class isKindOf: ea) ifTrue: [ ^true ] ].
        ^false
    ]
    
    
    printOn: aStream [
        aStream
            nextPutAll: self class article;
            space;
           
            nextPutAll: self class name
    ]
    
     basicAt: anIndex [
        anIndex >= self basicSize
            ifTrue: [ self error: 'Index out of bounds' ].
        ^Character value: (self byteAt: anIndex)
    ]

    test: aNumber [
        ^ self test:(aNumber + 2) test1:2 test2:3.
    ]
    
    test: aNum1 test1: aNum2 test2: aNum3 [
        ^ aNum1  + (aNum2 * aNum3) 
    ]
    

    initializeHandle: aMode [
            
            aMode
            caseOf: {
                
                [ self class read ] -> [ self allowReadOnly ].
                [ self class write ] -> [ self allowWriteOnly ].
                [ self class readWrite ] -> [ self allowReadAndWrite ].
                [ self class readWriteCreate ] -> [ self allowReadAndWrite ] }
        ]
    
    new: size [
        <primitive: 'Array_newColon'>
    ]
    
    -> anObject [
        ^Association key: self value: anObject
    ]
    
   reverseDo: aBlock [
        self from: self size to: 1 by: -1 do: [ :ea |
        aBlock value: ea ]
    ]
    
]
"

 
"
Eval [ 
    nil subclass: #Object
        instanceVariableNames: ''
        classVariableNames: ''.
    
    Object subclass: #Behavior
           instanceVariableNames: 'name superclass instanceVariableNames instanceSize methodDictionary subclasses finalizationRequest'
           classVariableNames: ''.
           
]"
   
      
  
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
Object subclass: R [
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
        "Create a new association between myself and anObject"
        ^Association key: self value: anObject
    ]
    
   reverseDo: aBlock [
        "Call aBlock traversing the elements of the collection in the opposite order of do:"
        self from: self size to: 1 by: -1 do: [ :ea |
        aBlock value: ea ]
    ]
    
]


 
"
Eval [ 
    nil subclass: #Object
        instanceVariableNames: ''
        classVariableNames: ''.
    
    Object subclass: #Behavior
           instanceVariableNames: 'name superclass instanceVariableNames instanceSize methodDictionary subclasses finalizationRequest'
           classVariableNames: ''.
           
]"
   
      
  
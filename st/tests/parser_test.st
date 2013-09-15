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
"
Object subclass: R [
    extent: newExtent [
           
            self put: 2; put: 3; put: 4.
            self color: 34.
            super prepend: 13.
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
   
      
  
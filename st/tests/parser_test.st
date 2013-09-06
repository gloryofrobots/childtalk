Object subclass: Rectangle [
    <instanceVariableNames:'origin corner'>
    
    "center [
        ^ origin + corner / 2
    ]
    
    extent: newExtent [
        corner :=  origin + newExtent
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
    ]"
    
    containsInstanceOf: aClass [
        |x|
         self do: [ :element | (element isKindOf: aClass) ifTrue: [^ true]].
         ^ false
    ]
]
    
      
  
Object subclass: Rectangle [
    <instanceVariableNames:'origin corner'>
    
    "center [
        ^ origin + corner / 2
    ]
    
    extent: newExtent [
        corner :=  origin + newExtent
    ]"
    
    intersects: aRectangle [
        ^ (origin max: aRectangle origin) < (corner min: aRectangle corner)
    ]
    
]
    
      
  
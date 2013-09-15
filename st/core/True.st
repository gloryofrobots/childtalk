True extend [
   
    
    isBoolean [
        ^true
    ]
    
    
    ifTrue: trueBlock ifFalse: falseBlock [
        ^trueBlock value
    ]
    
    ifTrue: trueBlock [
        ^trueBlock value
    ]
        
    ifFalse: falseBlock [
        ^nil
    ]
    
    & operand [
        ^operand
    ]
    
    | operand [
        ^true
    ]
    
    and: aBlock [
        ^aBlock value
    ]
    
    or: aBlock [
        ^true
    ]
    
    not [
        ^false
    ]
    
    
    printOn: aStream [
        aStream nextPutAll: 'true'
    ]
    
    storeOn: aStream [
        aStream nextPutAll: 'true'
    ]
]
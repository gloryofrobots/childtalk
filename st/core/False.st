False extend [

    ifTrue: trueBlock ifFalse: falseBlock [
        ^falseBlock value
    ]
    
    
    ifTrue: trueBlock [
        ^nil
    ]
    
    ifFalse: falseBlock [
        ^falseBlock value
    ]
    
    & operand [
        ^false
    ]
    
    | operand [
        ^operand
    ]
    
    and: aBlock [
        ^false
    ]
    
    or: aBlock [
        ^aBlock value
    ]
    
    not [
        ^true
    ]
    
    
    printOn: aStream [
        aStream nextPutAll: 'false'
    ]
    
    storeOn: aStream [
        aStream nextPutAll: 'false'
    ]
]
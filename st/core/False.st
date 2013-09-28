"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

Boolean subclass: False [

    ifTrue: trueBlock ifFalse: falseBlock [
        ^falseBlock value
    ]
    
    
    ifTrue: trueBlock [
        ^nil
    ]
    
    ifFalse: falseBlock [
        ^falseBlock value
    ]
    
    bitAnd: operand [
        ^false
    ]
    
    bitOr: operand [
        ^operand
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
"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

Boolean subclass: True [
   
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
    
    bitAnd: operand [
        ^operand
    ]
    
    bitOr: operand [
        ^true
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
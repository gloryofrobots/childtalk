"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

String subclass: Symbol [
  
   (+) new [
        self shouldNotImplement
    ]
    
   (+) basicNew [
        self shouldNotImplement
    ]

    at: anIndex put: anObject [
        self shouldNotImplement
    ]
    
    basicAt: anIndex put: anObject [
        self shouldNotImplement
    ]
  
    asString [
        ^String withAll: self
    ]
    
    asSymbol [
        ^self
    ]
    
    printOn: aStream [
        aStream
            nextPut: $#;
            nextPutAll: self
    ]
    
    storeOn: aStream [
        aStream
            nextPut: $#;
            nextPutAll: self
    ]
]
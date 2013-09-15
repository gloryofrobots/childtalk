Symbol extend [
    new: count [
        self shouldNotImplement
    ]
    
    new [
        self shouldNotImplement
    ]
    
    basicNew [
        self shouldNotImplement
    ]
    
    basicNew: count [
        self shouldNotImplement
    ]
    
    
    at: anIndex put: anObject [
        self shouldNotImplement
    ]
    
    basicAt: anIndex put: anObject [
        self shouldNotImplement
    ]
    
    hash [
        ^hash
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
    
    
    isString [
        ^false
    ]
    
    isSymbol [
        ^true
    ]
    
    
]
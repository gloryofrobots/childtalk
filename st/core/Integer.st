Integer extend [

    gcd: aNumber [
        "Answer the greatest common divisor"
        | num tmp remainder |
        num := self max: aNumber.
        remainder := self min: aNumber.
        [ remainder isZero ]
    	whileFalse: [    
    	    tmp := remainder.
    	    remainder := num \\ remainder.
    	    num := tmp ].
        ^num abs
    ]
    
    
    allMask: anInteger [
        "Answer true if all 1 bits of the receiver are 1 also in the argument"
        ^(self bitAnd: anInteger) == anInteger
    ]
    
    anyMask: anInteger [
        "Answer true if any of the bits that are 1 in the argument are 1 in the receiver"
        ^(self bitAnd: anInteger) ~~ anInteger
    ]
    
    noMask: anInteger [
        "Answer true if none of the bits that are 1 in the argument are 1 in the receiver"
        ^(self bitAnd: anInteger) == 0
    ]
    
    bitAnd: aNumber [
        aNumber isInteger
    	ifFalse: [ self error: 'An integer is required' ].
    
        self generality > aNumber generality
    	ifTrue: [ ^self bitAnd: (self coerce: aNumber) ]
    	ifFalse: [ ^(aNumber coerce: self) bitAnd: aNumber ]
    ]
    
    bitOr: aNumber [
        aNumber isInteger
    	ifFalse: [ self error: 'An integer is required' ].
    
        self generality > aNumber generality
    	ifTrue: [ ^self bitOr: (self coerce: aNumber) ]
    	ifFalse: [ ^(aNumber coerce: self) bitOr: aNumber ]
    ]
    
    bitXor: aNumber [
        aNumber isInteger
    	ifFalse: [ self error: 'An integer is required' ].
    
        self generality > aNumber generality
    	ifTrue: [ ^self bitXor: (self coerce: aNumber) ]
    	ifFalse: [ ^(aNumber coerce: self) bitXor: aNumber ]
    ]
    
    bitShift: aNumber [
        aNumber isSmallInteger
    	ifFalse: [ self error: 'A small integer is required' ].
       
        ^self asLargeInteger bitShift: aNumber
    ]
    
    bitAt: index [
        "Answer the bit 0 or 1 at the given index"
        ^(self bitShift: (index - 1) negated) bitAnd: 1
    ]
    
    bitInvert [
        "Answer an Integer whose bits are inverted"
        ^self bitXor: -1
    ]
    
    << aNumber [
        ^self bitShift: aNumber
    ]
    
    >> aNumber [
        ^self bitShift: aNumber negated
    ]
    
    
    radix: base [
        "Returns a string representing the integer with the given base.
         The base must be between 2 and 16"
        | stream num |
        base isInteger & (base between: 2 and: 16)
    	ifFalse: [ self error: 'Argument must be an integer between 2 and 16' ].
        self isZero
    	ifTrue: [ ^'0' ].
        stream := WriteStream on: String new.
        num := self abs.
        [ num ~= 0 ]
    	whileTrue: [
    	    | cp |
    	    cp := $0 asciiValue + (num \\ base).
    	    cp > $9 asciiValue
    		ifTrue: [ cp := cp + $A asciiValue - $9 asciiValue - 1 ].
    	    stream nextPut: (Character value: cp).
    	    num := num // base ].
        self negative
    	ifTrue: [ stream nextPut: $- ].
        ^stream contents reverse
    ]
    
    printStringRadix: base [
        | stream |
        stream := WriteStream on: String new.
        self printOn: stream base: base.
        ^stream contents
    ]
    
    printOn: aStream [
        self printOn: aStream base: 10
    ]
    
    storeOn: aStream [
        self storeOn: aStream base: 10
    ]
    
    printOn: aStream base: base [
        aStream nextPutAll: (self radix: base)
    ]
    
    storeOn: aStream base: base [
        aStream nextPutAll: (self radix: base)
    ]
    
    
    ceiling [
        ^self
    ]
    
    floor [
        ^self
    ]
    
    truncated [
        ^self
    ]
    
    
    isInteger [
        ^true
    ]
    
    
    timesRepeat: aBlock [
        1 to: self to: [ :ea |
            aBlock value ]
    ]
]
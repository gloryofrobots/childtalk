Fraction extend [
    
    numerator: topNumber denominator: bottomNumber [
        ^self basicNew
    	initializeNumerator: topNumber denominator: bottomNumber;
    	yourself
    ]
    
    
    initializeNumerator: topNumber denominator: bottomNumber [
        numerator := topNumber.
        denominator := bottomNumber.
        self ensureDenominator
    ]
    
    
    numerator [
        ^numerator
    ]
    
    denominator [
        ^denominator
    ]
    
    
    ensureDenominator [
        denominator isZero
        	ifTrue: [ ^self zeroDivide ].
        denominator negative
        	ifTrue: [ numerator := numerator negated. denominator := denominator negated ]
    ]
    
    
    ceiling [
        ^self asFloat ceiling
    ]
    
    floor [
        ^self asFloat floor
    ]
    
    truncated [
        ^self asFloat truncated
    ]
    
    reduce [
        | gcd |
        gcd := numerator gcd: denominator.
        numerator = 0
    	ifTrue: [ ^0 ].
    
        numerator := numerator / gcd.
        denominator := denominator / gcd.
    
        denominator = 1
    	ifTrue: [ ^numerator ].
        numerator = denominator
    	ifTrue: [ ^1 ].
        ^self
    ]
    
    + aNumber [
        aNumber generality > self generality
    	ifTrue: [ ^(aNumber coerce: self) + aNumber ].
    
        ^aNumber isFraction
    	ifTrue: [
    	    (Fraction
    		 numerator: (numerator * aNumber denominator) + (denominator * aNumber numerator)
    		 denominator: denominator * aNumber denominator) reduce ]
    	ifFalse: [
    	    (Fraction
    		 numerator: numerator + (denominator * aNumber)
    		 denominator: denominator) reduce ]
    ]
    
    - aNumber [
        aNumber generality > self generality
    	ifTrue: [ ^(aNumber coerce: self) - aNumber ].
    
        ^aNumber isFraction
    	ifTrue: [
    	    (Fraction
    		 numerator: (numerator * aNumber denominator) - (denominator * aNumber numerator)
    		 denominator: denominator * aNumber denominator) reduce ]
    	ifFalse: [
    	    (Fraction
    		 numerator: numerator - (denominator * aNumber)
    		 denominator: denominator) reduce ]
    ]
    
    * aNumber [
        aNumber generality > self generality
    	ifTrue: [ ^(aNumber coerce: self) - aNumber ].
    
        ^aNumber isFraction
    	ifTrue: [
    	    (Fraction
    		 numerator: numerator * aNumber numerator
    		 denominator: denominator * aNumber denominator) reduce ]
    	ifFalse: [
    	    (Fraction
    		 numerator: numerator * aNumber
    		 denominator: denominator) reduce ]
    ]
    
    / aNumber [
        ^self * aNumber reciprocal
    ]
    
    reciprocal [
        ^Fraction numerator: denominator denominator: numerator
    ]
    
    < aNumber [
        ^(self compare: aNumber) < 0
    ]
    
    > aNumber [
        ^(self compare: aNumber) > 0
    ]
    
    <= aNumber [
        ^(self compare: aNumber) <= 0
    ]
    
    >= aNumber [
        ^(self compare: aNumber) >= 0
    ]
    
    = aNumber [
        ^(self compare: aNumber) = 0
    ]
    
    ~= aNumber [
        ^(self compare: aNumber) ~= 0
    ]
    
    compare: aNumber [
        | a b |
        aNumber generality > self generality
    	ifTrue: [ ^(aNumber coerce: self) compare: aNumber ].
    
        ^aNumber isFraction
    	ifTrue: [
    	    | d |
    	    d := denominator * aNumber denominator.
    	    a := d * self numerator.
    	    b := d * aNumber numerator.
    	    a compare: b ]
    	ifFalse: [
    	    numerator compare: denominator * aNumber ]
    ]
    
    
    coerce: aNumber [
        ^aNumber asFraction
    ]
    
    generality [
        ^40
    ]
    
    asFloat [
        ^numerator asFloat / denominator asFloat
    ]
    
    
    printOn: aStream [
        aStream
    	nextPut: $(;
    	nextPutAll: numerator printString;
    	nextPut: $/;
    	nextPutAll: denominator printString;
    	nextPut: $)
    ]
    
    
    zero [
        ^Fraction numerator: 0 denominator: 1
    ]
    
    sign [
        ^denominator sign
    ]
    
    isFraction [
        ^true
    ]
]
LargeInteger extend [
    (+) new: aNumber [
        <primitive: 'LargeInteger_newColon'>
    ]
    
    clear [
        <primitive: 'LargeInteger_clear'>
    	self primitiveFailed
    ]
    
    finalize [
        self clear
    ]
    
    
    asFloat [
        <primitive: 'LargeInteger_asFloat'>
    	self primitiveFailed
    ]
    
    asLargeInteger [
        ^self
    ]
    
    asInteger [
        ^self
    ]
    
    generality [
        ^20
    ]
    
    zero [
        ^0 asLargeInteger
    ]
    
    unity [
        ^1 asLargeInteger
    ]
    
    coerce: aNumber [
        ^aNumber asLargeInteger
    ]
    
    
    isLargeInteger [
        ^true
    ]
    
    
    < aNumber [
         <primitive: 'LargeInteger_lt'>
    	 ^super < aNumber
    ]
    
    > aNumber [
         <primitive: 'LargeInteger_gt'>
    	 ^super > aNumber
    ]
    
    <= aNumber [
         <primitive: 'LargeInteger_le'>
    	 ^super <= aNumber
    ]
    
    >= aNumber [
         <primitive: 'LargeInteger_ge'>
    	 ^super >= aNumber
    ]
    
    = aNumber [
         <primitive: 'LargeInteger_eq'>
    	 ^super = aNumber
    ]
    
    == aNumber [
         <primitive: 'LargeInteger_eq'>
    	 ^super == aNumber
    ]
    
    ~= aNumber [
         <primitive: 'LargeInteger_ne'>
    	 ^super ~= aNumber
    ]
    
    ~~ aNumber [
         <primitive: 'LargeInteger_ne'>
    	 ^super ~~ aNumber
    ]
    
    
    + aNumber [
         <primitive: 'LargeInteger_plus'>
    	 ^super + aNumber
    ]
    
    - aNumber [
         <primitive: 'LargeInteger_minus'>
    	 ^super - aNumber
    ]
    
    / aNumber [
         <primitive: 'LargeInteger_div'>
    	 ^aNumber isLargeInteger
    	     ifTrue: [ (Fraction numerator: self denominator: aNumber) reduce ]
    	     ifFalse: [ super / aNumber ]
    ]
    
    * aNumber [
         <primitive: 'LargeInteger_mul'>
    	 ^super * aNumber
    ]
    
    // aNumber [
         <primitive: 'LargeInteger_intDiv'>
    	 self generality > aNumber generality
    	     ifTrue: [ ^self // (self coerce: aNumber) ]
    	     ifFalse: [ ^(aNumber coerce: self) // aNumber ]
    ]
    
    quo: aNumber [
         <primitive: 'LargeInteger_quo'>
    	 self generality > aNumber generality
    	     ifTrue: [ ^self // (self coerce: aNumber) ]
    	     ifFalse: [ ^(aNumber coerce: self) // aNumber ]
    ]
    
    \\ aNumber [
         <primitive: 'LargeInteger_mod'>
    	 ^super \\ aNumber
    ]
    
    
    bitAnd: aSmallInteger [
        <primitive: 'LargeInteger_bitAnd'>
    	self primitiveFailed
    ]
    
    bitOr: aSmallInteger [
        <primitive: 'LargeInteger_bitOr'>
    	self primitiveFailed
    ]
    
    bitXor: aSmallInteger [
        <primitive: 'LargeInteger_bitXor'>
    	self primtiveFailed
    ]
    
    bitShift: aSmallInteger [
        <primitive: 'LargeInteger_bitShift'>
    	self primitiveFailed
    ]
]
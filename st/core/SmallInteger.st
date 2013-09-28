"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

Integer subclass: SmallInteger [
    (+) new: aNumber [
        <primitive: 'SmallInteger_newColon'>
    ]
    
    + aNumber [
         <primitive: 'SmallInteger_plus'>
    	 ^aNumber isSmallInteger
    	     ifTrue: [ self asLargeInteger + aNumber asLargeInteger ]
    	     ifFalse: [ super + aNumber ]
    ]
    
    - aNumber [
         <primitive: 'SmallInteger_minus'>
    	 ^aNumber isSmallInteger
    	     ifTrue: [ self asLargeInteger - aNumber asLargeInteger ]
    	     ifFalse: [ super - aNumber ]
    ]
    
    * aNumber [
         <primitive: 'SmallInteger_mul'>
    	 ^aNumber isSmallInteger
    	     ifTrue: [self asLargeInteger * aNumber asLargeInteger ]
    	     ifFalse: [ super * aNumber ]
    ]
    
    < aNumber [
         <primitive: 'SmallInteger_lt'>
    	 ^super < aNumber
    ]
    
    > aNumber [
         <primitive: 'SmallInteger_gt'>
    	 ^super > aNumber
    ]
    
    <= aNumber [
         <primitive: 'SmallInteger_le'>
    	 ^super <= aNumber
    ]
    
    >= aNumber [
         <primitive: 'SmallInteger_ge'>
    	 ^super >= aNumber
    ]
    
    = aNumber [
         <primitive: 'SmallInteger_eq'>
    	 ^super = aNumber
    ]
    
    == aNumber [
         <primitive: 'SmallInteger_eq'>
    	 ^super == aNumber
    ]
    
    ~= aNumber [
         <primitive: 'SmallInteger_ne'>
    	 ^super ~= aNumber
    ]
    
    ~~ aNumber [
         <primitive: 'SmallInteger_ne'>
    	 ^super ~~ aNumber
    ]
    
    
    / aNumber [
        <primitive: 'SmallInteger_div'>
        ^ super / aNumber
    ]
    

    \\ aNumber [
     <primitive: 'SmallInteger_mod'>
     ^super \\ aNumber 
    ]
    
    bitAnd: aNumber [
        <primitive: 'SmallInteger_bitAnd'>
    	^super bitAnd: aNumber
    ]
    
    bitOr: aNumber [
        <primitive: 'SmallInteger_bitOr'>
    	^super bitOr: aNumber
    ]
    
    bitXor: aNumber [
        <primitive: 'SmallInteger_bitXor'>
    	^super bitXor: aNumber
    ]
    
    bitShift: aNumber [
        <primitive: 'SmallInteger_bitShift'>
            ^super bitShift: aNumber
    ]
    
    
    isSmallInteger [
        ^true
    ]
    
    
    asInteger [
        ^self
    ]
    
    asFloat [
        <primitive: 'SmallInteger_asFloat'>
    	self primitiveFailed
    ]
    
    asLargeInteger [
        <primitive: 'SmallInteger_asLargeInteger'>
    	self primitiveFailed
    ]
    
    zero [
        ^0
    ]
    
    generality [
        ^10
    ]
    
    unity [
        ^1
    ]
]
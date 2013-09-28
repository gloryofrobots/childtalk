"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

Number subclass: Float [
    (+) new: aNumber [
        <primitive: 'Float_newColon'>
    ]
    
    asFraction [
        ^self notYetImplemented
    ]
    
    asFloat [
        ^self
    ]
    
    unity [
        ^1.0
    ]
    
    generality [
        ^50
    ]
    
    zero [
        ^0.0
    ]
    
    coerce: aNumber [
        ^aNumber asFloat
    ]
    
    isFloat [
        ^true
    ]
    
    < aNumber [
         <primitive: 'Float_lt'>
    	 ^super < aNumber
    ]
    
    > aNumber [
         <primitive: 'Float_gt'>
    	 ^super > aNumber
    ]
    
    <= aNumber [
         <primitive: 'Float_le'>
    	 ^super <= aNumber
    ]
    
    >= aNumber [
         <primitive: 'Float_ge'>
    	 ^super >= aNumber
    ]
    
    = aNumber [
         <primitive: 'Float_eq'>
    	 ^super = aNumber
    ]
    
    == aNumber [
         <primitive: 'Float_eq'>
    	 ^super == aNumber
    ]
    
    ~= aNumber [
         <primitive: 'Float_ne'>
    	 ^super ~= aNumber
    ]
    
    ~~ aNumber [
         <primitive: 'Float_ne'>
    	 ^super ~~ aNumber
    ]
    
    
    + aNumber [
         <primitive: 'Float_plus'>
    	 ^super + aNumber
    ]
    
    - aNumber [
         <primitive: 'Float_minus'>
    	 ^super - aNumber
    ]
    
    * aNumber [
         <primitive: 'Float_mul'>
    	 ^super * aNumber
    ]
    
    / aNumber [
         <primitive: 'Float_div'>
         ^super / aNumber
    ]
    
    ceiling [
        <primitive: 'Float_ceil'>
    	self primitiveFailed
    ]
    
    floor [
        <primitive: 'Float_floor'>
    	self primitiveFailed
    ]
    
    truncated [
        <primitive: 'Float_trunc'>
    	self primitiveFailed
    ]
]
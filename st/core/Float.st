Float extend [
 
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
    
    
    printNl [
        ^self print
    ]
    
    print [
        <primitive: 'Float_print'>
    	self primitiveFailed
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
    
    \\ aNumber [
         self notYetImplemented
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
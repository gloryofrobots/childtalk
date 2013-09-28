"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

ArrayedCollection subclass: ByteArray [
    
       (+) new: size [
            ^ self basicNew:size
        ]
        
       (+) basicNew: size [
            <primitive: 'ByteArray_newColon'>
        	self primitiveFailed
        ] 
        
        basicSize [
            <primitive: 'ByteArray_size'>
        ]
        
        at: anIndex [
            <primitive: 'ByteArray_at'>
        	self primitiveFailed
        ]
    
        at: anIndex put: aSmallInteger [
            <primitive: 'ByteArray_at_put'>
        	self primitiveFailed
        ]
    
        basicAt: anIndex [
            <primitive: 'ByteArray_at'>
        	self primitiveFailed
        ]
    
        basicAt: anIndex put: aSmallInteger [
            <primitive: 'ByteArray_at_put'>
        	self primitiveFailed
        ]
    
        byteAt: anIndex [
            <primitive: 'ByteArray_at'>
        	self primitiveFailed
        ]
    
        byteAt: anIndex put: aSmallInteger [
            <primitive: 'ByteArray_at_put'>
        	self primitiveFailed
        ] 
        
        
         printOn: aStream [
            aStream
                nextPutAll: self.
            
        ]
    
        storeOn: aStream [
            aStream
               nextPutAll: self.
        ]
    
        growTo: anInteger [
            <primitive: 'ByteArray_growTo'>
            self primitiveFailed
        ]
        + aObj [
            <primitive: 'ByteArray_concat'>
            self primitiveFailed.
         ]
    
        replaceFrom: startIndex to: stopIndex with: aCollection startingAt: anIndex [
        	| element j endIndex|
        	j := anIndex - 1.
        	
        	startIndex to: stopIndex do: [ :i |
        	    element := aCollection at: (j := j + 1).
        	    (element isKindOf: Character) 
        	       ifFalse: [ (element isKindOf: SmallInteger) 
        	                ifTrue: [((element < 0) bitOr: (element > 255)) 
        	                           ifTrue: [self primitiveFailed] ]
        	                ifFalse: [self primitiveFailed] ].
                
                self at:i put: element].
        ] 
    ]
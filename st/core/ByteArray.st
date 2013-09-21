ByteArray extend [
    
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
            
        growTo: anInteger [
            <primitive: 'ByteArray_growTo'>
            self primitiveFailed
        ]
    
        replaceFrom: startIndex to: stopIndex with: aCollection startingAt: anIndex [
        	| element j endIndex|
        	j := anIndex - 1.
        	
        	startIndex to: stopIndex do: [ :i |
        	    element := aCollection at: (j := j + 1).
        	    (element isCharacter or: [ element isSmallInteger & (element >= 0) & (element <= 255) ])
        		ifTrue: [ self at: i put: element ]
        		ifFalse: [ self primitiveFailed ] ]		
        ] 
    ]
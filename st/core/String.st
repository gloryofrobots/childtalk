String extend [

    new: anIndex [
        "Add 1 to anIndex to make room for NULL character"
        ^super new: anIndex + 1
    ]
    
    
    at: anIndex [
        ^self basicAt: anIndex
    ]
    
    at: anIndex put: aCharacter [
        ^self basicAt: anIndex put: aCharacter
    ]
    
    basicAt: anIndex [
        anIndex >= self basicSize
            ifTrue: [ self error: 'Index out of bounds' ].
        ^Character value: (self byteAt: anIndex)
    ]
    
    basicAt: anIndex put: aCharacter [
        anIndex >= self basicSize
            ifTrue: [ self error: 'Index out of bounds' ].
        aCharacter isCharacter
    	ifFalse: [ self error: 'A character is required got ', aCharacter printString ].
        self byteAt: anIndex put: aCharacter asciiValue.
        ^aCharacter
    ]
    
    
    displayOn: aStream [
        aStream nextPutAll: self
    ]
    
    printOn: aStream [
        aStream
            nextPut: $';
            nextPutAll: self;
            nextPut: $'
    ]
    
    storeOn: aStream [
        aStream nextPut: $'.
        self do: [ :ea |
            ea = $'
                ifTrue: [ aStream nextPut: $' ].
            aStream
                nextPut: ea ].
        aStream nextPut: $'
    ]
    
    
    hash [
        <primitive: 'String_hash'>
    	self primitiveFailed
    ]
    
    size [
        "The size is less by 1 because of the NULL character"
        ^self basicSize - 1
    ]
    
    
    subStrings: separators [
        | stream index size subindex |
        stream := WriteStream on: Array new.
        size := separators size.
        index := 1.
        [ index <= self size ]
    	whileTrue: [
    	    subindex := self indexOfSubCollection: separators startingAt: index.
    	    subindex = 0
    		ifTrue: [ stream nextPut: (self copyFrom: index to: self size).
    			  ^stream contents ].
    	    stream nextPut: (self copyFrom: index to: subindex - 1).
    	    index := subindex + size ].
        ^stream contents
    ]
    
    
    primParse: aString on: aMethod for: aClass [
        <primitive: 'Compiler_parse'>
    	self primitiveFailed
    ]
    
    compileFor: aClass [
        | method |
        method := CompiledMethod new.
        ^self primParse: self on: method for: aClass    
    ]
    
    doIt [
        | method context |
        method := ('goDoIt ', self) compileFor: nil class.
        ^method runOn: nil withArguments: #()
    ]
    
    
    asSymbol [
        <primitive: 'String_asSymbol'>
    	self primitiveFailed
    ]
    
    asString [
        ^self
    ]
    
    asUppercase [
        ^self collect: [ :ea | ea asUppercase ]
    ]
    
    asLowercase [
        ^self collect: [ :ea | ea asLowercase ]
    ]
    
    asNumber [
        ^Number readFrom: (ReadStream on: self)
    ]
    
    
    isCapitalized [
        ^self first isUppercase
    ]
    
    isString [
        ^true
    ]
    
    
    sameAs: aCollection [
        self size == aCollection size
    	ifFalse: [ ^false ].
    
        self with: aCollection do: [ :a :b |
    	a = b
    	    ifFalse: [ ^false ] ].
    
        ^true
    ]
    
    = aString [
         self hash == aString hash
    	ifFalse: [ ^false ].
    
         ^super = aString
    ]
    
    < aString [
        ^(self compare: aString) < 0
    ]
    
    > aString [
        ^(self compare: aString) > 0
    ]
    
    <= aString [
        ^(self compare: aString) <= 0
    ]
    
    >= aString [
        ^(self compare: aString) >= 0
    ]
    
    compare: aString [
        1 to: (self size min: aString size) do: [ :i |
    	| n1 n2 |
    	n1 := (self at: i) asciiValue.
    	n2 := (aString at: i) asciiValue.
    	n1 = n2 ifFalse: [ ^n1 - n2 ] ].
    
        ^self size - aString size
    ]
]
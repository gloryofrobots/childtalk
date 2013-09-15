Character extend [
    
    new [
        self shouldNotImplement
    ]
    
    initialize [
        WhiteSpaceTable := { Character cr. Character nl. Character lf. Character space. Character tab.
    			 Character verticalTab. Character ff } asSet.
        XDigitsTable := (Set new: 40) addAll: #($0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $a $b $c $d $e $f $A $B $C $D $E $F)
    ]
    
    whiteSpaces [
        ^WhiteSpaceTable
    ]
    
    separators [
        ^WhiteSpaceTable
    ]
    
    backspace [
        "Answer a backspace character"
        ^self value: 8
    ]
    
    esc [
        "Answer an esc character"
        ^self value: 27
    ]
    
    cr [
        "Answer a carriage return character"
        ^self value: 13
    ]
    
    nl [
        "Answer a new line character"
        ^self value: 10
    ]
    
    lf [
        "Answer a line feed character"
        ^self value: 10
    ]
    
    space [
        "Answer a space character"
        ^self value: 32
    ]
    
    tab [
        "Answer a tab character"
        ^self value: 9
    ]
    
    verticalTab [
        "Answer a vertical tab character"
        ^self value: 11
    ]
    
    ff [
        "Answer a form-feed character"
        ^self value: 12
    ]
    
    newPage [
        "Answer a form-feed character"
        ^self value: 12
    ]
    
    codePoint: anInteger [
        ^self value: anInteger
    ]
    
    value: anInteger [
        "Create a Character from the given integer representation"
        <primitive: 'Character_new'>
    	self primitiveFailed
    ]
    
    digitValue: anInteger [
        "Create a Character whose digit value is anInteger"
        ^self value: anInteger + $0 asciiValue
    ]
    
    
    codePoint [
        ^self asciiValue
    ]
    
    asciiValue [
        "Answer the integer rapresentation of myself"
        <primitive: 'Character_value'>
    	self primitiveFailed
    ]
    
    digitValue [
        "Answer the number corresponding to the digit value of the receiver"
        ^self asciiValue - $0 asciiValue
    ]
    
    
    < aCharacter [
         ^self codePoint < aCharacter codePoint
    ]
    
    > aCharacter [
         ^self codePoint > aCharacter codePoint
    ]
    
    <= aCharacter [
         ^self codePoint <= aCharacter codePoint
    ]
    
    >= aCharacter [
         ^self codePoint >= aCharacter codePoint
    ]
    
    = aCharacter [
         aCharacter isCharacter
    	 ifFalse: [ ^false ].
         ^self codePoint = aCharacter codePoint
    ]
    
    == aCharacter [
         aCharacter isCharacter
    	 ifFalse: [ ^false ].
         ^self codePoint = aCharacter codePoint
    ]
    
    
    asCharacter [
        ^self
    ]
    
    asString [
        "Return a new string with a single character"
        ^String with: self
    ]
    
    asUppercase [
        self isLowercase
            ifTrue: [ ^Character value: self codePoint - 32 ]
    ]
    
    asLowercase [
        self isUppercase
            ifTrue: [ ^Character value: self codePoint + 32 ]
    ]
    
    
    printOn: aStream [
        aStream
            nextPut: $$;
            nextPut: self
    ]
    
    storeOn: aStream [
        aStream
            nextPut: $$;
            nextPut: self
    ]
    
    
    isCharacter [
        ^true
    ]
    
    isWhiteSpace [
        ^WhiteSpaceTable includes: self
    ]
    
    isSeparator [
        ^self isWhiteSpace
    ]
    
    isLetter [
        ^self isLowercase | self isUppercase
    ]
    
    isDigit [
        ^self between: $0 and: $9
    ]
    
    isXDigit [
        ^XDigitsTable includes: self
    ]
    
    isAlphaNumeric [
        ^self isDigit | self isLetter
    ]
    
    isLowercase [
        ^self between: $a and: $z
    ]
    
    isUppercase [
        ^self between: $A and: $Z
    ]
    
    isVowel [
        ^#($a $e $i $o $u) includes: self asLowercase
    ]
]
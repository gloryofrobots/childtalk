"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

Object subclass: Transcript [
    \crSymbol\
    
    (+) show: anObject [
        <primitive: 'Transcript_show'>
    ]
    
    (+) showCr: anObject [
        self show: anObject; cr;
    ]
    
    (+) lineSeparator [
        crSymbol 
            ifNil: [crSymbol := SmallTalk getProperty:'line.separator'.].
        ^ crSymbol.
    ]
    
    (+) cr [
        self show: self lineSeparator.
    ]
]
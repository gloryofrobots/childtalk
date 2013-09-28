"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"
Object subclass: UndefinedObject [
        
        printOn: aStream [
            aStream nextPutAll: 'nil'
        ] 
        
        copy [
            ^ self.
        ]
        
        ifNil: aBlock [
            ^aBlock value
        ]
        
        ifNotNil: aBlock [
            ^self
        ]
        
        isNil [
            ^true
        ]
        
        notNil [
            ^false
        ] 
]
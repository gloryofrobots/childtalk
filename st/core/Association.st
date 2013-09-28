"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"


 Object subclass: Association[
    |key value|
    (+) key: aKey value: aValue [
        "Create an association aKey -> aValue"
        ^self new key: aKey value: aValue
    ]
    
    
    key [
        "Answer the association's key"
        ^key
    ]
    
    value: aValue [
        "Set the value of the association to aValue"
        value := aValue
    ]
    
    key: aKey value: aValue [
        "Set the key and the value of the association"
        key := aKey.
        value := aValue
    ]
    
    value [
        "Answer the association's value"
        ^value
    ]
    
    
    printOn: aStream [
        aStream
            nextPutAll: self key printString;
            nextPutAll: '->';
            nextPutAll: self value printString
    ]
    
    storeOn: aStream [
        aStream
            nextPut: $(;
            store: self class;
            nextPutAll: ' key: ';
            store: self key;
            nextPutAll: ' value: ';
            store: self value
    ]
]

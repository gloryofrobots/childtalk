 "
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"
Association subclass: VariableBinding [
    |dictionary|

   (+) key: aSymbol on: aDictionary [
        ^(self key: aSymbol value: 0)
        dictionary: aDictionary;
        yourself
    ]
    
    update [
        | index |
        value := dictionary indexOf: key.
        (dictionary basicAt: value) ifNil:
        [ self errorNotFound: 'Key not found: ', key ]
    ]
    
    dictionary: aDictionary [
        dictionary := aDictionary.
        self update
    ]
    
    dictionary [
        ^dictionary
    ]
]
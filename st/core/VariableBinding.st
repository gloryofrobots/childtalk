 VariableBinding extend [
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
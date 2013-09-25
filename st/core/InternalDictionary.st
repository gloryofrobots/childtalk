InternalDictionary extend [
    at: aSymbol put: anObject [
       <primitive: 'InternalDictionary_at_put'>
       self primitiveFailed.
    ]
    
    at: aSymbol [
       <primitive: 'InternalDictionary_at'>
       self primitiveFailed.
    ]
]
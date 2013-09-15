Array extends [
    
    
    new: size [
        <primitive: 'Array_newColon'>
    ]
    
    size [
        <primitive: 'Array_size'>
        self primitiveFailed
    ]

     at: anIndex [
        <primitive: 'Array_at'>
        anIndex printNl
        self primitiveFailed
    ]
    
    at: anIndex put: anObject [
        <primitive: 'Array_at_put'>
        self primitiveFailed
    ]

    replaceFrom: startIndex to: stopIndex with: aCollection startingAt: anIndex [
        <primitive: 'Array_replaceFromToWithStartingAt'>
        self primitiveFailed
    ]
]
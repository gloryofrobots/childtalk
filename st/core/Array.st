Array extend [
    new: size [
        <primitive: 'Array_newColon'>
    ]
    
    add: aObject [
        <primitive: 'Array_add'>
    ]
    
    replaceFrom: startIndex to: stopIndex with: aCollection startingAt: anIndex [
        <primitive: 'Array_replaceFromToWithStartingAt'>
        self primitiveFailed
    ]
]
Metaclass extend [
   printOn: aStream [
    aStream
        nextPutAll: instanceClass name asString;
        nextPutAll: ' class'
    ]
]
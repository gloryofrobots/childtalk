

Behavior subclass: Metaclass [
   printOn: aStream [
    aStream
        nextPutAll: instanceClass name asString;
        nextPutAll: ' class'
    ]
]






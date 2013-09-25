StdIOStream extend [

    upToEnd [
        | w |
        w := WriteStream on: String new.
        [ w nextPut: self next ] repeat.
        ^w contents
    ]
    
    nextPut: aCharacter [
        <primitive: 'StdIOStream_nextPut'>
        self primitiveFailed
    ]
    
    nextPutAll: aString [
        <primitive: 'StdIOStream_nextPutAll'>
        self primitiveFailed
    ]
    
    nextPutBuffer: aString [
    ]
]
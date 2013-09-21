FileStream extend [
  
    (+) new [
        self shouldNotImplement
    ]
        
   (+) on: aFD for: aMode [
        ^self basicNew initializeHandle: (self fileOp: 8 with: aFD with: aMode) for: aMode
    ]
    
   (+) open: aFilename [
        ^self open: aFilename for: self read
    ]
    
   (+) open: aFilename for: aMode [
        ^self basicNew initializeHandle: (self fileOp: 0 with: aFilename with: aMode) for: aMode
    ]
    
   (+) fileOp: anInteger with: anArgument with: anotherArgument [
        "Returns a FILE* handle"
        <primitive: 'FileStream_fileOp'>
    	self primitiveFailed
    ]
    
   (+) fileIn: aFilename [
        ^(self open: aFilename) fileIn; yourself
    ]
    
   (+) append [
        ^'a'
    ]
    
   (+) write [
        ^'w'
    ]
    
   (+) read [
        ^'r'
    ]
    
   (+) readWrite [
        ^'r+'
    ]
    
   (+) readWriteCreate [
        ^'w+'
    ]
    
    initialize [
        super initialize.
        collection := String new: 10.
        curPos := endPos := 0
    ]
    
    initializeHandle: aFilePointer for: aMode [
        self initialize.
        handle := aFilePointer.
        aMode
    	caseOf: {
    	    [ self class append ] -> [ self allowWriteOnly ].
    	    [ self class read ] -> [ self allowReadOnly ].
    	    [ self class write ] -> [ self allowWriteOnly ].
    	    [ self class readWrite ] -> [ self allowReadAndWrite ].
    	    [ self class readWriteCreate ] -> [ self allowReadAndWrite ] }
    ]
    
    
    contents [
        | pos ret |
        pos := curPos.
        self reset.
        ret := self upToEnd.
        self position: pos.
        ^ret
    ]
    
    upToEnd [
        ^self next: (self size - curPos)
    ]
    
    size [
        | primSize |
        primSize := self fileOp: 7 on: handle.
        ^primSize max: endPos
    ]
    
    atEnd [
        ^self size = curPos
    ]
    
    fileIn [
        | stream lexer parser method context peek |
        stream := ReadWriteStream with: String new.
        peek := self next.
        peek = $#
    	ifTrue: [ self skipTo: Character nl ]
    	ifFalse: [ stream nextPut: peek ].
        stream
    	nextPutAll: self upToEnd;
    	reset.
        lexer := CompilerLexer readFrom: stream.
        parser := CompilerParser on: lexer for: nil class.
        parser runMultipleAndDeclarations
    ]
    
    
    fileOp: anInteger on: fileHandle [
        <primitive: 'FileStream_fileOp'>
    	self primitiveFailed
    ]
    
    fileOp: anInteger on: fileHandle with: anArgument [
        <primitive: 'FileStream_fileOp'>
    	self primitiveFailed
    ]
    
    
    handle [
        ^handle
    ]
    
    nextPutBuffer: aByteArray [
        | pos |
        pos := curPos + 1.
        curPos := curPos + aByteArray size.
        super atEnd
    	ifTrue: [ (endPos := curPos) >= collection size
    		      ifTrue: [ collection growTo: endPos + 10 ] ].
        ^collection replaceFrom: pos to: curPos with: aByteArray
    ]
    
    
    fileno [
        ^self fileOp: 9 on: handle
    ]
    
    close [
        self fileOp: 1 on: handle.
        handle := nil
    ]
    
    nextPut: aCharacter [
        self ensureWriteable.
        self fileOp: 2 on: handle with: aCharacter.
        self nextPutBuffer: aCharacter asString
    ]
    
    nextPutAll: aByteArray [
        self ensureWriteable.
        self fileOp: 3 on: handle with: aByteArray.
        self nextPutBuffer: aByteArray
    ]
    
    flush [
        self fileOp: 4 on: handle
    ]
    
    next [
        | c |
        self ensureReadable.
        c := self fileOp: 5 on: handle.
        self nextPutBuffer: c asString.
        ^c
    ]
    
    next: numBytes [
        | s |
        self ensureReadable.
        s := self fileOp: 6 on: handle with: numBytes.
        self nextPutBuffer: s.
        ^s
    ]
    
    
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
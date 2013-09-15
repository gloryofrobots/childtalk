WriteStream extend [
   
    initialize [
        super initialize.
        self allowWriteOnly.
        self truncate
    ]
    
    
    initialize [
        super initialize.
        self allowReadOnly
    ]
    
    
    initialize [
        super initialize.
        self allowReadAndWrite
    ]
]
WriteStream extend [
   
    initialize [
        super initialize.
        self allowWriteOnly.
        self truncate
    ]
]

ReadStream extend [
    initialize [
        super initialize.
        self allowReadOnly
    ]
]

ReadWriteStream extend [
    initialize [
        super initialize.
        self allowReadAndWrite
    ]
]
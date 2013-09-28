"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

ArrayedCollection subclass: Array [
    new: size [
        <primitive: 'Array_newColon'>
        self primitiveFailed
    ]
    
    append: aObject [
        <primitive: 'Array_add'>
        self primitiveFailed
    ]
]
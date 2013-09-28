"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"


Behaviour subclass: Metaclass [
    |instanceClass|
    
   printOn: aStream [
    aStream
        nextPutAll: instanceClass name asString;
        nextPutAll: ' class'
    ]
]
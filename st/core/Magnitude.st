"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"

Object subclass: Magnitude [

    = aMagnitude [
        self subclassResponsibility
    ]
    
    < aMagnitude [
        self subclassResponsibility 
    ]
    
    > aMagnitude [
        ^aMagnitude < self
    ]
    
    <= aMagnitude [
        ^(aMagnitude < self) not
    ]
    
    >= aMagnitude [
        ^(self < aMagnitude) not
    ]
    
    max: aMagnitude [
        "Answer self if greater than aMagnitude, else return aMagnitude"
        ^self > aMagnitude
    	ifTrue: [ self ]
    	ifFalse: [ aMagnitude ]
    ]
    
    min: aMagnitude [
        "Answer self if lesser than aMagnitude, else return aMagnitude"
        ^self < aMagnitude
    	ifTrue: [ self ]
    	ifFalse: [ aMagnitude ]
    ]
    
    between: firstNumber and: secondNumber [
        "Answer whether the receiver is both greater than or equal to the first number
         and less than or equal to the second number"
        ^(self >= firstNumber) bitAnd: (self <= secondNumber)
    ]
]
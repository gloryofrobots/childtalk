Magnitude extend [
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
        ^(self >= firstNumber) & (self <= secondNumber)
    ]
]
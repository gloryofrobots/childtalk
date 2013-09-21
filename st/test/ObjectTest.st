"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."


TestCase subclass: ObjectTest [

     test [
        self testHasInitializeMethods.
        self testComparingMethods.
        self testAccessingMethods.
        self testClassMembershipMethods.
        self testPrinting.
    ]
    
    testHasInitializeMethods [
        self assertTrue: (Object respondsTo: #initialize) withMessage: 'should have initialize method.'.
        self assertTrue: (Object class respondsTo: #initialize) withMessage: 'should have initialize method.'.
    ]
     testComparingMethods [
        "Assert Object has the expected comparing methods."
        self assertTrue: (Object == Object) withMessage: 'should be equal (==).'.
        self assertTrue: (Object = Object) withMessage: 'should be equal (=).'.
        self assertTrue: (Object ~= String) withMessage: 'should not be equal (~=).'.
        self assertTrue: (Object ~~ String) withMessage: 'should not be equal (~~).'.
        self assertTrue: (Object hash ~= 0) withMessage: 'should not be 0.'.
    ]
    
     testAccessingMethods [
        "Assert Object has the expected accessing methods."
        self assertTrue: (Object yourself == Object) withMessage: 'should evaluate to self.'.
        self assertTrue: ('' class == Object) withMessage: '"" class == String'.
    ]
    
     testClassMembershipMethods [
        "Assert Object has the expected class membership methods."
        self assertTrue: (String isMemberOf: String class) withMessage: 'should be String.'.
        self assertFalse: (String isMemberOf: Object class) withMessage: 'should not be String.'.
        self assertTrue: (Object respondsTo: #isKindOf:) withMessage: 'should respond to #isKindOf:.'.
        self assertFalse: (Object respondsTo: #not) withMessage: 'should not respond to #not.'.
        self assertTrue: (String new isKindOf: Object) withMessage: 'should be kind of Object.'.
        self assertTrue: (String inheritsFrom: Object) withMessage: 'should inheritsFrom Object.'.
       
    ]
    
     testPrinting [
        self assertTrue: ( Object new printString = 'an Object') withMessage: 'should print "an Object"'.
    ]
]

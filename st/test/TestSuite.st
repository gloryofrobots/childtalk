"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: TestSuite [

    run [
        self show: '-------------------------------------------------------'.
        self show: '--- S m a l l t a l k  T e s t s'.
        self show: '-------------------------------------------------------'.
        
        (ObjectTest new:'Object') test.
        
        (TrueTest new:'True') test.
        
        (FalseTest new:'False') test.
        
        (UndefinedObjectTest new:'UndefinedObject') test.
        
        (CommonTest new:'CommonTest') test.
        "
        SimpleTest new test.
        "
    ]
    show: message [
        Transcript show: message; cr.
    ]
]

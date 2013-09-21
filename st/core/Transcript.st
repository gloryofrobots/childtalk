"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Transcript extend [
    (+) show: anObject [
        anObject echo.
    ]
    
    (+) cr [
        self show: '\n'
    ]
]
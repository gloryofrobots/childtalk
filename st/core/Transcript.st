"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Transcript extend [
    (+) show: anObject [
        <primitive: 'Transcript_show'>
    ]
    
    (+) initialize [
        lineSeparator = SmallTalk getProperty:'line.separator'.
    ]
    (+) showCr: anObject [
        self show: anObject; cr;
    ]

    (+) cr [
        self show: lineSeparator.
    ]
]
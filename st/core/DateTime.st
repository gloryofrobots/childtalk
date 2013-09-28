"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"
Magnitude subclass: DateTime [
    |offset second minute hour mday month year wday yday|
    
    (+) now [
        ^self new initializeGMTime
    ]
    
    primGMTime [
        <primitive: 'DateTime_gmTime'>
    	self primitiveFailed
    ]
    
    initializeGMTime [
        | gmTime |
        gmTime := self primGMTime.
        second := gmTime at: 1.
        minute := gmTime at: 2.
        hour := gmTime at: 3.
        mday := gmTime at: 4.
        month := gmTime at: 5.
        year := gmTime at: 6.
        wday := gmTime at: 7.
        yday := gmTime at: 8
    ]
    
    
    dayOfMonth [
        ^mday
    ]
    
    dayOfWeek [
        ^wday
    ]
    
    dayOfYear [
        ^yday
    ]
    
    hour [
        ^hour
    ]
    
    minute [
        ^minute
    ]
    
    second [
        ^second
    ]
    
    month [
        ^month
    ]
    
    year [
        ^year
    ]
    
    offset [
        ^offset
    ]
    
    offset: aDuration [
        offset := aDuration
    ]
]
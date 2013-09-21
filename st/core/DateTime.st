DateTime extend [
  
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
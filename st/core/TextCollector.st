TextCollector extend [
      
   (+) new [
        self shouldNotImplement
    ]
    
   (+) proxyMessage: anAssociation [
        "Specify an association of receiver -> selector, to be performed when writing to the TextCollector"
        ^self basicNew proxyMessage: anAssociation; yourself
    ]
    
    
    proxyMessage [
        "Answer the receiver -> selector association being used to write to the TextCollector"
        ^proxyReceiver -> proxySelector
    ]
    
    proxyMessage: anAssociation [
        "Set the association receiver -> selector, to be performed when writing to the TextCollector"
        proxyReceiver := anAssociation key.
        proxySelector := anAssociation value
    ]
    
    
    cr [
        "Write a new line to the receiver"
        super cr.
        self nl
    ]
    
    nextPut: aCharacter [
        "Write aCharacter to the receiver"
        proxyReceiver perform: proxySelector with: aCharacter asString
    ]
    
    nextPutAll: aString [
        "Write aString to the receiver"
        proxyReceiver perform: proxySelector with: aString
    ]
    
    show: aString [
        "Write aString to the receiver"
        self nextPutAll: aString
    ]
    
    showCr: aString [
        "Write aString to the receiver followed by a new line"
        self nextPutAll: aString; cr
    ]
]
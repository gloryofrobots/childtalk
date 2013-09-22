UndefinedObject extend [
        printOn: aStream [
            aStream nextPutAll: 'nil'
        ] 
        
        copy [
            ^ self.
        ]
        
        ifNil: aBlock [
            ^aBlock value
        ]
        
        ifNotNil: aBlock [
            ^self
        ]
        
        isNil [
            ^true
        ]
        
        notNil [
            ^false
        ] 
]
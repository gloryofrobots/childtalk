"
Childtalk is 
    Copyright (c) 2013 Alex Birulia

Childtalk standard library is based on the Smalltalk YX which is 
    Copyright (c) 2007-2008 Luca Bruno

Original license of Smalltalk YX may be found in the LICENSE file.
"


SequenceableCollection subclass: Interval [

    (+) new: count [
        self shouldNotImplement
    ]
    
    (+) from: start to: stop by: step [
        step = 0
    	ifTrue: [ ^self error: 'Zero step has been given' ].
        ^self basicNew initializeFrom: start to: stop by: step; yourself
    ]
    
    (+) from: start to: stop [
        ^self from: start to: stop by: 1
    ]
    
    
    initializeFrom: start to: stop by: step [
        start := start.
        stop := stop.
        step := step
    ]
    
    
    at: anIndex [
        anIndex > self size
    	ifTrue: [ ^self error: 'Invalid index' ].
        ^start + (anIndex - 1 * step)
    ]
    
    size [
        ^((start - stop) abs + 1 / step) ceiling
    ]
    
    
    start [
        ^start
    ]
    
    stop [
        ^stop
    ]
    
    step [
        ^step
    ]
    
    
    species [
        ^Array
    ]
    
    
    reverse [
        "Answer a reversed Interval of the receiver"
        ^Interval from: stop to: start by: step negated
    ]
    
    do: aBlock [
        "Do aBlock passing the interval elements as first argument of the given block"
        | i |
        i := start.
        step positive
    	ifTrue: [
    	    [ i <= stop ]
    		whileTrue: [ aBlock value: i.
    			     i := i + step ] ]
    	ifFalse: [
    	    [ i >= stop ]
    		whileTrue: [ aBlock value: i.
    			     i := i + step ] ]
    ]
    
    collect: transformerBlock [
        "Evaluate the block for each element of the receiver.
         The results of these evaluations are collected into a new collection"
        | coll |
        coll := self copyEmptyForCollect.
        self from: 1 to: self size keysAndValuesDo: [ :i :v |
    	coll at: i put: (transformerBlock value: v) ].
        ^coll
    ]
]
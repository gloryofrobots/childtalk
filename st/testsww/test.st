
 SOME := 2
 
 SOME <- SOME < 3 ifTrue: [3]
 
 Object subclass: Point [
  		 <instanceVariableNames: 'i1 i2 i3'>
  		 <classVariableNames: 'c1 c2 c3'>
  		 <poolDictionaries: 'P1 P2'>
	 	 <category: 'Collections Sequenceable'>
		 someX: x someY: y [
		   	
	 			<category:'creation'> 		
	 			<comment:'sssdfsdf'>
		  		 |temp1 temp2|
		  			temp1 := 3.
		  			temp2 := 6.
		   		 x < y
		   			 ifTrue: [ ^ x + 1 * 6 ] 
		   		     ifFalse:[do: [:element| element at:1 destroy]]
		  	 ]
	  	 ]
	  ]
	  
	  Point methods: [
	  fly: where with: who [
	  	<primitive: #Point_Fly_With>
	  	self primitiveFailed
	  ]
	 ]
   ]
  
  
  
  
  
   ArrayedCollection subclass: Array [
    category: 'Collections-Sequenceable'
    comment: ''
	instanceVariables: 's1 s2'
	classVariables : 's2 s2'

    at: anIndex ifAbsent: aBlock [
	"Answer the index-th indexed instance variable of the receiver"

	<category: 'built ins'>
	<primitive: VMpr_Object_basicAt>
	^self checkIndexableBounds: anIndex ifAbsent: aBlock
    ]

    printOn: aStream [
	"Print a representation for the receiver on aStream"

	<category: 'printing'>
	aStream nextPut: $(.
	self do: 
		[:elt | 
		elt printOn: aStream.
		aStream space].
	aStream nextPut: $)
    ]

    isLiteralObject [
	"Answer whether the receiver is expressible as a Smalltalk literal."

	<category: 'printing'>
	^self isReadOnly not
    ]

    isArray [
	<category: 'testing'>
	^true
    ]
]


 * 
 * */
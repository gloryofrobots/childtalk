Eval [

    |a|
    
    a:=2.
    
    "(String new: 5) at:0 put:$e"
    
    "Array name first isVowel"
    
    "#($a $e $i $o $u) includes: $r"
    "[a <= 3] whileTrue: [a echo. a:=a+1.]."
    
    "[:i|   [ i <= 3 ]
            whileTrue: [ i echo.
                     i := i + 1 ] ] value: 4."
    
    "Object subclasses from:1 to:3 do: [:el| el echo.]."
    
   " Object allSubclassesDo:[:ea| ea echo.]."
     
    TestSuite new run.
    "[:something| (something isSmallInteger) ifTrue: [ ^ ea + 2]] value:2. "
]
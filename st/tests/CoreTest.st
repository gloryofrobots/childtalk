
TestCase subclass: CoreTest [
    doTest [
        self testSemantic.
      
    ]
    testSemantic [
        self assertTrue: (Array name first isVowel) withMessage: 'Array name first isVowel should be true.'.
        
    ]
    
]
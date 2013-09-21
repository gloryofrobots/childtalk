Eval [

    nil subclass: #Object
        instanceVariableNames: ''
        classVariableNames: ''.
    
    Object subclass: #Behaviour
           instanceVariableNames: 'className classSuperclass instanceVariableNames instanceSize methodDictionary classSubclasses finalizationRequest'
           classVariableNames: ''.
    
    Object subclass: #UndefinedObject
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Object subclass: #Test
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Behaviour subclass: #Class
         instanceVariableNames: 'classVariables'
             classVariableNames: ''.
    
    Behaviour subclass: #Metaclass
         instanceVariableNames: 'instanceClass'
             classVariableNames: ''.
    
    Object subclass: #Transcript
           instanceVariableNames: ''
           classVariableNames: ''.
    
    "Collections"
    
    Object subclass: #Collection
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Collection subclass: #Set
           instanceVariableNames: 'tally'
           classVariableNames: ''.
    
    Collection subclass: #SequenceableCollection
           instanceVariableNames: ''
               classVariableNames: ''.
    
    SequenceableCollection subclass: #Interval
           instanceVariableNames: 'start stop step'
           classVariableNames: ''.
    
    SequenceableCollection subclass: #ArrayedCollection
                   instanceVariableNames: ''
                           classVariableNames: ''.
    
    ArrayedCollection subclass: #Array
              instanceVariableNames: ''
                      classVariableNames: ''.
    
    ArrayedCollection subclass: #ByteArray
              instanceVariableNames: ''
                      classVariableNames: ''.
    
    SequenceableCollection subclass: #OrderedCollection
                   instanceVariableNames: 'firstIndex lastIndex'
                           classVariableNames: ''.
    
    Object subclass: #Stack
           instanceVariableNames: 'collection top'
           classVariableNames: ''.
    
    Set subclass: #Bag
        instanceVariableNames: ''
        classVariableNames: ''.
    
    "Execution"
    
    Object subclass: #Context
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Object subclass: #Block
           instanceVariableNames: 'block outerFrame'
           classVariableNames: ''.
           
    Object subclass: #Method
           instanceVariableNames: ''
           classVariableNames: ''.
    
    
    "Streams"
    
    Object subclass: #Stream
           instanceVariableNames: 'accessMode'
           classVariableNames: ''.
    
    Stream subclass: #PositionableStream
           instanceVariableNames: 'collection curPos endPos'
           classVariableNames: ''.
    
    Stream subclass: #Random
           instanceVariableNames: 'seed'
           classVariableNames: ''.
    
    PositionableStream subclass: #ReadStream
               instanceVariableNames: ''
               classVariableNames: ''.
    
    PositionableStream subclass: #WriteStream
               instanceVariableNames: ''
                       classVariableNames: ''.
    
    WriteStream subclass: #ReadWriteStream
            instanceVariableNames: ''
                classVariableNames: ''.
    
    ReadWriteStream subclass: #ByteStream
            instanceVariableNames: ''
                    classVariableNames: ''.
    
    ByteStream subclass: #FileStream
           instanceVariableNames: 'handle name filePtr peek'
               classVariableNames: ''.
    
    FileStream subclass: #StdIOStream
           instanceVariableNames: ''
           classVariableNames: ''.
    
    "Basic data types"
    
    ByteArray subclass: #String
          instanceVariableNames: ''
              classVariableNames: ''.
    
    String subclass: #Symbol
           instanceVariableNames: 'hash'
           classVariableNames: ''.
    
    Object subclass: #Magnitude
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Magnitude subclass: #Character
          instanceVariableNames: ''
              classVariableNames: 'WhiteSpaceTable XDigitsTable'.
    
    Magnitude subclass: #Number
          instanceVariableNames: ''
              classVariableNames: ''.
    
    Magnitude subclass: #Date
              instanceVariableNames: 'days day month year'
              classVariableNames: 'DayNames MonthNames'.
    
    Magnitude subclass: #DateTime
          instanceVariableNames: 'offset second minute hour mday month year wday yday'
          classVariableNames: ''.
    
    Magnitude subclass: #Duration
          instanceVariableNames: 'seconds'
          classVariableNames: ''.
    
    Number subclass: #Integer
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Integer subclass: #SmallInteger
        instanceVariableNames: ''
        classVariableNames: ''.
    
    Integer subclass: #LargeInteger
        instanceVariableNames: ''
        classVariableNames: ''.
    
    Number subclass: #Fraction
           instanceVariableNames: 'numerator denominator'
           classVariableNames: ''.
    
    Number subclass: #Float
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Object subclass: #Boolean
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Boolean subclass: #True
        instanceVariableNames: ''
            classVariableNames: ''.
    
    Boolean subclass: #False
        instanceVariableNames: ''
            classVariableNames: ''.
    
    "Dictionaries"
    
    Object subclass: #Association
           instanceVariableNames: 'key value'
           classVariableNames: ''.
    
    Association subclass: #VariableBinding
            instanceVariableNames: 'dictionary'
            classVariableNames: ''.
    
    Collection subclass: #Dictionary
           instanceVariableNames: 'tally'
           classVariableNames: ''.
    
    Dictionary subclass: #IdentityDictionary
           instanceVariableNames: ''
           classVariableNames: ''.
    
    IdentityDictionary subclass: #SystemDictionary
               instanceVariableNames: 'systemInitializationRequests commandLine startupProcess startupObject'
               classVariableNames: ''.
    
    "Exceptions"
    
    Object subclass: #Signal
           instanceVariableNames: 'signalerContext messageText'
           classVariableNames: ''.
           
    "All other exceptions defined here"
    Signal subclass: #Exception
           instanceVariableNames: 'handlerContext'
           classVariableNames: ''.
]

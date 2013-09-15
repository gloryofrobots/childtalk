Eval [
    nil subclass: #Object
        instanceVariableNames: ''
        classVariableNames: ''.
    
    Object subclass: #Behaviour
           instanceVariableNames: 'name superclass instanceVariableNames instanceSize methodDictionary subclasses finalizationRequest'
           classVariableNames: ''.
    
    Object subclass: #UndefinedObject
           instanceVariableNames: ''
           classVariableNames: ''.
    
    Behavior subclass: #Class
         instanceVariableNames: 'classVariables'
             classVariableNames: ''.
    
    Behavior subclass: #Metaclass
         instanceVariableNames: 'instanceClass'
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
    
    Object subclass: #ContextPart
           instanceVariableNames: 'framePointer stack parent method arguments'
           classVariableNames: ''.
    
    ContextPart subclass: #MethodContext
            instanceVariableNames: 'receiver'
                classVariableNames: ''.
    
    ContextPart subclass: #BlockContext
            instanceVariableNames: 'closure handledException handlerBlock ensureBlock'
                classVariableNames: ''.
    
    Object subclass: #Process
           instanceVariableNames: 'stack framePointer suspended returnedObject next scheduled'
           classVariableNames: ''.
    
    Object subclass: #Semaphore
           instanceVariableNames: 'waitingProcesses signals'
           classVariableNames: ''.
    
    Object subclass: #CompiledCode
           instanceVariableNames: 'bytecodes argumentCount temporaryCount literals stackSize text class'
           classVariableNames: ''.
    
    CompiledCode subclass: #CompiledMethod
             instanceVariableNames: 'selector primitive'
                 classVariableNames: ''.
    
    CompiledCode subclass: #CompiledBlock
             instanceVariableNames: ''
             classVariableNames: ''.
    
    Object subclass: #BlockClosure
           instanceVariableNames: 'block outerFrame'
           classVariableNames: ''.
    
    
    "Management"
    
    Object subclass: #ProcessorScheduler
           instanceVariableNames: 'processByteslice activeProcess'
           classVariableNames: ''.
    
    Object subclass: #ObjectMemory
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
    
    Magnitude subclass: #CPointer
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
    
    
    
    "WinWorkspace"
    
    Object subclass: #WinWorkspace
           instanceVariableNames: ''
           classVariableNames: ''.
    
    "Exceptions"
    
    Object subclass: #Signal
           instanceVariableNames: 'signalerContext messageText'
           classVariableNames: ''.
    
    Signal subclass: #Exception
           instanceVariableNames: 'handlerContext'
           classVariableNames: ''.
    
    Exception subclass: #Error
          instanceVariableNames: ''
              classVariableNames: ''.
    
    Error subclass: #NotImplemented
          instanceVariableNames: ''
          classVariableNames: ''.
    
    Error subclass: #ZeroDivide
          instanceVariableNames: ''
          classVariableNames: ''.
    
    Error subclass: #PrimitiveFailed
        instanceVariableNames: ''
            classVariableNames: ''.
    
    PrimitiveFailed subclass: #WrongArgumentCount
            instanceVariableNames: ''
            classVariableNames: ''.
    
    Error subclass: #MessageNotUnderstood
         instanceVariableNames: 'message receiver'
         classVariableNames: ''.
    
    NotImplemented subclass: #NotYetImplemented
               instanceVariableNames: ''
                   classVariableNames: ''.
    
    NotImplemented subclass: #ShouldNotImplement
               instanceVariableNames: ''
                   classVariableNames: ''.
    
    ShouldNotImplement subclass: #SubclassResponsibility
               instanceVariableNames: ''
                       classVariableNames: ''.
    
    Error subclass: #InvalidValue
          instanceVariableNames: ''
          classVariableNames: ''.
    
    InvalidValue subclass: #InvalidArgument
             instanceVariableNames: ''
                 classVariableNames: ''.
    
    InvalidArgument subclass: #NotFound
            instanceVariableNames: ''
                    classVariableNames: ''.
    
    InvalidArgument subclass: #NotKeyed
                    instanceVariableNames: ''
                    classVariableNames: ''.
]

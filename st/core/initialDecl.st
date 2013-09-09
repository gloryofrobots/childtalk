

"Collections"

Object subclass: Collection [
       <instanceVariableNames: ''>
       <classVariableNames: ''>
]

Collection subclass: Set [
	   <instanceVariableNames: 'tally'
	   <classVariableNames: ''>
]

Collection subclass: SequenceableCollection [
	   <instanceVariableNames: ''>
           <classVariableNames: ''>
]

SequenceableCollection subclass: Interval [
	   <instanceVariableNames: 'start stop step'>
	   <classVariableNames: ''>
]

SequenceableCollection subclass: ArrayedCollection [
		       <instanceVariableNames: ''>
                       <classVariableNames: ''>
]

ArrayedCollection subclass: Array [
		  <instanceVariableNames: ''>
                  <classVariableNames: ''>
]

ArrayedCollection subclass: ByteArray [
		  <instanceVariableNames: ''>
                  <classVariableNames: ''>
]

SequenceableCollection subclass: OrderedCollection [
		       <instanceVariableNames: 'firstIndex lastIndex'
                       <classVariableNames: ''>
]

Object subclass: Stack [
       <instanceVariableNames: 'collection top'>
       <classVariableNames: ''>
]

Set subclass: Bag [
    <instanceVariableNames: ''>
    <classVariableNames: ''>
]


Object subclass: ObjectMemory
       <instanceVariableNames: ''>
       <classVariableNames: ''>

"Streams"

Object subclass: Stream
       <instanceVariableNames: 'accessMode'>
       <classVariableNames: ''>

Stream subclass: PositionableStream
       <instanceVariableNames: 'collection curPos endPos'>
       <classVariableNames: ''>

Stream subclass: Random
       <instanceVariableNames: 'seed'>
       <classVariableNames: ''>

PositionableStream subclass: ReadStream
		   <instanceVariableNames: ''>
		   <classVariableNames: ''>

PositionableStream subclass: WriteStream
		   <instanceVariableNames: ''>
                   <classVariableNames: ''>

WriteStream subclass: ReadWriteStream
	    <instanceVariableNames: ''>
            <classVariableNames: ''>

ReadWriteStream subclass: ByteStream
		<instanceVariableNames: ''>
                <classVariableNames: ''>

ByteStream subclass: FileStream
	   <instanceVariableNames: 'handle name filePtr peek'>
           <classVariableNames: ''>

FileStream subclass: StdIOStream
	   <instanceVariableNames: ''>
	   <classVariableNames: ''>

"Basic data types"



"Console"

Object subclass: CommandLine
       <instanceVariableNames: 'commandLineArguments arguments options'>
       <classVariableNames: ''>

Object subclass: Console
       <instanceVariableNames: 'haveReadline'>
       <classVariableNames: ''>

"Exceptions"

Object subclass: Signal
       <instanceVariableNames: 'signalerContext messageText'>
       <classVariableNames: ''>

Signal subclass: Exception
       <instanceVariableNames: 'handlerContext'>
       <classVariableNames: ''>

Exception subclass: Error
	  <instanceVariableNames: ''>
          <classVariableNames: ''>

Error subclass: NotImplemented
      <instanceVariableNames: ''>
      <classVariableNames: ''>

Error subclass: VMError
      <instanceVariableNames: ''>
      <classVariableNames: ''>

Error subclass: Halt
      <instanceVariableNames: ''>
      <classVariableNames: ''>

Error subclass: ZeroDivide
      <instanceVariableNames: ''>
      <classVariableNames: ''>

VMError subclass: PrimitiveFailed
	<instanceVariableNames: ''>
        <classVariableNames: ''>

Signal subclass: SystemSignal
       <instanceVariableNames: 'handlerBlock'
       <classVariableNames: ''>

PrimitiveFailed subclass: WrongArgumentCount
		<instanceVariableNames: ''>
		<classVariableNames: ''>

Halt subclass: MessageNotUnderstood
     <instanceVariableNames: 'message receiver'
     <classVariableNames: ''>

NotImplemented subclass: NotYetImplemented
	       <instanceVariableNames: ''>
               <classVariableNames: ''>

NotImplemented subclass: ShouldNotImplement
	       <instanceVariableNames: ''>
               <classVariableNames: ''>

ShouldNotImplement subclass: SubclassResponsibility
		   <instanceVariableNames: ''>
            <classVariableNames: ''>

Error subclass: InvalidValue
      <instanceVariableNames: ''>
      <classVariableNames: ''>

InvalidValue subclass: InvalidArgument
	     <instanceVariableNames: ''>
             <classVariableNames: ''>

InvalidArgument subclass: NotFound
		<instanceVariableNames: ''>
                <classVariableNames: ''>

InvalidArgument subclass: NotKeyed
                <instanceVariableNames: ''>
                <classVariableNames: ''>

SystemSignal subclass: UserInterrupt
	     <instanceVariableNames: ''>
	     <classVariableNames: 'HandlerBlock'>

SystemSignal subclass: TerminationSignal
	     <instanceVariableNames: ''>
	     <classVariableNames: 'HandlerBlock'>

SystemSignal subclass: AbnormalTermination
	     <instanceVariableNames: ''>
	     <classVariableNames: ''>

SystemSignal subclass: FloatingPointException
	     <instanceVariableNames: ''>
	     <classVariableNames: ''>

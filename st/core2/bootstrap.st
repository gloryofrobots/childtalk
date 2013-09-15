







Object subclass: Magnitude [
       <instanceVariableNames: ''>
       <classVariableNames: ''>
]


Magnitude subclass: Character [
      <instanceVariableNames: ''>
          <classVariableNames: 'WhiteSpaceTable XDigitsTable'>
]

Magnitude subclass: Number [
      <instanceVariableNames: ''>
          <classVariableNames: ''>
]

Magnitude subclass: Date [
          <instanceVariableNames: 'days day month year'>
          <classVariableNames: 'DayNames MonthNames'>
]

Magnitude subclass: DateTime [
      <instanceVariableNames: 'offset second minute hour mday month year wday yday'>
      <classVariableNames: ''>
]

Magnitude subclass: Duration [
      <instanceVariableNames: 'seconds'>
      <classVariableNames: ''>
]

Number subclass: Integer [
       <instanceVariableNames: ''>
       <classVariableNames: ''>
[

Integer subclass: SmallInteger [
    <instanceVariableNames: ''>
    <classVariableNames: ''>
]

Integer subclass: LargeInteger [
    <instanceVariableNames: ''>
    <classVariableNames: ''>
]

Number subclass: Fraction [
       instanceVarableNames: 'numerator denominator'
       <classVariableNames: ''>
]

Number subclass: Float [
       <instanceVariableNames: ''>
       <classVariableNames: ''>
]

Object subclass: Boolean [
       <instanceVariableNames: ''>
       <classVariableNames: ''>
]
Boolean subclass: True [
    <instanceVariableNames: ''>
        <classVariableNames: ''>
]
Boolean subclass: False [
    <instanceVariableNames: ''>
        <classVariableNames: ''>
]

"Dictionaries"

Object subclass: Association [
       <instanceVariableNames: 'key value'>
       <classVariableNames: ''>
]

Association subclass: VariableBinding [
        <instanceVariableNames: 'dictionary'>
        <classVariableNames: ''>
]

Collection subclass: Dictionary [
       <instanceVariableNames: 'tally'>
       <classVariableNames: ''>
]
Dictionary subclass: IdentityDictionary [
       <instanceVariableNames: ''>
       <classVariableNames: ''>
]

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
               <instanceVariableNames: 'firstIndex lastIndex'>
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


Object subclass: ObjectMemory [
       <instanceVariableNames: ''>
       <classVariableNames: ''>
]

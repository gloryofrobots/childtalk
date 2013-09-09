
nil subclass: Object [
    <instanceVariableNames: ''>
    <classVariableNames: ''>
]

Object subclass: Behavior [
       <instanceVariableNames: 'name superclass instanceVariableNames instanceSize methodDictionary subclasses finalizationRequest'
       <classVariableNames: ''>
]

Object subclass: UndefinedObject [
       <instanceVariableNames: ''>
       <classVariableNames: ''>
]

Behavior subclass: Class [
     <instanceVariableNames: 'classVariables'
         <classVariableNames: ''>
]

Behavior subclass: Metaclass [
     <instanceVariableNames: 'instanceClass'
         <classVariableNames: ''>
]


ByteArray subclass: String [
      <instanceVariableNames: ''>
          <classVariableNames: ''>
]

String subclass: Symbol [
       <instanceVariableNames: 'hash'>
       <classVariableNames: ''>
]

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



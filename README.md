childtalk
==========

Simple Smalltalk dialect written on Java for self-educational reasons.

Project is not developing now.

Childtalk was my first completed interpreter. I tried to write it in clear and good style, but, as always, bugs and fixes do their job well.

Childtalk is based on  <a href = 'http://code.google.com/p/syx/'>Smalltalk YX</a>. Actually parser and lexer are Java versions of Syx parser and lexer, but I added AST generation between parser and compiler, and built compiler around visitor pattern (it has very good explanation in Andrew Appel books). Also all runtime modules are totally different from Syx and even different from almost all smalltalk systems I studied. At first I wrote them like lisp system, but than I understood simple rule: your runtime structure must be very close to your language. So I have already changed a lot of code. Than I decided to rewrite everything everywhere. And at last I decided to leave Childtalk as this. However it was a funny month when I wrote Childtalk code and slept poorly.



<h4>FEATURES</h4>
<ul>
<li> Stackless. All methods and blocks run in serial queue.</li>
<li> Easy declarative syntax based on GNU Smalltalk </li>
<li> Many standart smalltalk features supported. You can read tests code and see what childtalk can do.</li>
<li> Working set of standart objects : SmallInteger, LargeInteger, Float, String, Symbol, Collection, Dictionary etc...
<li> Simple code. Not ideal, and I am ashamed of some things. However may be it helps you to create your own</li>
</ul>

<h4>PROBLEMS AND FAILS</h4>
<ul>
<li> It`s slow. Very slow. I decided to completely ignore perfomance factor. So, sorry fo that. </li>
<li> Only fake green thread processes avalaible.</li>
<li> No filestream and stdiostream support. It`s not very hard to add it, of course. </li>
<li> Image creation based on Java serialization. I simply serialize image instances without optimisations.</li>
<li> Static initializers "initialize" for classes not called automatically. </li>
</ul>

<h4>Syntax example</h4>

```
Robot subclass: Destroyer [
    "I am comment"
    
    "Static variables"
    \controlCenter\

    "Instance variables"
    |rocketLauncher machineGun|
    
    "Static method"
   (+) new [
        ^ self new intialize.
    ]

    "Instance methods"
    initialize [
    	rocketLauncher := RocketLauncher new.
        machineGun := MachineGun new.
    ]

    destroy: aTarget [
    	"Temporary variable"
    	|counter|
        counter := 1000000000000000.
        1 to: counter do: [:i| 
                            rocketLauncher fireOn: aTarget withNumberOfRockets: i.
                            machineGun fireOn: aTarget].
    ]
]

"Extending of existed class"
Destroyer extend [
      version [
          ^ 'Destroyer 2.35'.
      ]
]

"Eval block"
Eval [
    "Eval has temporaries too. "
     |target|
     target := Human new.
     Destroyer new destroy: target.
]
```

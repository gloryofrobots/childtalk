"
Copyright (c) 2007-2008 Luca Bruno

This file is part of Smalltalk YX.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the 'Software'), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
"

!SystemSignal methodsFor: 'signaling'!

description
    ^'The application received a signal from the system.'
! !

!SystemSignal methodsFor: 'testing'!

isHandled
    ^false
! !

!SystemSignal methodsFor: 'private'!

notHandled
    Transcript showCr: self messageText.
    Smalltalk quit
! !

!UserInterrupt class methodsFor: 'signaling'!

signal
    ^self signal: 'User interrupt (SIGINT)'
! !

!UserInterrupt class methodsFor: 'initialize-release'!

initialize
    Smalltalk registerForSystemInitialization: self
!

initializeSystem
    HandlerBlock := nil
! !

!UserInterrupt class methodsFor: 'handling'!

handlerBlock: aBlock
    HandlerBlock := aBlock
!

handlerBlock
    ^HandlerBlock
! !

!UserInterrupt methodsFor: 'private'!

handle
    ^HandlerBlock value: self
! !

!UserInterrupt methodsFor: 'testing'!

isHandled
    ^HandlerBlock notNil
! !

!FloatingPointException class methodsFor: 'signaling'!

signal
    ^self signal: 'Floating point exception (SIGFPE)'
! !

!TerminationSignal class methodsFor: 'signaling'!

signal
    ^self signal: 'Termination signal (SIGTERM)'
! !

!TerminationSignal class methodsFor: 'initialize-release'!

initialize
    Smalltalk registerForSystemInitialization: self
!

initializeSystem
    HandlerBlock := nil
! !

!TerminationSignal class methodsFor: 'handling'!

handlerBlock: aBlock
    HandlerBlock := aBlock
!

handlerBlock
    ^HandlerBlock
! !

!TerminationSignal methodsFor: 'private'!

handle
    ^HandlerBlock value: self
! !

!TerminationSignal methodsFor: 'testing'!

isHandled
    ^HandlerBlock notNil
! !

!AbnormalTermination class methodsFor: 'signaling'!

signal
    ^self signal: 'Abnormal termination (SIGABRT)'
! !
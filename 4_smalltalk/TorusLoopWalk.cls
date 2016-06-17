"Filed out from Dolphin Smalltalk 7"!

TorusPointWalk subclass: #TorusLoopWalk
	instanceVariableNames: 'direction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusLoopWalk guid: (GUID fromString: '{398912EA-04CD-45EF-B1F7-9E2FD3290D09}')!
TorusLoopWalk comment: ''!
!TorusLoopWalk categoriesForClass!Kernel-Objects! !
!TorusLoopWalk methodsFor!

do: operation
	| p |
	p := startPoint.
	
	[operation value: p.
	p := p + direction.
	p ~~ startPoint] whileTrue!

initWithDirection: dir
	direction := dir! !
!TorusLoopWalk categoriesFor: #do:!public! !
!TorusLoopWalk categoriesFor: #initWithDirection:!private! !

!TorusLoopWalk class methodsFor!

newWithPoint: p direction: dir
	| r |
	r := super newWithPoint: p.
	r initWithDirection: dir.
	^r! !
!TorusLoopWalk class categoriesFor: #newWithPoint:direction:!public! !


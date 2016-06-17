"Filed out from Dolphin Smalltalk 7"!

TorusPointWalk subclass: #TorusIterWalk
	instanceVariableNames: 'length direction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusIterWalk guid: (GUID fromString: '{BD53E688-1C69-4121-99A5-F579D184A773}')!
TorusIterWalk comment: ''!
!TorusIterWalk categoriesForClass!Kernel-Objects! !
!TorusIterWalk methodsFor!

do: operation
	| p |
	p := startPoint.
	length timesRepeat: 
			[operation value: p.
			p := p + direction]!

initWithLength: n direction: dir
	length := n.
	direction := dir! !
!TorusIterWalk categoriesFor: #do:!public! !
!TorusIterWalk categoriesFor: #initWithLength:direction:!private! !

!TorusIterWalk class methodsFor!

newWithPoint: p length: n direction: dir
	| r |
	r := super newWithPoint: p.
	r initWithLength: n direction: dir.
	^r! !
!TorusIterWalk class categoriesFor: #newWithPoint:length:direction:!public! !


"Filed out from Dolphin Smalltalk 7"!

TorusWalk subclass: #TorusPointWalk
	instanceVariableNames: 'startPoint'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusPointWalk guid: (GUID fromString: '{7031322C-68D3-49A2-85E8-09FB7E5AC1EE}')!
TorusPointWalk comment: ''!
!TorusPointWalk categoriesForClass!Kernel-Objects! !
!TorusPointWalk methodsFor!

initWithPoint: p
	startPoint := p! !
!TorusPointWalk categoriesFor: #initWithPoint:!private! !

!TorusPointWalk class methodsFor!

newWithPoint: p
	| r |
	r := self basicNew.
	r initWithPoint: p.
	^r! !
!TorusPointWalk class categoriesFor: #newWithPoint:!public! !


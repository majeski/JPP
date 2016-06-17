"Filed out from Dolphin Smalltalk 7"!

TorusPointWalk subclass: #TorusBlockWalk
	instanceVariableNames: 'stepOperation'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusBlockWalk guid: (GUID fromString: '{5B38BAB0-79CF-44D1-B3AB-69CCDD9C4513}')!
TorusBlockWalk comment: ''!
!TorusBlockWalk categoriesForClass!Unclassified! !
!TorusBlockWalk methodsFor!

do: operation
	operation value: startPoint.
	(stepOperation value: startPoint) ifNotNil: [:w | w do: operation]!

initWithStep: operation
	stepOperation := operation! !
!TorusBlockWalk categoriesFor: #do:!public! !
!TorusBlockWalk categoriesFor: #initWithStep:!private! !

!TorusBlockWalk class methodsFor!

newWithPoint: p step: operation
	| r |
	r := super newWithPoint: p.
	r initWithStep: operation.
	^r! !
!TorusBlockWalk class categoriesFor: #newWithPoint:step:!public! !


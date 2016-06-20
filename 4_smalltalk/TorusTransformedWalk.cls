"Filed out from Dolphin Smalltalk 7"!

TorusWalk subclass: #TorusTransformedWalk
	instanceVariableNames: 'walk transformation arg'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusTransformedWalk guid: (GUID fromString: '{2AD51600-6A9F-41CA-8E89-701861B94B91}')!
TorusTransformedWalk comment: ''!
!TorusTransformedWalk categoriesForClass!Kernel-Objects! !
!TorusTransformedWalk methodsFor!

do: operation
	walk do: [:p | (transformation value: p) do: operation]!

init: w withTransformation: t
	walk := w.
	transformation := t! !
!TorusTransformedWalk categoriesFor: #do:!public! !
!TorusTransformedWalk categoriesFor: #init:withTransformation:!private! !

!TorusTransformedWalk class methodsFor!

new: w withTransformation: t
	| r |
	r := self basicNew.
	r init: w withTransformation: t.
	^r! !
!TorusTransformedWalk class categoriesFor: #new:withTransformation:!public! !


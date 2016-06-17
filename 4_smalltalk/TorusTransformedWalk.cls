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
	walk do: [:p | (p perform: transformation with: arg) do: operation]!

init: w withTransformation: t parameter: p
	walk := w.
	transformation := t.
	arg := p! !
!TorusTransformedWalk categoriesFor: #do:!public! !
!TorusTransformedWalk categoriesFor: #init:withTransformation:parameter:!private! !

!TorusTransformedWalk class methodsFor!

new: w withTransformation: s parameter: p
	| r |
	r := self basicNew.
	r
		init: w
		withTransformation: s
		parameter: p.
	^r! !
!TorusTransformedWalk class categoriesFor: #new:withTransformation:parameter:!public! !


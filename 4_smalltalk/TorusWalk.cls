"Filed out from Dolphin Smalltalk 7"!

Collection subclass: #TorusWalk
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusWalk guid: (GUID fromString: '{8D5ECAC1-678B-47A7-955F-E85AC7150E00}')!
TorusWalk comment: ''!
!TorusWalk categoriesForClass!Kernel-Objects! !
!TorusWalk methodsFor!

% x
	^TorusTransformedWalk new: self withTransformation: [:p | p % x]!

, walk2
	| walks |
	walks := OrderedCollection new.
	walks
		addLast: self;
		addLast: walk2.
	^TorusConcatenatedWalk newWith: walks!

| x
	^TorusTransformedWalk new: self withTransformation: [:p | p | x]!

add: _
	^self shouldNotImplement!

do: operation
	^self subclassResponsibility!

first: anInteger
	| answer i |
	answer := OrderedCollection new.
	anInteger > 0 ifFalse: [^answer].
	i := anInteger.
	self do: 
			[:each |
			answer add: each.
			i := i - 1.
			i = 0 ifTrue: [^answer]].
	^answer!

remove: _1 ifAbsent: _2
	^self shouldNotImplement!

species
	^OrderedCollection! !
!TorusWalk categoriesFor: #%!public! !
!TorusWalk categoriesFor: #,!public! !
!TorusWalk categoriesFor: #|!public! !
!TorusWalk categoriesFor: #add:!public! !
!TorusWalk categoriesFor: #do:!public! !
!TorusWalk categoriesFor: #first:!public! !
!TorusWalk categoriesFor: #remove:ifAbsent:!public! !
!TorusWalk categoriesFor: #species!public! !

!TorusWalk class methodsFor!

new
	^self shouldNotImplement.!

new: _
	^self shouldNotImplement.! !
!TorusWalk class categoriesFor: #new!public! !
!TorusWalk class categoriesFor: #new:!public! !


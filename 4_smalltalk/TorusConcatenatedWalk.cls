"Filed out from Dolphin Smalltalk 7"!

TorusWalk subclass: #TorusConcatenatedWalk
	instanceVariableNames: 'walk1 walk2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusConcatenatedWalk guid: (GUID fromString: '{0AC3BEF0-F048-4FBB-920B-8AA14A720D4B}')!
TorusConcatenatedWalk comment: ''!
!TorusConcatenatedWalk categoriesForClass!Kernel-Objects! !
!TorusConcatenatedWalk methodsFor!

do: operation
	walk1 do: operation.
	walk2 do: operation!

initWith: w1 and: w2
	walk1 := w1.
	walk2 := w2! !
!TorusConcatenatedWalk categoriesFor: #do:!public! !
!TorusConcatenatedWalk categoriesFor: #initWith:and:!private! !

!TorusConcatenatedWalk class methodsFor!

newWith: w1 and: w2
	| r |
	r := self basicNew.
	r initWith: w1 and: w2.
	^r! !
!TorusConcatenatedWalk class categoriesFor: #newWith:and:!public! !


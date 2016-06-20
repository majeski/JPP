"Filed out from Dolphin Smalltalk 7"!

TorusWalk subclass: #TorusConcatenatedWalk
	instanceVariableNames: 'walks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusConcatenatedWalk guid: (GUID fromString: '{0AC3BEF0-F048-4FBB-920B-8AA14A720D4B}')!
TorusConcatenatedWalk comment: ''!
!TorusConcatenatedWalk categoriesForClass!Kernel-Objects! !
!TorusConcatenatedWalk methodsFor!

, w
	| newWalks |
	newWalks := OrderedCollection new.
	newWalks
		addAllLast: walks;
		addLast: w.
	^TorusConcatenatedWalk newWith: newWalks!

do: operation
	walks do: [:w | w do: operation]!

initWith: ws
	walks := ws! !
!TorusConcatenatedWalk categoriesFor: #,!public! !
!TorusConcatenatedWalk categoriesFor: #do:!public! !
!TorusConcatenatedWalk categoriesFor: #initWith:!private! !

!TorusConcatenatedWalk class methodsFor!

newWith: ws
	| r |
	r := self basicNew.
	r initWith: ws.
	^r! !
!TorusConcatenatedWalk class categoriesFor: #newWith:!public! !


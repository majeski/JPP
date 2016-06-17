"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Torus
	instanceVariableNames: 'shape points'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Torus guid: (GUID fromString: '{B6298D68-0A88-4F94-AD0F-EC30C5115913}')!
Torus comment: ''!
!Torus categoriesForClass!Kernel-Objects! !
!Torus methodsFor!

fixCoords: coords
	1 to: shape size
		do: 
			[:i |
			| val newVal |
			val := coords at: i.
			newVal := val \\ (shape at: i).
			coords at: i put: newVal].
	^coords!

getPoint: coords
	self fixCoords: coords.
	^points at: coords ifAbsentPut: [TorusPoint newWithTorus: self coords: coords]!

initWithShape: s
	shape := s.
	points := Dictionary new! !
!Torus categoriesFor: #fixCoords:!private! !
!Torus categoriesFor: #getPoint:!public! !
!Torus categoriesFor: #initWithShape:!private! !

!Torus class methodsFor!

shape: s
	| torus zeroCoords |
	torus := super new.
	torus initWithShape: s copy.
	zeroCoords := Array new: s size withAll: 0.
	^torus getPoint: zeroCoords! !
!Torus class categoriesFor: #shape:!public! !


"Filed out from Dolphin Smalltalk 7"!

Object subclass: #TorusPoint
	instanceVariableNames: 'torus coords value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusPoint guid: (GUID fromString: '{4DD6398F-27CC-4A95-B5A6-BE77C76C912C}')!
TorusPoint comment: ''!
!TorusPoint categoriesForClass!Kernel-Objects! !
!TorusPoint methodsFor!

- direction
	^self movedOn: direction negated by: 1!

% desc
	^TorusIterWalk
		newWithPoint: self
		length: desc key
		direction: desc value!

& stepBlock
	^TorusBlockWalk newWithPoint: self step: stepBlock!

@ ds
	| newCoords |
	newCoords := coords copy.
	1 to: newCoords size
		do: 
			[:i |
			| diff coord |
			diff := ds at: i.
			coord := newCoords at: i.
			newCoords at: i put: coord + diff].
	^torus getPoint: newCoords!

| direction
	^TorusLoopWalk newWithPoint: self direction: direction!

+ direction
	^self movedOn: direction by: 1!

initWithTorus: t coords: cs
	torus := t.
	coords := cs!

movedOn: direction by: x
	| diff idx newCoords |
	direction = 0 ifTrue: [^self].
	diff := direction > 0 ifTrue: [x] ifFalse: [x negated].
	idx := direction abs.
	newCoords := coords copy.
	newCoords at: idx put: (coords at: idx) + diff.
	^torus getPoint: newCoords!

printOn: target
	value printOn: target!

value
	^value!

value: v
	value := v! !
!TorusPoint categoriesFor: #-!public! !
!TorusPoint categoriesFor: #%!public! !
!TorusPoint categoriesFor: #&!public! !
!TorusPoint categoriesFor: #@!public! !
!TorusPoint categoriesFor: #|!public! !
!TorusPoint categoriesFor: #+!public! !
!TorusPoint categoriesFor: #initWithTorus:coords:!private! !
!TorusPoint categoriesFor: #movedOn:by:!private! !
!TorusPoint categoriesFor: #printOn:!public! !
!TorusPoint categoriesFor: #value!public! !
!TorusPoint categoriesFor: #value:!public! !

!TorusPoint class methodsFor!

newWithTorus: t coords: cs
	| r |
	r := super new.
	r initWithTorus: t coords: cs.
	^r! !
!TorusPoint class categoriesFor: #newWithTorus:coords:!public! !


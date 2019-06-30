!
!  BRACY/INTERNAL. Internal representation of a Bracy source.
!
!  Copyright © 2014 James B. Moen.
!
!  This  program is free  software: you  can redistribute  it and/or  modify it
!  under the terms  of the GNU General Public License as  published by the Free
!  Software Foundation,  either version 3 of  the License, or  (at your option)
!  any later version.
!
!  This program is distributed in the  hope that it will be useful, but WITHOUT
!  ANY  WARRANTY;  without even  the  implied  warranty  of MERCHANTABILITY  or
!  FITNESS FOR  A PARTICULAR PURPOSE.  See  the GNU General  Public License for
!  more details.
!
!  You should have received a copy of the GNU General Public License along with
!  this program.  If not, see <http://www.gnu.org/licenses/>.
!

(prog

!  Constants that identify internal data objects.

  makeTag :− enum()

  atomTag :− makeTag()  !  Object is an ATOM.
  pairTag :− makeTag()  !  Object is a PAIR.
  markTag :− makeTag()  !  Object is a previously visited PAIR.
  wordTag :− makeTag()  !  Object is a WORD.

!  ATOM. An object representing a delimiter or symbol, other than a word. LABEL
!  identifies what kind of ATOM this is. NAME may be used to write the ATOM for
!  debugging.

  atom :−
   (tuple
     int    tag,
     int    label,
     string name)

!  PAIR. An object that links other objects together. CAR and CDR both point to
!  other objects. Most objects will be PAIRs, so OBJECT is a synonym for PAIR.

  pair :−
   (tuple
     var int        tag,
     var ref object car,
     var ref object cdr)

  object :− pair

!  WORD. An object representing a word. CHARS are the chars in the word. STYLES
!  are the chars's positionally corresponding style sets.

  word :−
   (tuple
     int     tag,
     string  chars,
     row set styles)

!  IS ATOM. Test if ITEM is an atom.

  isAtom :−
   (form (ref object item) bool:
    (with item :− (past item)
     do item ≠ nil ∧ item↑.tag = atomTag))

!  IS PAIR. Test if ITEM is a pair. IS SCOPE is a synonym.

  isPair :−
   (form (ref object item) bool:
    (with item :− (past item)
     do item ≠ nil ∧ item↑.tag = pairTag))

  isScope :− isPair

!  IS WORD. Test if ITEM is a word.

  isWord :−
   (form (ref object item) bool:
    (with item :− (past item)
     do item ≠ nil ∧ item↑.tag = wordTag))

!  MAKE ATOM. Return a pointer to a new ATOM that contains NAME and LABEL.

  makeAtom :−
   (proc (string name, int label) ref object:
    (with item :− fromHeap(var atom)
     do item↑.tag   := atomTag
        item↑.name  := name
        item↑.label := label
        item{ref object}))

!  MAKE PAIR. Return a pointer to a new PAIR that holds CAR and CDR.

  makePair :−
   (proc (ref object car, ref object cdr) ref object:
    (with item :− fromHeap(var pair)
     do item↑.tag := pairTag
        item↑.car := car
        item↑.cdr := cdr
        item{ref object}))

!  MAKE WORD. Return a pointer to a new WORD with copies of CHARS and STYLES.

  makeWord :−
   (proc (var char0Buffer chars, var setBuffer styles) ref object:
    (with
      var row var char0 newChars  :− fromHeap(length(chars) + 1, var char0)
      var row var set   newStyles :− fromHeap(length(styles), var set)
      ref var word      newWord   :− fromHeap(var word)
     do newWord↑.tag    := wordTag
        newWord↑.chars  := newChars{string}
        newWord↑.styles := newStyles{row set}
        restart(chars)
        (for char0 ch in elements(chars)
         do newChars↑ := ch
            newChars += 1)
        newChars↑ := '\0'
        restart(styles)
        (for set style in elements(styles)
         do newStyles↑ := style
            newStyles += 1)
        newWord{ref object}))

!  UNMAKE. Recursively traverse ITEM, returning it to the heap. ITEM is assumed
!  to have no circular pointers.

  unmake :−
   (proc (ref object item) void:
    (with

!  UNMAKE PAIR. Return the pair ITEM back to the heap.

      unmakePair :−
       (form () void:
        (with var ref object items :− item
         do (while items ≠ nil
             do unmake(items↑.car)
                items := (items↑.cdr also toHeap(items)))))

!  UNMAKE WORD. Return the word ITEM back to the heap.

      unmakeWord :−
       (form () void:
         toHeap(item{ref word}↑.chars)
         toHeap(item{ref word}↑.styles)
         toHeap(item))

!  UNMAKE UNKNOWN. We get here if ITEM has an unknown TAG slot.

      unmakeUnknown :−
       (form () void:
         fail(''Cannot unmake tag %i at %08X.'': item↑.tag, item))

!  Dispatch on ITEM's TAG slot. We never return ATOMs to the heap.

     do (if item ≠ nil
         then (case item↑.tag
               of atomTag: skip
                  pairTag: unmakePair()
                  wordTag: unmakeWord()
                  none:    unmakeUnknown()))))
)

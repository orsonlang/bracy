!
!  BRACY/SET. Sets of small nonnegative integers.
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

!  SET. A set of zero or more elements that fit in an INT.

  set :− (tuple int Bits)

!  ∅. A set with no elements.

  set ∅ :−
   (with var set self
    do self.Bits := 0
       self)

!  "∪". Return a set like LEFT except that it contains (the elements in) RIGHT.

  "∪" :−
   (alt
    (form (set left, int right) set:
     (with var set left :− (past left)
      do left ∪= right
         left)),
    (form (set left, set right) set:
     (with var set left :− (past left)
      do left ∪= right
         left)))

!  "∪=". Add (the elements in) RIGHT to the set variable LEFT.

  "∪=" :−
   (alt
    (form (var set left, int right) void:
      left.Bits |= 1{int} ← right),
    (form (var set left, set right) void:
      left.Bits |= right.Bits))

!  "−". Return a set like LEFT except that it doesn't contain (the elements in)
!  RIGHT.

  "−" :−
   (alt
    (form (set left, int right) set:
     (with var set left :− (past left)
      do left −= right
         left)),
    (form (set left, set right) set:
     (with var set left :− (past left)
      do left −= right
         left)))

!  "−=". Remove (the elements in) RIGHT from the set variable LEFT.

  "−=" :−
   (alt
    (form (var set left, int right) void:
      left.Bits &= ~(1{int} ← right)),
    (form (var set left, set right) void:
      left.Bits &= ~right.Bits))

!  "≠". Test if two sets are not equal.

  "≠" :−
   (form (set left, set right) bool:
     left.Bits ≠ right.Bits)

!  "=". Test if two sets are equal.

  "=" :−
   (form (set left, set right) bool:
     left.Bits = right.Bits)

!  " {}". Cast the set LEFT to an INT.

  " {}" :−
   (form (set left, type int) int:
     left.Bits)

!  "∊". Test if the element LEFT is in the set RIGHT.

  "∊" :−
   (form (int left, set right) bool:
     right.Bits & (1{int} ← left) ≠ 0)

!  ELEMENTS. Iterator. Call BODY on each element E in the set S.

  elements :−
   (form (set s) foj:
    (form (form (int) obj body) obj:
     (for int e in 0, limit(set)
      do (if e ∊ s
          then body(e)))))

!  LIMIT. Return the maximum element that can fit in a set, a constant.

  limit :−
   (form (type set) inj:
     8 × size(set.Bits) − 1)

!  MAKE SET. Return a new set containing integers in the list R.

  makeSet :−
   (form (list r) set:
    (with var set s :− ∅
     do (for obj e, list r in tails(isInt, r)
         do (if 0 ≤ e ≤ limit(set)
             then s ∪= e
             else error(r, "out of range")))
        s))
)

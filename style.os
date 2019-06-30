!
!  BRACY/STYLE. Stacks of style codes.
!
!  Copyright © 2012 James B. Moen.
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

!  STYLE STACK. A linked stack, implemented as a chain of LAYERs.

  styleStack :− ref layer

!  LAYER. Whenever the scanner encounters an OPEN TOKEN, we push a LAYER on the
!  style stack.  It holds STYLE, the current set of style codes, and TOKEN, the
!  token corresponding to OPEN TOKEN's matching CLOSE TOKEN.  We keep the stack
!  shallow by run-length encoding, so COUNT tells how many times in a row we've
!  pushed identical copies of STYLE and TOKEN. NEXT points to the next LAYER in
!  the chain. See:
!
!  Steele, Guy Lewis Jr.  Sussman, Gerald J.  "The Dream of a Lifetime:  A Lazy
!  Variable Extent Mechanism."   Conference Record of the 1980 Lisp Conference.
!  Stanford, California. August 1980, pp. 163-172.

  layer :−
   (tuple
     var int       count,
     set           style,
     int           token,
     var ref layer next)

!  IS EMPTY. Test if STACK is empty.

  isEmpty :−
   (form (styleStack stack) bool:
     stack = nil)

!  MAKE STYLE STACK. Return a new empty style stack.

  makeStyleStack :−
   (form () styleStack:
     nil)

!  PUSH. Push TOKEN and STYLE on STACK.

  push :−
   (form (var styleStack stack, int token, set style) void:
    (if isEmpty(stack) ∨ stack↑.style ≠ style ∨ stack↑.token ≠ token
     then (with ref var layer temp :− fromHeap(var layer)
           do temp↑.count := 1
              temp↑.style := style
              temp↑.token := token
              temp↑.next  := stack
              stack       := temp{ref layer})
     else stack↑.count += 1))

!  POP. Pop TOKEN and STYLE off STACK. We assume STACK isn't empty.

  pop :−
   (form (var int token, var set style, var styleStack stack) void:
     token := stack↑.token
     style := stack↑.style
     (if stack↑.count = 1
      then stack := (stack↑.next also toHeap(stack))
      else stack↑.count −= 1))
)

!
!  BRACY/CODE. Char style codes.
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

!  We've implemented the few text styles that are supposed to be present in all
!  browsers. We also have a small capitals style which uses capital letters and
!  <SMALL> tags. See:
!
!  Thomas A. Powell. HTML: The Complete Reference. 3rd Edition. Osborne McGraw-
!  Hill, 2001. Page 93.
!
!  The QUOTED style is an undocumented kludge that temporarily turns off the -q
!  and -u command options. It may be obtained by writing {q text}. For example,
!  {q '}, {q `}, and {q ^} are rendered as an apostrophe, a grave accent, and a
!  caret, respectively.

(prog

!  Constants that represent char style codes. They must fit in a SET.

  makeCode :− enum(limit(set))

  boldCode      :− makeCode()  !  Boldface.
  capitalCode   :− makeCode()  !  Small capitals.
  italicCode    :− makeCode()  !  Italic.
  grayCode      :− makeCode()  !  Gray.
  quotedCode    :− makeCode()  !  Quoted.
  strikeCode    :− makeCode()  !  Strikethrough.
  subCode       :− makeCode()  !  Subscript.
  subSubCode    :− makeCode()  !  Sub-subscript.
  superCode     :− makeCode()  !  Superscript.
  underlineCode :− makeCode()  !  Underlined.
  typedCode     :− makeCode()  !  Typewriter.
  maxCode       :− makeCode()  !  Number of type style codes.

!  The constant array CODE TO OPEN maps style codes to their opening HTML tags,
!  and CODE TO CLOSE does the same with closing tags. Both arrays are necessary
!  to handle GRAY CODE. CAPITAL CODE and QUOTED CODE have no tags of their own,
!  but we'll deal with them later.

  codeToClose :−
   (with var [maxCode] string self
    do self[boldCode]      := ''</b>''
       self[capitalCode]   := ''</small>''
       self[italicCode]    := ''</i>''
       self[grayCode]      := ''</font>''
       self[quotedCode]    := ϵ
       self[strikeCode]    := ''</strike>''
       self[subCode]       := ''</sub>''
       self[subSubCode]    := ''</sub>''
       self[superCode]     := ''</sup>''
       self[underlineCode] := ''</u>''
       self[typedCode]     := ''</tt>''
       self)

  codeToOpen :−
   (with var [maxCode] string self
    do self[boldCode]      := ''<b>''
       self[capitalCode]   := ''<small>''
       self[italicCode]    := ''<i>''
       self[grayCode]      := ''<font color="#A9A9A9">''
       self[quotedCode]    := ϵ
       self[strikeCode]    := ''<strike>''
       self[subCode]       := ''<sub>''
       self[subSubCode]    := ''<sub>''
       self[superCode]     := ''<sup>''
       self[underlineCode] := ''<u>''
       self[typedCode]     := ''<tt>''
       self)

!  CODE STACK. Return the type of a code stack holding at most LIMIT codes. The
!  slot COUNT tells how many codes are on the stack, CODES is the stack itself,
!  and STYLE is the union of all codes from CODES. If LIMIT is missing, then we
!  return a joker that matches a CODE STACK of any size.

  codeStack :−
   (alt
    (form () type tup:
     (tuple
       int        count,
       [] var int codes,
       set        style)),
    (form (inj limit) type tup:
     (if isInt(limit)
      then (if limit ≥ 0
            then (tuple
                   int             count,
                   [limit] var int codes,
                   set             style)
            else error($limit, "out of range")
                 codeStack(0))
      else error($limit, "constant expected")
           codeStack(0))))

!  IS EMPTY. Test if STACK is empty.

  isEmpty :−
   (form (codeStack() stack) bool:
     stack.count = 0)

!  IS FULL. Test if STACK is full.

  isFull :−
   (form (codeStack() stack) bool:
     stack.count = length(stack.codes))

!  MAKE CODE STACK. Return a code stack of at most MAX COUNT codes.

  makeCodeStack :−
   (form (inj limit) codeStack():
    (if isInt(limit)
     then (if limit ≥ 0
           then (with var codeStack(limit) self
                 do self.count := 0
                    self.style := ∅
                    self)
           else error($limit, "out of range")
                makeCodeStack(0))
     else error($limit, "constant expected")
          makeCodeStack(0)))

!  POP. Delete the topmost code from STACK. We assume STACK is not empty.

  pop :−
   (form (var codeStack() stack) void:
     stack.count −= 1
     stack.style −= stack.codes[stack.count])

!  PUSH. Add a code to the top of STACK. We assume STACK is not full.

  push :−
   (form (var codeStack() stack, int code) void:
    (with code :− (past code)
     do stack.style ∪= code
        stack.codes[stack.count] := code
        stack.count += 1))

!  TOP. Return the topmost code in STACK. We assume STACK is not empty.

  top :−
   (form (codeStack() stack) int:
     stack.codes[stack.count − 1])
)

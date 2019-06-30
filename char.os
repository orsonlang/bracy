!
!  BRACY/CHAR. Read chars from a Bracy source file.
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

!  NEXT CHAR. Copy the next char from LINE into CH. If LINE is empty, then read
!  the next line before we do it.

  nextChar :−
   (proc () void:
    (if atEnd(line)
     then nextLine())
    ch := start(line)
    advance(line))

!  NEXT LINE. Read the next line from SOURCE into LINE.  If L is the ASCII line
!  feed char and R is the ASCII return char then a line may be terminated by L,
!  R, LR, or RL. The last line in SOURCE may be optionally terminated by an end
!  of file char instead.

  nextLine :−
   (proc () void:
    (with
      var char ch

!  IS ILLEGAL CHAR. Test if CH is a control char, which isn't allowed to appear
!  in a source file.

      isIllegalChar :−
       (form (char ch) bool:
         '\#00' ≤ ch ≤ '\#1F' ∨
         ch = '\#7F'          ∨
         '\#80' ≤ ch ≤ '\#9F' ∨
         ch = '\#2028'        ∨
         ch = '\#2029')

!  NEXT END FILE. Read an EOF CHAR into LINE and stop reading.

      nextEndFile :−
       (form () bool:
        (if atStart(line)
         then append(line, eofChar))
        append(line, ' ')
        false)

!  NEXT LINEFEED. Read an EOL CHAR into LINE and stop reading.

      nextLinefeed :−
       (form () bool:
         ch := read(source)
         (if ch ≠ endChar ∧ ch ≠ returnChar
          then unread(source, ch))
         append(line, ' ')
         append(line, eolChar)
         append(line, ' ')
         false)

!  NEXT RETURN. Read an EOL CHAR into LINE and stop reading.

      nextReturn :−
       (form () bool:
         ch := read(source)
         (if ch ≠ endChar ∧ ch ≠ linefeedChar
          then unread(source, ch))
         append(line, ' ')
         append(line, eolChar)
         append(line, ' ')
         false)

!  NEXT OTHER. Read an ordinary char into LINE and keep reading.

      nextOther :−
       (form () bool:
        (if isFull(line)
         then syntaxError(lineTooLongErr)
              remove(line, 3)
              append(line, ' ')
              append(line, eolChar)
              append(line, ' ')
         else if isIllegalChar(ch)
              then syntaxError(charErr)
                   append(line, ' ')
              else append(line, ch))
        true)

!  This is NEXT LINE's body. We scold the user about any errors on the previous
!  line, then read the next line.

     do empty(line)
        sourceLine += 1
        (while
         (case ch := read(source); ch
          of endChar:      nextEndFile()
             linefeedChar: nextLinefeed()
             returnChar:   nextReturn()
             none:         nextOther()))))
)

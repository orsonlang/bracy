!
!  BRACY/DEBUG. Methods for debugging.
!
!  Copyright © 2018 James B. Moen.
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

!  These methods are never called by Bracy: they're only for debugging. The box
!  '□' is used throughout to indicate an unprintable character.

(prog

!  NEXT CHARS. Write characters from the source file to OUTPUT.

  nextChars :−
   (form () void:
     nextLine()
     (while
       nextChar()
       ch ≠ eofChar
      do write('"')
         write(ch)
         writeln('"')))

!  NEXT LINES. Write lines from the source file to OUTPUT.

  nextLines :−
   (form () void:
    (with

!  WRITING LINE. Write a line.

      writingLine :−
       (form () void:
         write('"')
         (for char ch in items(line)
          do (if width(ch) ≤ 0
              then write('□')
              else write(ch)))
         writeln('"'))

!  Read lines until we get one whose first character is an EOF CHAR.

     do (while
          nextLine()
          writingLine()
          start(line) ≠ eofChar)))

!  NEXT SUBTOKENS. Write subtokens from the source file to OUTPUT. We indent to
!  show nesting within style scopes.

  nextSubtokens :−
   (form () void:
    (with
      var int indent       :− 0
      int     indentChange :− 2

!  WRITING SUBTOKEN. Write SUBTOKEN and its associated objects.

      writingSubtoken :−
       (form (int subtoken) void:
        (with

!  WRITING BLANK. Write a BLANK SUBTOKEN.

          writingBlank :−
           (form () void:
             writeBlanks(output, indent)
             writeln(''blank''))

!  WRITING CLOSE. Write a CLOSE SUBTOKEN.

          writingClose :−
           (form () void:
             indent −= indentChange
             writeBlanks(output, indent)
             writeln(''close''))

!  WRITING END LINE. Write an END LINE SUBTOKEN.

          writingEndLine :−
           (form () void:
             writeBlanks(output, indent)
             writeln(''end line''))

!  WRITING GLYPH. Write a GLYPH SUBTOKEN. We write both its character and style
!  parts.

          writingGlyph :−
           (form () void:
             writeBlanks(output, indent)
             write(''glyph '')
             (if width(subtokenChar) ≤ 0
              then write('□')
              else write(subtokenChar))
             writeSet(subtokenStyle)
             writeln())

!  WRITING IGNORED. Write an IGNORED SUBTOKEN.

          writingIgnored :−
           (form () void:
             writeBlanks(output, indent)
             writeln(''ignored''))

!  WRITING OPEN. Write an OPEN SUBTOKEN and the scope name it establishes.

          writingOpen :−
           (form () void:
             writeBlanks(output, indent)
             write(''open '')
             (if subtokenObject = nil
              then writeln(''nil'')
              else if isAtom(subtokenObject)
                   then writeln(''"%s"'': subtokenObject{ref atom}↑.name)
                   else writeln(''non atom''))
             indent += indentChange)

!  WRITING UNKNOWN. Write an unknown subtoken. This should never happen.

          writingUnknown :−
           (form () void:
             writeBlanks(output, indent)
             writeln(''unknown''))

!  This is WRITING SUBTOKEN's body. Dispatch on SUBTOKEN.

         do (case subtoken
             of blankSubtoken:   writingBlank()
                closeSubtoken:   writingClose()
                endLineSubtoken: writingEndLine()
                glyphSubtoken:   writingGlyph()
                ignoredSubtoken: writingIgnored()
                openSubtoken:    writingOpen()
                none:            writingUnknown())))

!  Initialize the subtoken scanner and read tokens until END FILE SUBTOKEN.

     do nextLine()
        nextChar()
        nextSubtoken()
        (while subtoken ≠ endFileSubtoken
         do writingSubtoken(subtoken)
            nextSubtoken())))

!  NEXT TOKENS.  Write tokens from the source file to OUTPUT. We indent to show
!  nesting within non-style scopes.

  nextTokens :−
   (form () void:
    (with
      var int indent       :− 0
      int     indentChange :− 2

!  WRITING TOKEN. Write TOKEN and its associated objects.

      writingToken :−
       (form (int token) void:
        (with

!  WRITING BLANK. Write a BLANK TOKEN.

          writingBlank :−
           (form () void:
             writeBlanks(output, indent)
             writeln(''blank''))

!  WRITING CLOSE. Write a CLOSE TOKEN.

          writingClose :−
           (form () void:
             indent −= indentChange
             writeBlanks(output, indent)
             writeln(''close''))

!  WRITING END LINE. Write an END LINE TOKEN.

          writingEndLine :−
           (form () void:
             writeBlanks(output, indent)
             writeln(''end line''))

!  WRITING OPEN. Write an OPEN TOKEN and the scope name it establishes.

          writingOpen :−
           (form () void:
             writeBlanks(output, indent)
             write(''open '')
             (if tokenObject = nil
              then writeln(''nil'')
              else if isAtom(tokenObject)
                   then writeln(''"%s"'': tokenObject{ref atom}↑.name)
                   else writeln(''non atom''))
             indent += indentChange)

!  WRITING WORD. Write a WORD TOKEN and its characters, but not its styles.

          writingWord :−
           (form () void:
             writeBlanks(output, indent)
             write(''word '')
             (if tokenObject = nil
              then writeln(''nil'')
              else write('"')
                   (for char ch in elements(tokenObject{ref word}↑.chars)
                    do (if width(ch) ≤ 0
                        then write('□')
                        else write(ch)))
                   writeln('"')))

!  WRITING UNKNOWN. Write an unknown token. This should never happen.

          writingUnknown :−
           (form () void:
             writeBlanks(output, indent)
             writeln(''unknown''))

!  This is WRITING TOKEN's body. Dispatch on TOKEN.

         do (case token
             of blankToken:   writingBlank()
                closeToken:   writingClose()
                endLineToken: writingEndLine()
                openToken:    writingOpen()
                wordToken:    writingWord()
                none:         writingUnknown())))

!  Initialize the token scanner and read tokens until END FILE TOKEN.

     do nextLine()
        nextChar()
        nextSubtoken()
        nextToken()
        (while token ≠ endFileToken
         do writingToken(token)
            nextToken())))

!  WRITE OBJECT. Write ITEM in a Lispy way. REMAINING is the number of chars we
!  have left on OUTPUT's current line.

  writeObject :−
   (proc (ref object item) void:
    (with
      int     maxRemaining :− 79
      var int remaining    :− maxRemaining

!  WRITING OBJECT. Do all the work for WRITE OBJECT.

      writingObject :−
       (proc (ref object item) void:
        (with

!  WIDTH. Return the number of columns needed to write CHARS. Chars with widths
!  less than or equal to 0 are written as □'s.

          width :−
           (form (string chars) int:
            (with var int total :− 0
             do (for char ch in elements(chars)
                 do (with int temp :− width(ch)
                     do (if temp ≤ 0
                         then total += 1
                         else total += temp)))
                total))

!  WRITING ATOM. Write the name of the atom OBJECT.

          writingAtom :−
           (form () void:
            (with name :− item{ref atom}↑.name
             do writingNewline(length(name))
                write(name)))

!  WRITING BLANK. Write a blank if there is room, otherwise write a newline.

          writingBlank :−
           (form () void:
            (if remaining = 0
             then writeln()
                  remaining := maxRemaining
             else write(' ')
                  remaining −= 1))

!  WRITING MARKED. Write a pair that is already being written by a pending call
!  to WRITING OBJECT.

          writingMarked :−
           (form () void:
             writingNewline(3)
             write(''...''))

!  WRITING NEWLINE. Make sure we have NEEDED chars left on the current line.

          writingNewline :−
           (form (int needed) void:
            (if needed > remaining
             then writeln()
                  remaining := maxRemaining)
            remaining −= needed)

!  WRITING NIL. Write NIL. Sheesh.

          writingNil :−
           (form () void:
             writingNewline(3)
             write(''nil''))

!  WRITING PAIR. Write a pair as a Lispy list in parentheses. Change the pair's
!  TAG slot to MARKED TAG when we see it for the first time. If we see it later
!  then we've entered a circular structure, so we write "..." instead.

          writingPair :−
           (form () void:
            (with var ref object items :− item
             do item↑.tag := markTag
                writingNewline(1)
                write('(')
                writingObject(items↑.car)
                items := items↑.cdr
                (while items ≠ nil
                 do writingBlank()
                    writingObject(items↑.car)
                    items := items↑.cdr)
                writingNewline(1)
                write(')')
                item↑.tag := pairTag))

!  WRITING UNKNOWN. Write an object with an unknown tag. This indicates a bug.

          writingUnknown :−
           (form () void:
             writingNewline(length(''unknown'') + 8)
             write(''unknown%08X'': item))

!  WRITING WORD. Write the CHARS slot of a word in double quotes.

          writingWord :−
           (form () void:
            (with string chars :− item{ref word}↑.chars
             do writingNewline(width(chars) + 2)
                write('"')
                (for char ch in elements(chars)
                 do (if width(ch) ≤ 0
                     then write('□')
                     else write(ch)))
                write('"')))

!  This is WRITING OBJECT's body. Dispatch on the TAG slot of ITEM.

         do (if item = nil
             then writingNil()
             else (case item↑.tag
                   of atomTag: writingAtom()
                      markTag: writingMarked()
                      pairTag: writingPair()
                      wordTag: writingWord()
                      none:    writingUnknown()))))

!  This is WRITE OBJECT's body. Write ITEM followed by a newline.

     do writingObject(item)
        writeln()))

!  WRITE SET. Write SET as { } or { 0 } or { 0, 1, 2 } etc.

  writeSet :−
   (form (set set) obj:
    (with var string delimiter :− '' ''
     do write('{')
        (for int element in elements(set)
         do write(''%s%i'': delimiter, element)
            delimiter := '', '')
        write('' }'')))
)

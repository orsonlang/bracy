!
!  BRACY/ORSONIZE. Modify styles to display Orson source text.
!
!  Copyright © 2016 James B. Moen.
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

!  FACE STYLES. Set of style codes for letters and digits.

  set faceStyles :− makeSet(: boldCode, capitalCode, italicCode, typedCode)

!  ORSONIZE. Here ITEM points to a WORD that holds a line of text. Traverse the
!  line via LINE CHARS and LINE STYLES, making changes to LINE STYLES as we go.

  orsonize :−
   (proc (ref object item) void:
    (with
      var string      lineChars  :− item{ref word}↑.chars
      var row var set lineStyles :− item{ref word}↑.styles{row var set}

!  NEXT CHAR. Move the current position in LINE CHARS one place to the right.

      nextChar :−
       (form () void:
         lineChars  := tail(lineChars)
         lineStyles := tail(lineStyles))

!  NEXT CHAR OR STRING. Parse a char constant or a string constant, leaving its
!  delimiting apostrophes unaffected by the -q option. We don't check for legal
!  closing delimiters.

      nextCharOrString :−
       (form () void:
        (with var bool going :− true
         do stylize(head(lineStyles), quotedCode)
            nextChar()
            (if head(lineChars) = '\''
             then stylize(head(lineStyles), quotedCode)
                  nextChar()
                  (while going
                   do (if head(lineChars) = '\''
                       then stylize(head(lineStyles), quotedCode)
                            nextChar()
                            (if head(lineChars) = '\''
                             then stylize(head(lineStyles), quotedCode)
                                  nextChar()
                                  (if head(lineChars) = '\''
                                   then stylize(head(lineStyles), quotedCode)
                                        nextChar())
                                  going := false
                             else if head(lineChars) = '\0'
                                  then going := false
                                  else nextSlashedChar())
                       else if head(lineChars) = '\0'
                            then going := false
                            else nextSlashedChar()))
             else (if head(lineChars)
                   then nextSlashedChar())
                  (if head(lineChars) = '\''
                   then stylize(head(lineStyles), quotedCode)
                        nextChar()))))

!  NEXT SLASHED CHAR. Parse a char inside a char constant or a string constant.
!  It might be backslashed, but we don't check for a legal letter following the
!  backslash.

      nextSlashedChar :−
       (proc () void:
        (if head(lineChars) = '\\'
         then nextChar()
              (if head(lineChars) = '#'
               then nextChar()
                    (while isDigit(head(lineChars), 16)
                     do nextChar())
               else if head(lineChars) ≠ '\0'
                    then nextChar())
         else nextChar()))

!  NEXT HOOK. Parse a hook name. We italicize its Roman letters.

      nextHook :−
       (form () void:
         nextSymbol()
         (while isPlain(head(lineChars))
          do (if isLetter(head(lineChars))
              then stylize(head(lineStyles), italicCode))
             nextChar()))

!  NEXT NUMBER. Parse a number.

      nextNumber :−
       (form () void:
        (while
          nextChar()
          isLetterOrDigit(head(lineChars)) ∨ head(lineChars) = '_'))

!  NEXT PLAIN NAME. Parse a plain name that may be reserved. If it is reserved,
!  then we embolden it, otherwise we italicize its Roman letters. If it has any
!  apostrophes, then protect them from the -q option.

      nextPlainName :−
       (form () void:
        (with
          var int         count      :− 0
          var string      nameChars  :− lineChars
          var row var set nameStyles :− lineStyles
          var buffer(maxSourceLineLength, char0) name
         do empty(name)
            (while isPlain(head(lineChars))
             do append(name, head(lineChars))
                count += 1
                nextChar())
            (if isReserved(name{string})
             then (while count > 0
                   do stylize(head(nameStyles), boldCode)
                      nameChars  := tail(nameChars)
                      nameStyles := tail(nameStyles)
                      count −= 1)
             else (while count > 0
                   do (if isLetter(head(nameChars))
                       then stylize(head(nameStyles), italicCode)
                       else if head(nameChars) = '\''
                            then stylize(head(nameStyles), quotedCode))
                      nameChars  := tail(nameChars)
                      nameStyles := tail(nameStyles)
                      count −= 1))))

!  NEXT QUOTED NAME. Parse a quoted name. Italicize its quote marks. Also if it
!  has apostrophes or grave accents, then protect them from the -q option.

      nextQuotedName :−
       (form () void:
         stylize(head(lineStyles), italicCode)
         nextChar()
         (while head(lineChars) ∧ head(lineChars) ≠ '"'
          do (if head(lineChars) = '\'' ∨ head(lineChars) = '`'
              then stylize(head(lineStyles), quotedCode))
             nextChar())
         (if head(lineChars) = '"'
          then stylize(head(lineStyles), italicCode)
               nextChar()))

!  NEXT SYMBOL. Parse a syntactic delimiter. Protect it from the -q option.

      nextSymbol :−
       (form () void:
         stylize(head(lineStyles), quotedCode)
         nextChar())

!  STYLIZE. Remove FACE STYLES from STYLE, and add CODE in their place.

      stylize :−
       (form (var set style, int code) void:
         style −= faceStyles
         style ∪= code)

!  This is ORSONIZE's body. We can stop parsing the line at a comment delimiter
!  since we leave comment styles unchanged.  We test for digits and apostrophes
!  first so IS PLAIN won't think they're parts of names.

     do (while head(lineChars) ∧ head(lineChars) ≠ '!'
         do (if isDigit(head(lineChars)) ∨ head(lineChars) = '#'
             then nextNumber()
             else if head(lineChars) = '\''
                  then nextCharOrString()
                  else if isPlain(head(lineChars))
                       then nextPlainName()
                       else if head(lineChars) = '"'
                            then nextQuotedName()
                            else if head(lineChars) = '?'
                                 then nextHook()
                                 else nextSymbol()))))
)

!
!  BRACY/SUBTOKEN. Read subtokens from a Bracy source file.
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

!  Constants that represent source subtokens.

  makeSubtoken :− enum(high(int0))

  blankSubtoken   :− makeSubtoken()  !  Separate words and scopes.
  closeSubtoken   :− makeSubtoken()  !  End a scope.
  endFileSubtoken :− makeSubtoken()  !  End of source file.
  endLineSubtoken :− makeSubtoken()  !  End of source line.
  glyphSubtoken   :− makeSubtoken()  !  A char with style information.
  ignoredSubtoken :− makeSubtoken()  !  A subtoken we don't care about.
  openSubtoken    :− makeSubtoken()  !  Begin a scope.

!  Constants that identify what ATOMs are for (see below).

  makeLabel :− enum()

  bulletLabel  :− makeLabel()  !  Scope with bulleted paragraphs.
  centerLabel  :− makeLabel()  !  Scope with centered lines.
  columnLabel  :− makeLabel()  !  Scope with unchanged paragraphs.
  displayLabel :− makeLabel()  !  Scope with unfilled lines.
  endLineLabel :− makeLabel()  !  End of line sentinel.
  endParaLabel :− makeLabel()  !  End of paragraph sentinel.
  gotoLabel    :− makeLabel()  !  Scope with a link.
  imageLabel   :− makeLabel()  !  Scope with an image.
  indentLabel  :− makeLabel()  !  Scope with indented lines.
  insertLabel  :− makeLabel()  !  Scope with an html file to insert.
  itemizeLabel :− makeLabel()  !  Scope with named paragraphs.
  justifyLabel :− makeLabel()  !  Scope with justified paragraphs.
  labelLabel   :− makeLabel()  !  Scope for an internal label.
  layoutLabel  :− makeLabel()  !  Scope to display a rectangular grid.
  leftLabel    :− makeLabel()  !  Scope with left-justified paragraphs.
  narrowLabel  :− makeLabel()  !  Scope with narrowed paragraphs.
  numberLabel  :− makeLabel()  !  Scope with numbered paragraphs.
  orsonLabel   :− makeLabel()  !  Scope to display Orson source code.
  overLabel    :− makeLabel()  !  Scope to display a big fraction.
  rightLabel   :− makeLabel()  !  Scope with right-justified paragraphs.
  rowLabel     :− makeLabel()  !  Scope for a row in a layout or table.
  ruleLabel    :− makeLabel()  !  Draw a horizontal line.
  tableLabel   :− makeLabel()  !  Scope to display a rectangular table.
  titleLabel   :− makeLabel()  !  Scope to display a title.
  unknownLabel :− makeLabel()  !  An unknown scope.

!  Some subtokens have associated ATOMs in SUBTOKEN OBJECT (see below).

  ref object bulletAtom  :− makeAtom(''bullet'',  bulletLabel)
  ref object centerAtom  :− makeAtom(''center'',  centerLabel)
  ref object columnAtom  :− makeAtom(''column'',  columnLabel)
  ref object displayAtom :− makeAtom(''display'', displayLabel)
  ref object endLineAtom :− makeAtom(''endLine'', endLineLabel)
  ref object endParaAtom :− makeAtom(''endPara'', endParaLabel)
  ref object gotoAtom    :− makeAtom(''goto'',    gotoLabel)
  ref object imageAtom   :− makeAtom(''image'',   imageLabel)
  ref object indentAtom  :− makeAtom(''indent'',  indentLabel)
  ref object insertAtom  :− makeAtom(''insert'',  insertLabel)
  ref object itemizeAtom :− makeAtom(''itemize'', itemizeLabel)
  ref object justifyAtom :− makeAtom(''justify'', justifyLabel)
  ref object labelAtom   :− makeAtom(''label'',   labelLabel)
  ref object layoutAtom  :− makeAtom(''layout'',  layoutLabel)
  ref object leftAtom    :− makeAtom(''left'',    leftLabel)
  ref object narrowAtom  :− makeAtom(''narrow'',  narrowLabel)
  ref object orsonAtom   :− makeAtom(''orson'',   orsonLabel)
  ref object overAtom    :− makeAtom(''over'',    overLabel)
  ref object numberAtom  :− makeAtom(''number'',  numberLabel)
  ref object rightAtom   :− makeAtom(''right'',   rightLabel)
  ref object rowAtom     :− makeAtom(''row'',     rowLabel)
  ref object ruleAtom    :− makeAtom(''rule'',    ruleLabel)
  ref object tableAtom   :− makeAtom(''table'',   tableLabel)
  ref object titleAtom   :− makeAtom(''title'',   titleLabel)
  ref object unknownAtom :− makeAtom(''unknown'', unknownLabel)

!  Variables.

  var styleStack opens           :− makeStyleStack()  !  Stack of OPEN tokens.
  var int        subtoken        :− blankSubtoken     !  Current subtoken.
  var char       subtokenChar    :− ' '               !  SUBTOKEN's char.
  var ref object subtokenObject  :− nil               !  SUBTOKEN's object.
  var set        subtokenStyle   :− ∅                 !  SUBTOKEN's style.

!  NEXT SUBTOKEN. Scan the next subtoken using chars read from SOURCE.

  nextSubtoken :−
   (proc () void:
    (with

!  NEXT BACKSLASH. Scan a char after a backslash as if it's a GLYPH SUBTOKEN.

      nextBackslash :−
       (form () void:
         nextChar()
         (if ch = eofChar
          then syntaxError(charExpectedErr)
               nextEndFile()
          else if ch = eolChar
               then nextBlank()
               else if ch = ' ' ∨ ch = '\\' ∨ ch = '{' ∨ ch = '}'
                    then nextGlyph()
                    else syntaxError(backslashErr)
                         nextBlank()))

!  NEXT BLANK. Scan a BLANK SUBTOKEN.

      nextBlank :−
       (form () void:
         subtoken := blankSubtoken
         subtokenChar := ' '
         subtokenObject := nil
         nextChar())

!  NEXT CLOSE. Scan a close brace, and pop a layer off the style stack. If that
!  layer corresponds to a scope that established a new style, then we treat the
!  subtoken as an IGNORED SUBTOKEN. Otherwise it's a CLOSE SUBTOKEN.

      nextClose :−
       (form () void:
        (if isEmpty(opens)
         then syntaxError(unexpectedBraceErr)
              subtoken := ignoredSubtoken
         else pop(subtoken, subtokenStyle, opens))
        subtokenObject := nil
        nextChar())

!  NEXT END FILE. Scan an END FILE SUBTOKEN. Do not read the next char, because
!  there isn't one.

      nextEndFile :−
       (form () void:
         subtoken := endFileSubtoken
         subtokenObject := nil)

!  NEXT END LINE. Scan an END LINE SUBTOKEN.

      nextEndLine :−
       (form () void:
         subtoken := endLineSubtoken
         subtokenObject := endLineAtom
         nextChar())

!  NEXT GLYPH. Scan any char that isn't a delimiter.

      nextGlyph :−
       (alt
        (form () void:
          nextGlyph(ch)),
        (form (char ch) void:
          subtoken := glyphSubtoken
          subtokenChar := ch
          subtokenObject := nil
          nextChar()))

!  NEXT OPEN. Scan an open brace followed by the name of a scope.

      nextOpen :−
       (form () void:
        (with
          var set  style   :− ∅
          var bool stylish :− true
          var buffer(maxSourceLineLength + 1, char0) openChars

!  NEXT COMMENT. Scan a comment. Comments may be nested. We stop scanning if we
!  see a matching close brace or the end of the source file.

          nextComment :−
           (form () void:
            (with var int count :− 1
             do (while count > 0 ∧ ch ≠ eofChar
                 do (if ch = '\\'
                     then nextChar()
                          (if ch ≠ eofChar
                           then nextChar())
                     else if ch = '{'
                          then count += 1
                               nextChar()
                          else if ch = '}'
                               then count −= 1
                                    nextChar()
                               else nextChar()))
                (if count > 0
                 then syntaxError(braceExpectedErr))
                subtoken := blankSubtoken
                subtokenObject := nil))

!  NEXT OTHER. Scan a char in a scope name that doesn't establish a style.

          nextOther :−
           (form () void:
             append(openChars, ch)
             stylish := false
             nextChar())

!  NEXT STYLE. Scan a char in a scope name that establishes a style code CODE.

          nextStyle :−
           (form (int code) void:
             append(openChars, ch)
             style ∪= code
             nextChar())

!  This is NEXT OPEN's body. Skip the open brace. If it's followed by "*", then
!  we have a comment.

         do nextChar()
            (if ch = '*'
             then nextComment()

!  If we don't have a comment, then the open brace must be followed by the name
!  of a scope: one or more letters, digits, and signs. These chars go into OPEN
!  SCOPE, and STYLISH tells if they established new style codes in STYLE.

             else empty(openChars)
                  (while isLetterOrDigit(ch) ∨ ch = '+' ∨ ch = '-' ∨ ch = '='
                   do (case ch
                       of '+': nextStyle(superCode)
                          '-': nextStyle(subCode)
                          '=': nextStyle(subSubCode)
                          'b': nextStyle(boldCode)
                          'c': nextStyle(capitalCode)
                          'i': nextStyle(italicCode)
                          'g': nextStyle(grayCode)
                          'q': nextStyle(quotedCode)
                          'u': nextStyle(underlineCode)
                          's': nextStyle(strikeCode)
                          't': nextStyle(typedCode)
                         none: nextOther()))

!  If we establish new style codes then our subtoken is IGNORED SUBTOKEN, which
!  is handled by the subtoken scanner. Otherwise our subtoken is OPEN SUBTOKEN,
!  which is handled by the token scanner.  Save the old SUBTOKEN STYLE, setting
!  up a new SUBTOKEN OBJECT or SUBTOKEN STYLE as needed.

                  (if stylish ∧ style ≠ ∅
                   then (while ch = ' ' ∨ ch = eolChar
                         do nextChar())
                        push(opens, ignoredSubtoken, subtokenStyle)
                        subtoken := ignoredSubtoken
                        subtokenStyle ∪= style
                   else push(opens, closeSubtoken, subtokenStyle)
                        subtoken := openSubtoken
                        subtokenObject :=
                         select(openChars{string}, unknownAtom:
                          (: ''bullet'',  bulletAtom),
                          (: ''center'',  centerAtom),
                          (: ''column'',  columnAtom),
                          (: ''display'', displayAtom),
                          (: ''goto'',    gotoAtom),
                          (: ''image'',   imageAtom),
                          (: ''indent'',  indentAtom),
                          (: ''insert'',  insertAtom),
                          (: ''itemize'', itemizeAtom),
                          (: ''justify'', justifyAtom),
                          (: ''label'',   labelAtom),
                          (: ''layout'',  layoutAtom),
                          (: ''left'',    leftAtom),
                          (: ''narrow'',  narrowAtom),
                          (: ''number'',  numberAtom),
                          (: ''orson'',   orsonAtom),
                          (: ''over'',    overAtom),
                          (: ''right'',   rightAtom),
                          (: ''row'',     rowAtom),
                          (: ''rule'',    ruleAtom),
                          (: ''table'',   tableAtom),
                          (: ''title'',   titleAtom))))))

!  This is NEXT SUBTOKEN's body. Look at CH to figure out what kind of subtoken
!  we have, then dispatch to code that scans it. Repeat, until we get one other
!  than IGNORED SUBTOKEN.

     do (while
         (case ch
          of ' ': nextBlank()
            '\\': nextBackslash()
             '}': nextClose()
             '{': nextOpen()
         eofChar: nextEndFile()
         eolChar: nextEndLine()
            none: nextGlyph())
         subtoken = ignoredSubtoken)))
)

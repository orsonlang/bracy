!
!  BRACY/TOKEN. Read tokens from a Bracy source file.
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

!  Constants. They represent source tokens.

  makeToken :− enum(high(int0))

  blankToken   :− makeToken()  !  Separate words and scopes.
  closeToken   :− makeToken()  !  End a scope.
  endFileToken :− makeToken()  !  End of source file.
  endLineToken :− makeToken()  !  End of source line.
  endParaToken :− makeToken()  !  End of source paragraph.
  openToken    :− makeToken()  !  Begin a scope.
  wordToken    :− makeToken()  !  A series of non-whitespace chars.

!  Variables.

  var int        token       :− blankToken  !  Current token.
  var ref object tokenObject :− nil         !  TOKEN's corresponding object.

!  NEXT TOKEN. Scan the next token using subtokens read from SOURCE.

  nextToken :−
   (proc () void:
    (with

!  NEXT BLANK. Scan a BLANK TOKEN.

      nextBlank :−
       (form () void:
         token := blankToken
         tokenObject := nil
         nextSubtoken())

!  NEXT CLOSE. Scan a CLOSE TOKEN. This is always a non-style scope because the
!  subtoken scanner keeps track of style scopes (see BRACY/SUBTOKEN).

      nextClose :−
       (form () void:
         token := closeToken
         tokenObject := nil
         nextSubtoken())

!  NEXT END FILE. Scan an END FILE TOKEN. Don't read the next subtoken, because
!  there isn't one.

      nextEndFile :−
       (form () void:
         token := endFileToken
         tokenObject := nil)

!  NEXT END LINE. Skip BLANK SUBTOKENs and END LINE SUBTOKENs, counting the END
!  LINE SUBTOKENs. Two or more END LINE SUBTOKENs gives an END PARAGRAPH TOKEN;
!  just one gives us an END LINE TOKEN.

      nextEndLine :−
       (form () void:
        (with var int count :− 0
         do (while
             (if subtoken = blankSubtoken
              then nextSubtoken()
                   true
              else if subtoken = endLineSubtoken
                   then count += 1
                        nextSubtoken()
                        true
                   else false))
            (if count > 1
             then token := endParaToken
                  tokenObject := endParaAtom
             else token := endLineToken
                  tokenObject := endLineAtom)))

!  NEXT OPEN. Scan an OPEN TOKEN. This always corresponds to a non-style scope,
!  since the subtoken scanner keeps track of style scopes (see BRACY/SUBTOKEN).

      nextOpen :−
       (form () void:
         token := openToken
         tokenObject := subtokenObject
         nextSubtoken())

!  NEXT WORD. Scan a WORD TOKEN: a series of one or more GLYPH SUBTOKENs.

      nextWord :−
       (form () void:
         empty(wordChars)
         empty(wordStyles)
         (while subtoken = glyphSubtoken
          do appendCharAndStyle(wordChars, wordStyles)
             nextSubtoken())
         token := wordToken
         tokenObject := makeWord(wordChars, wordStyles))

!  NEXT UNKNOWN. Scan an unknown token. This should never happen.

      nextUnknown :−
       (form () void:
         syntaxError(unknownTokenErr)
         token := blankToken
         tokenObject := nil
         nextSubtoken())

!  This is NEXT TOKEN's body. Look at CH to decide what token we have, and then
!  dispatch to code that scans it. Repeat until we get a token other than BLANK
!  TOKEN.

     do (while
         (case subtoken
          of blankSubtoken: nextBlank()
             closeSubtoken: nextClose()
           endFileSubtoken: nextEndFile()
           endLineSubtoken: nextEndLine()
             glyphSubtoken: nextWord()
              openSubtoken: nextOpen()
                      none: nextUnknown())
         token = blankToken)))

!  APPEND CHAR AND STYLE. Add GLYPH TOKEN's char and style to the buffers CHARS
!  and STYLES, respectively. Only lower case chars get CAPITAL CODE.

  appendCharAndStyle :−
   (form (var char0Buffer chars, var setBuffer styles) void:
     append(chars, subtokenChar)
     (if capitalCode ∊ subtokenStyle
      then (if isLower(subtokenChar)
            then append(styles, subtokenStyle)
            else append(styles, subtokenStyle − capitalCode))
      else append(styles, subtokenStyle)))
)

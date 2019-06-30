!
!  BRACY/PARSE. Translate a Bracy source to internal form.
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

!  NEXT SOURCE. Parse the current source file, which should contain a series of
!  scopes. Return a chain of these scopes in internal form.

  nextSource :−
   (form () ref object:
    (with
      var int        count :− 0
      ref object     first :− makePair(nil, nil)
      var ref object last  :− first
     do nextLine()
        nextChar()
        nextSubtoken()
        nextToken()
        makeLabelTree()
        seenTitle := false
        (while token ≠ endFileToken
         do (case token
             of endLineToken: nextIgnoredToken()
                endParaToken: nextIgnoredToken()
                   openToken: nextExpectedScope(count, last)
                   wordToken: nextUnexpectedToken(scopeExpectedErr)
                        none: nextUnexpectedToken(unknownTokenErr)))
        (if count = 0
         then syntaxError(scopeExpectedErr))
        first↑.cdr also toHeap(first)))

!  NEXT SCOPE. Parse a scope and return its internal form representation.

  nextScope :−
   (proc () ref object:
    (with
      var int        count :− 0
      ref object     first :− makePair(tokenObject, nil)
      var ref object last  :− first

!  IN SCOPE. Test if TOKEN can occur inside a scope.

      inScope :−
       (form () bool:
         token ≠ closeToken ∧ token ≠ endFileToken)

!  NEXT EMPTY. Parse a scope that has no nested scopes or words.

      nextEmpty :−
       (form () void:
         nextOpen()
         (while inScope()
          do (case token
              of endLineToken: nextIgnoredToken()
                 endParaToken: nextIgnoredToken()
                    openToken: nextUnexpectedScope()
                    wordToken: nextUnexpectedToken(unexpectedWordErr)
                         none: nextUnexpectedToken(unknownTokenErr)))
         nextClose())

!  NEXT EVEN SCOPES AND WORDS. Parse a scope that has a non-zero even number of
!  nested scopes and words. Ends of lines and paragraphs are not significant.

      nextEvenScopesAndWords :−
       (form () void:
         nextScopesAndWords()
         (if count & 1
          then syntaxError(evenScopesErr)))

!  NEXT IMAGE. Parse an IMAGE scope. It must have exactly one word, an external
!  label that is assumed to be the URL for an image.

      nextImage :−
       (form () void:
        (with
          var bool seenWord :− false

!  NEXT EXTERNAL. Try to add an external label to the scope.

          nextExternal :−
           (form () void:
            (if seenWord
             then nextUnexpectedToken(unexpectedWordErr)
             else seenWord := true
                  (if isInternal(tokenObject)
                   then syntaxError(externalExpectedErr))
                  nextExpectedToken(count, last)))

!  This is NEXT IMAGE's body. We just dispatch on tokens.

         do nextOpen()
            (while inScope()
             do (case token
                 of endLineToken: nextIgnoredToken()
                    endParaToken: nextIgnoredToken()
                       openToken: nextUnexpectedScope()
                       wordToken: nextExternal()
                            none: nextUnexpectedToken(unknownTokenErr)))
            nextClose(count)))

!  NEXT LABEL. Parse a LABEL scope. It must have exactly one word which has not
!  appeared before in another LABEL scope. Ends of lines and paragraphs are not
!  significant.

      nextLabel :−
       (form () void:
        (with
          var bool seenWord :− false

!  NEXT UNIQUE WORD TOKEN. Try to add the word to the scope.

          nextUniqueWordToken :−
           (form () void:
            (if seenWord
             then nextUnexpectedToken(unexpectedWordErr)
             else seenWord := true
                  (if isInternal(tokenObject)
                   then (if isDuplicate(tokenObject{ref word}↑.chars)
                         then syntaxError(duplicateLabelErr))
                   else syntaxError(labelErr))
                  nextExpectedToken(count, last)))

!  This is NEXT LABEL's body. We just dispatch on tokens.

         do nextOpen()
            (while inScope()
             do (case token
                 of endLineToken: nextIgnoredToken()
                    endParaToken: nextIgnoredToken()
                       openToken: nextUnexpectedScope()
                       wordToken: nextUniqueWordToken()
                            none: nextUnexpectedToken(unknownTokenErr)))
            nextClose()))

!  NEXT LINED PARAGRAPHS. Parse a scope that has a series of one or more words,
!  in which ends of lines and paragraphs are significant.

      nextLinedParagraphs :−
       (form () void:
         nextOpen()
         (while inScope()
          do (case token
              of endLineToken: nextExpectedToken(last)
                 endParaToken: nextExpectedToken(last)
                    openToken: nextLinkScope(count, last)
                    wordToken: nextExpectedToken(count, last)
                         none: nextUnexpectedToken(unknownTokenErr)))
         nextClose(count))

!  NEXT LINES. Parse a scope that contains a series of one or more lines, where
!  blanks are significant. We read lines using subtokens instead of tokens, and
!  we use the WORD BUFFER to assemble the lines as if they were words. (This is
!  one reason we have both subtokens and tokens, by the way.)

      nextLines :−
       (form () void:
        (with
          var ref object temp

!  NEXT EXPECTED LINE. Read a line. Turn it into a word and add the word to the
!  end of the enclosing scope.

          nextExpectedLine :−
           (form () void:
             empty(wordChars)
             empty(wordStyles)
             (while
               nextSubtoken()
               subtoken ≠ closeSubtoken ∧
               subtoken ≠ endFileSubtoken ∧
               subtoken ≠ endLineSubtoken
              do appendCharAndStyle(wordChars, wordStyles))
             (if subtoken = endLineSubtoken
              then nextSubtoken())
             temp := makePair(makeWord(wordChars, wordStyles), nil)
             last↑.cdr := temp
             last := temp
             count += 1)

!  This is NEXT LINES's body. Skip the first line of blanks (if it exists) that
!  follows the scope's opening, then read a series of lines. When we're done we
!  do an extra NEXT TOKEN to synchronize the token and subtoken scanners.

         do (while subtoken = blankToken
             do nextSubtoken())
            (if subtoken = endLineSubtoken
             then nextSubtoken()
             else syntaxError(endLineExpectedErr))
            (while subtoken ≠ closeSubtoken ∧ subtoken ≠ endFileSubtoken
             do nextExpectedLine())
            nextToken()
            nextClose(count)))

!  NEXT PARAGRAPHS. Parse a scope that has a series of one or more words, where
!  ends of paragraphs are significant.

      nextParagraphs :−
       (form () void:
         nextOpen()
         (while inScope()
          do (case token
              of endLineToken: nextIgnoredToken()
                 endParaToken: nextExpectedToken(last)
                    openToken: nextLinkScope(count, last)
                    wordToken: nextExpectedToken(count, last)
                         none: nextUnexpectedToken(unknownTokenErr)))
         nextClose(count))

!  NEXT INSERT. Parse an INSERT scope. It must have exactly one word which is a
!  pathname to an existing file.

      nextInsert :−
       (form () void:
        (with
          var bool seenWord :− false

!  NEXT PATH TOKEN. Try to add the word to the scope.

          nextPathToken :−
           (form () void:
            (if seenWord
             then nextUnexpectedToken(unexpectedWordErr)
             else (with opened :− open(tokenObject{ref word}↑.chars, ''r'')
                   do seenWord := true
                      (if ¬ opened.success ∨ ¬ close(opened.stream)
                       then syntaxError(pathErr))
                      nextExpectedToken(count, last))))

!  This is NEXT INSERT's body. We just dispatch on tokens.

         do nextOpen()
            (while inScope()
             do (case token
                 of endLineToken: nextIgnoredToken()
                    endParaToken: nextIgnoredToken()
                       openToken: nextUnexpectedToken(unexpectedScopeErr)
                       wordToken: nextPathToken()
                            none: nextUnexpectedToken(unknownTokenErr)))
            nextClose(count)))

!  NEXT ROWS. Like NEXT SCOPES, but every nested scope must be a ROW scope, and
!  every ROW scope must have the same number of nested scopes.

      nextRows :−
       (form () void:
        (with
          var int firstWidth :− 0

!  NEXT ROW. Parse a ROW scope. WIDTH is the number of things nested inside it.

          nextRow :−
           (proc (var int width) void:
            (with
              var ref object last :− makePair(rowAtom, nil)
              ref object     next :− makePair(last, nil)
             do nextOpen()
                (while inScope()
                 do (case token
                     of endLineToken: nextIgnoredToken()
                        endParaToken: nextIgnoredToken()
                           openToken: nextExpectedScope(width, last)
                                none: nextUnexpectedToken(unknownTokenErr)))
                nextClose(width)
                count += 1
                (past last)↑.cdr := next
                (past last) := next))

!  NEXT EXPECTED FIRST ROW. Parse the first nested ROW scope. Remember how many
!  scopes are nested inside it.

          nextExpectedFirstRow :−
           (form () void:
            (if tokenObject = rowAtom
             then nextRow(firstWidth)
             else syntaxError(rowExpectedErr)
                  unmake(nextScope())))

!  NEXT EXPECTED LATER ROW. Parse any nested ROW scope other than the first. It
!  should have the same number of scopes nested inside it as the first ROW.

          nextExpectedLaterRow :−
           (form () void:
            (with var int width :− 0
             do (if tokenObject = rowAtom
                 then nextRow(width)
                 else syntaxError(rowExpectedErr)
                      unmake(nextScope()))
                (if width ≠ firstWidth
                 then syntaxError(rowWidthErr))))

!  This is NEXT ROWS's body. Parse a scope while testing for rectangularity.

         do nextOpen()
            (while inScope() ∧ count = 0
             do (case token
                 of endLineToken: nextIgnoredToken()
                    endParaToken: nextIgnoredToken()
                       openToken: nextExpectedFirstRow()
                       wordToken: nextUnexpectedToken(rowExpectedErr)
                            none: nextUnexpectedToken(unknownTokenErr)))
            (while inScope()
             do (case token
                 of endLineToken: nextIgnoredToken()
                    endParaToken: nextIgnoredToken()
                       openToken: nextExpectedLaterRow()
                       wordToken: nextUnexpectedToken(rowExpectedErr)
                            none: nextUnexpectedToken(unknownTokenErr)))
            nextClose(count)))

!  NEXT SCOPES. Parse a scope with a series of one or more nested scopes, where
!  lines and paragraphs are not significant.

      nextScopes :−
       (form () void:
         nextOpen()
         (while inScope()
          do (case token
              of endLineToken: nextIgnoredToken()
                 endParaToken: nextIgnoredToken()
                    openToken: nextExpectedScope(count, last)
                    wordToken: nextUnexpectedToken(scopeExpectedErr)
                         none: nextUnexpectedToken(unknownTokenErr)))
         nextClose(count))

!  NEXT SCOPES AND WORDS. Parse a scope that has a series of one or more nested
!  scopes and words, where ends of lines and paragraphs are not significant.

      nextScopesAndWords :−
       (form () void:
         nextOpen()
         (while inScope()
          do (case token
              of endLineToken: nextIgnoredToken()
                 endParaToken: nextIgnoredToken()
                    openToken: nextExpectedScope(count, last)
                    wordToken: nextExpectedToken(count, last)
                         none: nextUnexpectedToken(unknownTokenErr)))
         nextClose(count))

!  NEXT TITLE. Parse a TITLE scope.

      nextTitle :−
       (form () void:
        (if seenTitle
         then nextUnknownScopesAndWords()
         else seenTitle := true
              nextWords()))

!  NEXT UNKNOWN SCOPES AND WORDS. Parse an unknown scope.

      nextUnknownScopesAndWords :−
       (form () void:
         syntaxError(unknownScopeErr)
         nextScopesAndWords())

!  NEXT WORDS. Parse a scope that has a series of words, in which ends of lines
!  and paragraphs are not significant.

      nextWords :−
       (form () void:
         nextOpen()
         (while inScope()
          do (case token
              of endLineToken: nextIgnoredToken()
                 endParaToken: nextIgnoredToken()
                    openToken: nextUnexpectedScope()
                    wordToken: nextExpectedToken(count, last)
                         none: nextUnexpectedToken(unknownTokenErr)))
         nextClose(count))

!  This is NEXT SCOPE's body. Dispatch on the name of the scope.

     do (case tokenObject{ref atom}↑.label
         of bulletLabel: nextScopesAndWords()
            centerLabel: nextLinedParagraphs()
            columnLabel: nextScopes()
           displayLabel: nextLines()
              gotoLabel: nextWords()
             imageLabel: nextImage()
            indentLabel: nextLines()
            insertLabel: nextInsert()
           itemizeLabel: nextEvenScopesAndWords()
           justifyLabel: nextParagraphs()
             labelLabel: nextLabel()
            layoutLabel: nextRows()
              leftLabel: nextParagraphs()
            narrowLabel: nextScopes()
            numberLabel: nextScopesAndWords()
             orsonLabel: nextLines()
              overLabel: nextScopes()
             rightLabel: nextParagraphs()
              ruleLabel: nextEmpty()
             tableLabel: nextRows()
             titleLabel: nextTitle()
                   none: nextUnknownScopesAndWords())
        first))

!  NEXT CLOSE. Parse the closing brace of a scope which holds COUNT significant
!  words and scopes.

  nextClose :−
   (alt
    (form () void:
     (if token = closeToken
      then nextToken()
      else syntaxError(braceExpectedErr))),
    (form (int count) void:
     (if count = 0
      then syntaxError(textExpectedErr))
     nextClose()))

!  NEXT EXPECTED SCOPE. Parse a nested scope, and add it to the end of another,
!  enclosing scope. The enclosing scope has COUNT significant words and scopes,
!  and LAST tells where it ends.

  nextExpectedScope :−
   (form (var int count, var ref object last) void:
    (with ref object next :− makePair(nextScope(), nil)
     do count += 1
        last↑.cdr := next
        last := next))

!  NEXT EXPECTED TOKEN. Add the object associated with the current token to the
!  end of an enclosing scope. The enclosing scope already has COUNT significant
!  words and scopes, and LAST tells where it ends.

  nextExpectedToken :−
   (alt
    (form (var ref object last) void:
     (with ref object next :− makePair(tokenObject, nil)
      do last↑.cdr := next
         last := next
         nextToken())),
    (form (var int count, var ref object last) void:
     (with ref object next :− makePair(tokenObject, nil)
      do count += 1
         last↑.cdr := next
         last := next
         nextToken())))

!  NEXT IGNORED TOKEN. The current token is insignificant, so skip it.

  nextIgnoredToken :−
   (form () void:
     nextToken())

!  NEXT LINK SCOPE. Like NEXT EXPECTED SCOPE but we parse a GOTO or LABEL scope
!  only. Any other scope is an error.

  nextLinkScope :−
   (form (var int count, var ref object last) void:
    (if tokenObject = gotoAtom ∨ tokenObject = labelAtom
     then nextExpectedScope(count, last)
     else syntaxError(unexpectedScopeErr)
          unmake(nextScope())))

!  NEXT OPEN. Skip the name of the current scope, and any END LINE TOKENs which
!  follow it.

  nextOpen :−
   (form () void:
     nextToken()
     (while token = endLineToken
      do nextToken()))

!  NEXT UNEXPECTED SCOPE. The next scope is syntactically unexpected. Parse it,
!  but don't add it to any enclosing scope.

  nextUnexpectedScope :−
   (form () void:
     syntaxError(unexpectedScopeErr)
     unmake(nextScope()))

!  NEXT UNEXPECTED TOKEN. The current token is syntactically unexpected. Assert
!  an error ERR and skip it. If we expected a scope then repeatedly skip tokens
!  until we find one that lets us continue.

  nextUnexpectedToken :−
   (form (int err) void:
     syntaxError(err)
     (if err = scopeExpectedErr
      then (while
             token = blankToken ∨
             token = endLineToken ∨
             token = endParaToken ∨
             token = wordToken
            do nextToken())
      else nextToken()))
)

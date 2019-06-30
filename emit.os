!
!  BRACY/EMIT. Write fragments of HTML code.
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

!  Constants.

  string ampersand           :− ''&amp;''    !  An HTML '&'.
  string blank               :− ''&nbsp;''   !  An HTML nonbreaking ' '.
  string closeQuote          :− ''&rsquo;''  !  An HTML '’'.
  string greaterThan         :− ''&gt;''     !  An HTML '>'.
  int    htmlIndentChange    :− 1            !  Indent HTML scopes this much.
  int    htmlParagraphIndent :− 5            !  Indent HTML paras this much.
  string lessThan            :− ''&lt;''     !  An HTML '<'.
  int    maxHtmlLength       :− 79           !  Max length of an HTML line.
  string openQuote           :− ''&lsquo;''  !  An HTML '‘'.
  string uparrow             :− ''&uarr;''   !  An HTML '↑'.

!  Spacers. These hold "invisible" chars and their styles.

  var [maxSourceLineLength] char spacerCharsBuffer
  var [maxSourceLineLength] set  spacerStylesBuffer

  row var char spacerChars  :− spacerCharsBuffer↓{row var char}
  row var set  spacerStyles :− spacerStylesBuffer↓{row var set}

!  Variables.

  var codeStack(maxCode) codes                           !  Current codes.
  var int                htmlRemaining :− maxHtmlLength  !  Chars left on line.
  var int                htmlIndent    :− 0              !  Current indenting.

!  AT LINE START. Test if we're about to write at the start of the current HTML
!  line.

  atLineStart :−
   (form () bool:
     htmlRemaining = maxHtmlLength)

!  EMIT BLANKS. Write COUNT blanks to OUTPUT. If COUNT is less than or equal to
!  zero, then write nothing.

  emitBlanks :−
   (proc (stream output, int count) void:
    (in count
     do write(output, ' ')))

!  EMIT CHAR AND STYLE. Write the char CH using style codes in STYLE. Write non
!  ASCII chars as &#ddd; and hope the browser likes them.

  emitCharAndStyle :−
   (form (char ch, set style) void:
    (for int code in elements(codes.style − style)
     do emitTurnOff(code))
    (for int code in elements(style − codes.style)
     do emitTurnOn(code))
    (if isAscii(ch)
     then emitAsciiCharAndStyle(ch, style)
     else write(html, ''&#%i;'': ch{int})))

!  EMIT ASCII CHAR AND STYLE. Write the ASCII char CH, using codes in STYLE. We
!  sanitize CH if it looks like HTML punctuation. Ignore QUOTING and UPARROWING
!  flags if the QUOTED style is in effect.

  emitAsciiCharAndStyle :−
   (form (char ch, set style) void:
    (if capitalCode ∊ style ∧ isLetter(ch)
     then write(html, upper(ch))
     else if quotedCode ∊ style
          then (case ch
                of ' ': write(html, blank)
                   '&': write(html, ampersand)
                   '<': write(html, lessThan)
                   '>': write(html, greaterThan)
                  none: write(html, ch))
          else (case ch
                of ' ': write(html, blank)
                   '&': write(html, ampersand)
                  '\'': write(html, (if quoting then closeQuote else '''''))
                   '<': write(html, lessThan)
                   '>': write(html, greaterThan)
                   '^': write(html, (if uparrowing then uparrow else ''^''))
                   '`': write(html, (if quoting then openQuote else ''`''))
                  none: write(html, ch))))

!  EMIT CLOSING TAG. Write TAG prefixed by a slash and surrounded by <>'s. It's
!  indented and appears on a line by itself.

  emitClosingTag :−
   (form (string tag) void:
    (if ¬ atLineStart()
     then writeln(html)
          htmlRemaining := maxHtmlLength)
    htmlIndent −= htmlIndentChange
    emitBlanks(html, htmlIndent)
    writeln(html, ''</%s>'': tag)
    htmlRemaining := maxHtmlLength)

!  EMIT FIRST INDENTED LINE. Emit HTML code to display ITEM, which is the first
!  line in an INDENTED scope. We make a copy of ITEM in the spacer buffer, then
!  write ITEM itself. Turn off all ITEM's styles when we're done. The resulting
!  HTML line may be longer than MAX HTML LENGTH.

  emitFirstIndentedLine :−
   (form (ref object item) void:
    (with
      ref object       item            :− (past item)
      var string       lineCharsStart  :− item{ref word}↑.chars
      var row set      lineStylesStart :− item{ref word}↑.styles
      var row var char spacerCharsEnd  :− spacerChars
      var row var set  spacerStylesEnd :− spacerStyles
     do (while head(lineCharsStart)
         do spacerCharsEnd↑  := head(lineCharsStart)
            spacerStylesEnd↑ := head(lineStylesStart) − grayCode
            lineCharsStart   := tail(lineCharsStart)
            lineStylesStart  := tail(lineStylesStart)
            spacerCharsEnd   := tail(spacerCharsEnd)
            spacerStylesEnd  := tail(spacerStylesEnd))
        spacerCharsEnd↑  := ' '
        spacerStylesEnd↑ := ∅
        emitWord(item, nil)
        writeln(html)
        htmlRemaining := maxHtmlLength))

!  EMIT HERALD. Write an HTML comment that includes this program's name and the
!  time at which it started running. For example:
!
!     <!-- Created by Bracy 0.1 on Sunday, June 15, 2008 at 3:55 AM CDT -->
!
!  This is essentially the example from the Orson library file TIME.OS.

  emitHerald :−
   (form () void:
    (for
      int second, int minute, int hour, string ampm,
      int date, string month, int year, string day, string zone
     in decoded(now())
     do writeln(html,
         ''<!-- Created by Bracy %s on %s, %s %i, %i at %i:%02i %s %s -->'':
         version, day, month, date, year, hour, minute, ampm, zone)))

!  EMIT LINE. Write LEFT, which points to a WORD containing an entire line.

  emitLine :−
   (form (ref object left) void:
    (with ref object left :− (past left)
     do (if head(left{ref word}↑.chars)
         then emitWord(left, nil))))

!  EMIT LONE TAG. Write TAG followed by ATTRIBUTES, surrounded by <>'s. It's on
!  a line by itself and indented. If OBJECTS is nonempty then we use ATTRIBUTES
!  as a format string to write its elements.

  emitLoneTag :−
   (alt
    (form (string tag) void:
      emitLoneTag(tag, ϵ:)),
    (form (string tag, string attributes) void:
      emitLoneTag(tag, attributes:)),
    (form (string tag, string attributes, list objects) void:
     (if ¬ atLineStart()
      then writeln(html))
     emitBlanks(html, htmlIndent)
     (if attributes = ϵ
      then writeln(html, '<' & tag & '>')
      else if isEmpty(objects)
           then writeln(html, '<' & tag & ' ' & attributes & '>')
           else writeln(html, '<' & tag & ' ' & attributes & '>', objects))
     htmlRemaining := maxHtmlLength))

!  EMIT NEXT INDENTED LINE. Emit HTML code to display ITEM, the second or later
!  line in an INDENTED scope. The HTML code may be longer than MAX HTML LENGTH.

  emitNextIndentedLine :−
   (form (ref object item) void:
    (with
      ref object       item              :− (past item)
      var string       lineCharsStart    :− item{ref word}↑.chars
      var row set      lineStylesStart   :− item{ref word}↑.styles
      var row var char spacerCharsStart  :− spacerChars
      var row var set  spacerStylesStart :− spacerStyles
      var row var char spacerCharsEnd    :− spacerChars
      var row var set  spacerStylesEnd   :− spacerStyles
      var bool         updating          :− true

!  UPDATE SPACER BUFFER. Update the spacer buffer with chars from ITEM.

      updateSpacerBuffer :−
       (form () void:
        (while head(lineCharsStart)
         do (if updating
             then (if head(lineCharsStart) ≠ ' '
                   then updating := false
                        spacerCharsEnd↑  := head(lineCharsStart)
                        spacerStylesEnd↑ := head(lineStylesStart) − grayCode)
             else spacerCharsEnd↑  := head(lineCharsStart)
                  spacerStylesEnd↑ := head(lineStylesStart) − grayCode)
            lineCharsStart  := tail(lineCharsStart)
            lineStylesStart := tail(lineStylesStart)
            spacerCharsEnd  := tail(spacerCharsEnd)
            spacerStylesEnd := tail(spacerStylesEnd))
        spacerCharsEnd↑  := ' '
        spacerStylesEnd↑ := ∅)

!  EMIT INVISIBLE. Emit invisible indenting chars from the spacer buffer, which
!  have the same color as the BACKGROUND.

      emitInvisible :−
       (form () void:
         lineCharsStart  := item{ref word}↑.chars
         lineStylesStart := item{ref word}↑.styles
         emitBlanks(html, htmlIndent)
         (if head(lineCharsStart) = ' '
          then write(html, ''<font color="#'' & background & ''">'')
               (while head(lineCharsStart) = ' '
                do emitCharAndStyle(
                    head(spacerCharsStart),
                    head(spacerStylesStart))
                   lineCharsStart    := tail(lineCharsStart)
                   lineStylesStart   := tail(lineStylesStart)
                   spacerCharsStart  := tail(spacerCharsStart)
                   spacerStylesStart := tail(spacerStylesStart))
               emitTurnOff()
               write(html, ''</font>'')))

!  EMIT VISIBLE. Emit visible HTML code for the chars of ITEM after its leading
!  blanks. Turn off all styles when we're done.

      emitVisible :−
       (form () void:
        (while head(lineCharsStart)
         do emitCharAndStyle(head(lineCharsStart), head(lineStylesStart))
            lineCharsStart  := tail(lineCharsStart)
            lineStylesStart := tail(lineStylesStart))
        emitTurnOff()
        htmlRemaining := maxHtmlLength
        writeln(html))

!  This is EMIT NEXT INDENTED LINE's body. If ITEM is empty, then emit nothing.

     do (if lineCharsStart↑
         then updateSpacerBuffer()
              emitInvisible()
              emitVisible())))

!  EMIT OPENING TAG. Write TAG followed by ATTRIBUTES, surrounded by <>'s. It's
!  indented and appears on a line by itself. If OBJECTS is nonempty then we use
!  ATTRIBUTES as a format string for the members of the list.

  emitOpeningTag :−
   (form (string tag, string attributes, list objects) void:
     (if ¬ atLineStart()
      then writeln(html))
     emitBlanks(html, htmlIndent)
     (if attributes = ϵ
      then writeln(html, '<' & tag & '>')
      else if isEmpty(objects)
           then writeln(html, '<' & tag & ' ' & attributes & '>')
           else writeln(html, '<' & tag & ' ' & attributes & '>', objects))
     htmlIndent += htmlIndentChange
     htmlRemaining := maxHtmlLength)

!  EMIT PARAGRAPH INDENT. Write a line break and a series of nonbreaking spaces
!  to start a new paragraph.

  emitParagraphIndent :−
   (form () void:
    (with int temp :− htmlParagraphIndent × length(blank)
     do emitLoneTag(''br'')
        emitBlanks(html, htmlIndent)
        (in htmlParagraphIndent
         do write(html, blank))
        htmlRemaining := maxHtmlLength − htmlIndent − temp))

!  EMIT STRINGS. Write a list of indented strings to HTML.

  emitStrings :−
   (form (list strings) void:
    (for obj string in elements(strings)
     do emitBlanks(html, htmlIndent)
        writeln(html, string))
    htmlRemaining := maxHtmlLength)

!  EMIT TITLE WORD. Like EMIT WORD, except that it writes the word ITEM without
!  embedded markup. We write words left-justified and filled.

  emitTitleWord :−
   (proc (ref object item) void:
    (with
      string chars  :− item{ref word}↑.chars
      int    needed :− count(chars)
     do (if atLineStart()
         then emitBlanks(html, htmlIndent)
              write(html, chars)
              htmlRemaining := maxHtmlLength − htmlIndent − needed
         else if needed + 1 > htmlRemaining
              then writeln(html)
                   emitBlanks(html, htmlIndent)
                   write(html, chars)
                   htmlRemaining := maxHtmlLength − htmlIndent − needed
              else write(html, ' ')
                   write(html, chars)
                   htmlRemaining −= needed + 1)))

!  EMIT TURN OFF. Write HTML tags that turn off the style code CODE, and record
!  that we did it in the code stack CODES. We may need to write several tags to
!  accomplish this, since other tags might be in the way. If CODE is not given,
!  then write tags that turn off all currently active style codes.

  emitTurnOff :−
   (alt
    (form () void:
     (while ¬ isEmpty(codes)
      do write(html, codeToClose[top(codes)])
         pop(codes))),
    (form (int code) void:
     (while
      (if isEmpty(codes)
       then false
       else write(html, codeToClose[top(codes)])
            top(codes) ≠ code also pop(codes)))))

!  EMIT TURN ON. Write an HTML tag that turns on the style code CODE and record
!  that we did it in the code stack CODES.

  emitTurnOn :−
   (form (int code) void:
     write(html, codeToOpen[code])
     push(codes, code))

!  EMIT WORD. Write the word LEFT with embedded style tags. LEFT is followed by
!  the word RIGHT. We need RIGHT to tell which styles should be turned off when
!  we get done writing LEFT. If LEFT isn't followed by another word, then RIGHT
!  is NIL.

  emitWord :−
   (proc (ref object left, ref object right) void:
    (with
      var string  leftChars  :− left{ref word}↑.chars
      var row set leftStyles :− left{ref word}↑.styles
      int         needed     :− wordLength(left, right)

!  EMIT CHARS AND STYLES. Write a word whose chars are in LEFT CHARS, and whose
!  corresponding style code sets are in LEFT STYLES.

      emitCharsAndStyles :−
       (proc () void:
        (while head(leftChars)
         do emitCharAndStyle(head(leftChars), head(leftStyles))
            leftChars  := tail(leftChars)
            leftStyles := tail(leftStyles))
        (if isWord(right)
         then (for int code
               in elements(codes.style − right{ref word}↑.styles↑)
               do emitTurnOff(code))
         else emitTurnOff()))

!  Write the word LEFT, left-justified and filled.

     do (if atLineStart()
         then emitBlanks(html, htmlIndent)
              emitCharsAndStyles()
              htmlRemaining := maxHtmlLength − htmlIndent − needed
         else if needed + 1 > htmlRemaining
              then writeln(html)
                   emitBlanks(html, htmlIndent)
                   emitCharsAndStyles()
                   htmlRemaining := maxHtmlLength − htmlIndent − needed
              else write(html, ' ')
                   emitCharsAndStyles()
                   htmlRemaining −= needed + 1)))

!  WORD LENGTH. Return the number of chars WRITE WORD will need to write a word
!  LEFT, considering style tags and sanitized chars. We simulate WRITE WORD via
!  a local CODES stack, incrementing COUNT for each char that would be written.
!  LEFT is followed by the word RIGHT (or NIL).

  wordLength :−
   (proc (ref object left, ref object right) int:
    (with
      var codeStack(maxCode) codes      :− (past codes)
      var int                count      :− 0
      var string             leftChars  :− left{ref word}↑.chars
      var row set            leftStyles :− left{ref word}↑.styles

      inj ampersandΔ   :− length(ampersand)
      inj blankΔ       :− length(blank)
      inj closeQuoteΔ  :− length(closeQuote)
      inj greaterThanΔ :− length(greaterThan)
      inj lessThanΔ    :− length(lessThan)
      inj openQuoteΔ   :− length(openQuote)
      inj uparrowΔ     :− length(uparrow)

!  COUNT TURN OFF. Simulate EMIT TURN OFF from WRITE WORD above.

      countTurnOn :−
       (form (int code) void:
         count += length(codeToOpen[code]) + 2
         push(codes, code))

!  COUNT TURN ON. Simulate EMIT TURN ON from WRITE WORD above.

      countTurnOff :−
       (alt
        (form () void:
         (while ¬ isEmpty(codes)
          do count += length(codeToClose[top(codes)]) + 3
             pop(codes))),
        (form (int code) void:
         (while
          (if isEmpty(codes)
           then false
           else count += length(codeToClose[top(codes)]) + 3
                top(codes) ≠ code also pop(codes)))))

!  INT LENGTH. Return how many decimal digits are needed to write a nonnegative
!  NUMBER.

      intLength :−
       (form (int number) int:
        (with
          var int temp  :− number
          var int count :− temp = 0
         do (while temp > 0
             do count += 1
                temp /= 10)
            count))

!  Simulate EMIT CHARS AND STYLES from WRITE WORD above. Do nothing special for
!  CAPITAL CODE because it doesn't affect the number of chars written.

     do (while head(leftChars)
         do (for int code in elements(codes.style − head(leftStyles))
             do countTurnOff(code))
            (for int code in elements(head(leftStyles) − codes.style)
             do countTurnOn(code))
            (if isAscii(head(leftChars))
             then (if quotedCode ∊ head(leftStyles)
                   then (case head(leftChars)
                         of ' ': count += blankΔ
                            '&': count += ampersandΔ
                            '<': count += lessThanΔ
                            '>': count += greaterThanΔ
                           none: count += 1)
                   else (case head(leftChars)
                         of ' ': count += blankΔ
                            '&': count += ampersandΔ
                           '\'': count += (if quoting then closeQuoteΔ else 1)
                            '<': count += lessThanΔ
                            '>': count += greaterThanΔ
                            '^': count += (if uparrowing then uparrowΔ else 1)
                            '`': count += (if quoting then openQuoteΔ else 1)
                           none: count += 1))
             else count += intLength(head(leftChars)) + 3)
            leftChars  := tail(leftChars)
            leftStyles := tail(leftStyles))
        (if isWord(right)
         then (for int code
               in elements(codes.style − right{ref word}↑.styles[0])
               do countTurnOff(code))
         else countTurnOff())
        count))
)

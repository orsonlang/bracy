!
!  BRACX. Cross reference generator for BRACY source files.
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

(load ''lib.ascii'')     !  Operations on ASCII characters.
(load ''lib.buffer'')    !  Fixed length linear queues.
(load ''lib.command'')   !  Process command line arguments.
(load ''lib.convert'')   !  Convert a string to an integer or a real.
(load ''lib.dynamic'')   !  Dynamic memory.
(load ''lib.fail'')      !  Terminate a program with an error message.
(load ''lib.headtail'')  !  Traverse arrays using pointers.
(load ''lib.string'')    !  Operations on strings.
(load ''lib.ubst'')      !  Unbalanced binary search trees.
(load ''lib.vlq'')       !  Variable length queues.

(prog

!  Constants that represent source tokens.

  foj makeToken  :− enum()

  int eofToken   :− makeToken()  !  End of file.
  int gotoToken  :− makeToken()  !  GOTO scope name.
  int labelToken :− makeToken()  !  LABEL scope name.
  int openToken  :− makeToken()  !  Open brace.
  int otherToken :− makeToken()  !  A token we don't care about.
  int wordToken  :− makeToken()  !  A word.

!  Other constants.

  string bracySuffix         :− ''.by''  !  Bracy source files end in this.
  int    digitsPerLineNumber :− 5        !  Line numbers are this long.
  char   eofChar             :− '\#01'   !  Indicates end of a source file.
  int    maxColumnsPerLabel  :− 1024     !  Labels can be this long.
  int    maxSourceLineLength :− 1024     !  Source lines can be this long.
  string version             :− ''0.4''  !  Version number.

!  SET. A set whose members may be IN GOTO and IN LABEL, or a set of such sets.
!  We represent sets as INTs: an element J is in a set iff bit number J is 1.

  set      :− int
  setOrInt :− int

  int inGoto  :− 0
  int inLabel :− 1
  set ∅       :− 0

!  "∊". Test if ELEMENT is in the set ELEMENTS.

  "∊" :−
   (form (setOrInt element, set elements) bool:
    (1{int} << element) & elements ≠ 0)

!  "∪=". Add NEW ELEMENTS to the set OLD ELEMENTS.

  "∪=" :−
   (form (var set oldElements, set newElements) void:
     oldElements |= newElements)

!  MAKE SET. Return a set containing ELEMENT, or the elements in ELEMENTS.

  makeSet :−
   (alt
    (form (int element) set:
      1{int} << element),
    (form (list elements) set:
     (if isEmpty(elements)
      then 0
      else makeSet(car(elements)) | makeSet(cdr(elements)))))

!  SET CHAR. Return a char that represents the set WHERE.
!
!    setChar(∅)                           ⇒  '?'
!    setChar(makeSet(inGoto))             ⇒  'G'
!    setChar(makeSet(inLabel))            ⇒  'L'
!    setChar(makeSet(: inGoto, inLabel))  ⇒  'B'

  setChar :−
   (form (set where) char:
     ''?GLB''[where])

!  PLACE. Represent the line NUMBER where a label appears in SOURCE. WHERE is a
!  set that says if the label appears in a GOTO scope, a LABEL scope, or both.

  place :−
   (tuple
     int     number,
     var set where)

!  MAKE PLACE. Return a new PLACE that contains NUMBER and WHERE.

  makePlace :−
   (form (int number, set where) place:
    (with var place self
     do self.number := number
        self.where  := where
        self))

!  Command line options and their defaults.

  var int  columnsPerLabel :− 21     !  Default for -c option.
  var bool defaultWheres   :− true   !  Are -b, -g, and -l defaults in effect?
  var int  numbersPerLine  :− 8      !  Default for -n option.
  var bool showExternals   :− false  !  Default for -e option.
  var bool writingLines    :− false  !  Default for -p option.

!  Default for the -b, -g, and -l options.

  var set wheres :−
   makeSet(:
    makeSet(inGoto),
    makeSet(inLabel),
    makeSet(: inGoto, inLabel))

!  Variables.

  var char                     ch               !  Char read from SOURCE.
  var int                      lineNumber :− 0  !  Line number in SOURCE.
  var ubst(string, vlq(place)) root             !  Store label PLACEs here.
  var stream                   source           !  Bracy source file.
  var int                      token            !  Token read from SOURCE.

!  Buffers, used to accumulate LINEs and WORDs read from SOURCE.

  var buffer(maxSourceLineLength, char)  line
  var buffer(maxSourceLineLength, char0) word

!  NEXT CHAR. Copy the next char from LINE into CH. If LINE is empty, then read
!  the next line before we do it.

  nextChar :−
   (proc () void:
    (if atEnd(line)
     then nextLine())
    ch := start(line)
    advance(line))

!  NEXT LINE. Read the next line from SOURCE into LINE.  If L is the ASCII line
!  feed char, and R is the ASCII return char,  then a line may be terminated by
!  L, R, L R, or R L. The final line in SOURCE may be terminated by an EOS.

  nextLine :−
   (proc () void:
    (with
      var char temp

!  NEXT END FILE. Terminate LINE and stop reading.

      nextEndFile :−
       (form () bool:
        (if atEnd(line)
         then append(line, eofChar))
        append(line, '\0')
        false)

!  NEXT LINEFEED. Terminate LINE and stop reading.

      nextLinefeed :−
       (form () bool:
         temp := read(source)
         (if temp ≠ eos ∧ temp ≠ '\R'
          then unread(source, temp))
         append(line, '\0')
         false)

!  NEXT RETURN. Terminate LINE and stop reading.

      nextReturn :−
       (form () bool:
         temp := read(source)
         (if temp ≠ eos ∧ temp{char} ≠ '\N'
          then unread(source, temp))
         append(line, '\0')
         false)

!  NEXT OTHER. Read an ordinary char into LINE and keep reading.

      nextOther :−
       (form () bool:
        (if ¬ isFull(line)
         then append(line, temp))
        true)

!  WRITE LINE. If we're WRITING LINES, and LINE doesn't have an EOF CHAR as its
!  first char, then write it.

      writeLine :−
       (form () void:
        (if writingLines ∧ start(line) ≠ eofChar
         then write(''%0*i '': digitsPerLineNumber, lineNumber)
              (for char ch in elements(line)
               do write(ch))
              writeln()))

!  This is NEXT LINE's body. We write LINE if WRITING LINES says we should.

     do empty(line)
        lineNumber += 1
        (while
          temp := read(source)
          (case temp
           of eos{char}: nextEndFile()
                   '\N': nextLinefeed()
                   '\R': nextReturn()
                   none: nextOther()))
        writeLine()))

!  NEXT TOKEN. Set TOKEN to the next token read from SOURCE. If TOKEN is a WORD
!  TOKEN then WORD also holds the chars of that word.

  nextToken :−
   (proc () void:
    (with

!  NEXT END FILE. Read a token that represents the end of SOURCE. We don't read
!  the next char after the token, because there isn't one. Stop reading tokens.

      nextEndFile :−
       (form () bool:
         token := eofToken
         false)

!  NEXT OPEN. Read a token that represents an open brace, and then stop reading
!  tokens.

      nextOpen :−
       (form () bool:
         token := openToken
         nextChar()
         false)

!  NEXT WORD. Read a token that represents a word, then stop reading tokens.

      nextWord :−
       (form () bool:
        (with

!  IS WORD CHAR. Test if CH can appear in a word.

          isWordChar :−
           (form (char ch) bool:
            (case ch
             of ' ': false
                '{': false
                '}': false
            eofChar: false
               none: true))

!  This is NEXT WORD's body. Read a word into WORD and then test if it's one of
!  the scope names we care about. Stop reading tokens.

     do empty(word)
        (while isWordChar(ch)
         do (if ch = '\\'
             then nextChar()
                  (if ch ≠ '\0'
                   then append(word, ch))
             else append(word, ch))
            nextChar())
        (if word{string} = ''goto''
         then token := gotoToken
         else if word{string} = ''label''
              then token := labelToken
              else token := wordToken)
        false))

!  NEXT OTHER. Read a token that represents some char we don't care about, then
!  continue reading tokens.

      nextOther :−
       (form () bool:
         token := otherToken
         nextChar()
         true)

!  This is NEXT TOKEN's body. Keep reading tokens until we get something that's
!  not an OTHER TOKEN.

     do (while
         (case ch
          of eofChar:        nextEndFile()
             '{':            nextOpen()
             '\0', ' ', '}': nextOther()
             none:           nextWord()))))

!  NEXT SOURCE. Parse a series of TOKENs. This is barely smart enough to detect
!  the start of a GOTO or LABEL scope, using the following grammar.
!
!    <goto scope>   ::=  { goto <label>
!    <label scope>  ::=  { label <label>
!    <label>        ::=  <label word> | <style> <label>
!    <label word>   ::=  <letter> | <label word> <letter or digit>
!    <style>        ::=  { <style word>
!    <style word>   ::=  <style char> | <style word> <style char>
!    <style char>   ::=  b | c | i | g | q | s | t | u | + | -

  nextSource :−
   (form () void:
     nextLine()
     nextChar()
     nextToken()
     (while token ≠ eofToken
      do (if token = openToken
          then nextToken()
               (if token = gotoToken
                then nextToken()
                     nextLabel(makeSet(inGoto))
                else if token = labelToken
                     then nextToken()
                          nextLabel(makeSet(inLabel))
                     else nextToken())
          else nextToken())))

!  NEXT LABEL. Parse a label in a GOTO or LABEL scope. WHERE tells what kind of
!  scope it appears in.

  nextLabel :−
   (proc (set where) void:
    (with

!  IS LABEL. Test if WORD contains a label, whose first char is an ASCII letter
!  and whose remaining chars are ASCII letters or digits.

      isLabel :−
       (form () bool:
        (with var string word :− (past word){string}
         do (if isLetter(head(word))
             then word := tail(word)
                  (while isLetter(head(word)) ∨ isDigit(head(word))
                   do word := tail(word))
                  head(word) = '\0'
             else false)))

!  IS STYLE. Test if WORD is the name of a style. It must be a series of one or
!  more chars that appear below in the CASE clause.

      isStyle :−
       (form () bool:
        (with var string word :− (past word){string}
         do (while isStyleChar(head(word))
             do word := tail(word))
            head(word) = '\0'))

!  IS STYLE CHAR. Test if CH can appear in the name of a style.

      isStyleChar :−
       (form (char ch) bool:
        (with char ch :− (past ch)
         do (case ch
             of '+': true
                '-': true
                'b': true
                'c': true
                'i': true
                'g': true
                'u': true
                'q': true
                's': true
                't': true
               none: false)))

!  This is NEXT LABEL's body. We parse a series of zero or more style prefixes,
!  followed by a word that can be a label. If we find the label, then we record
!  it by calling RECORD LABEL.

      do (while
          (if token = wordToken
           then (if showExternals ∨ isLabel()
                 then recordLabel(where))
                nextToken()
                false
           else if token = openToken
                then nextToken()
                     (if token = wordToken ∧ isStyle()
                      then nextToken()
                           true
                      else false)
                else nextToken()
                     false))))

!  RECORD LABEL. Record that the chars of WORD appeared as a label described by
!  WHERE.

  recordLabel :−
   (proc (set where) void:
    (for bool found, var string key, var vlq(place) value
     in got(root, comp, word{string})
     do (if ¬ found
         then key := copy(word{string})
              init(value))
        enqueue(value, makePlace(lineNumber, where))))

!  WRITE TREE. Write the KEYs and PLACEs in ROOT.

  writeTree :−
   (form () void:
    (with

!  PLACES WHERE. Return a SET describing where a label appears in PLACES.

      placesWhere :−
       (form (vlq(place) places) set:
        (with var set where :− ∅
         do (for place place in elements(places)
             do where ∪= place.where)
            where))

!  WRITE LABEL PREFIX. Write a prefix summarizing WHERE a label appears.

      writeLabelPrefix :−
       (form (set where) void:
         write(setChar(where))
         write(' '))

!  WRITE LABEL. Write at most the first LABEL LENGTH chars of LABEL. If there's
!  space left over, then fill it with blanks. LABEL contains only ASCII letters
!  and digits.

      writeLabel :−
       (form (string label) void:
        (with var int count :− columnsPerLabel
         do (for breaker() break, char0 ch in elements(label)
             do (if count > 0
                 then write(ch)
                      count −= 1
                 else break()))
            (in count
             do write(' '))))

!  WRITE PLACES. Write the line NUMBERs in PLACES, separated by blanks, using a
!  series of one or more lines. Each NUMBER is prefixed by a char that says how
!  it appears.

      writePlaces :−
       (form (vlq(place) places) void:
        (with var int count :− numbersPerLine
         do (for place place in elements(places)
             do (if count = 0
                 then writeln()
                      (in columnsPerLabel + 2
                       do write(' '))
                      count := numbersPerLine)
                write(' ')
                write(setChar(place.where))
                write(''%0*i'': digitsPerLineNumber, place.number)
                count −= 1)
            writeln()))

!  This is WRITE TREE's body. Visit each LABEL and its queue of PLACES in ROOT.
!  If it appears as described by WHERES, then show it in the table.

     do (for string label, vlq(place) places in pairs(root)
         do (with set where :− placesWhere(places)
             do (if where ∊ wheres
                 then writeLabelPrefix(where)
                      writeLabel(label)
                      writePlaces(places))))))

!  READ FILE. Read a Bracy source file whose pathname is PATH.

  readFile :−
   (form (string path) void:
    (if path↑ = '-'
     then fail(''Illegal pathname '%s'.'': path))
    (if ¬ isEnd(path, bracySuffix)
     then fail(''Unexpected suffix in '%s'.'': path))
    (if ¬ open(source, path, ''r'')
     then fail(''Cannot open '%s'.'': path))
    nextSource()
    (if writingLines
     then write('\F'))
    (if ¬ close(source)
     then fail(''Cannot close '%s'.'': path)))

!  SET COLUMNS PER LABEL. Set (at most) how many columns may be used to write a
!  label in the cross reference table.

  setColumnsPerLabel :−
   (form (string digits) void:
    (for bool ok, int count in convert(int, digits)
     do (if ok
         then (if 1 ≤ count ≤ maxColumnsPerLabel
               then columnsPerLabel := count
               else fail(''-c must be between 1 and %i.'': maxColumnsPerLabel))
         else fail(''-c must be an integer.''))))

!  SET EXTERNAL. In the cross reference table, show external labels.

  setExternal :−
   (form () void:
     showExternals := true)

!  SET NUMBERS PER LINE. Set (at most) how many places can appear on every line
!  of the cross reference table.

  setNumbersPerLine :−
   (form (string digits) void:
    (for bool ok, int count in convert(int, digits)
     do (if ok
         then (if count > 0
               then numbersPerLine := count
               else fail(''-n must be greater than 0.''))
         else fail(''-n must be an integer greater than 0.''))))

!  SET WHERES. In the cross reference table, show labels described in SCOPES.

  setWheres :−
   (form (list scopes) void:
    (if defaultWheres
     then defaultWheres := false
          wheres := ∅)
    wheres ∪= makeSet(makeSet(scopes)))

!  SET WRITING LINES. Write Bracy source files before writing a cross reference
!  table.

  setWritingLines :−
   (form () void:
     writingLines := true)

!  WRITE VERSION. Write the name of this program and its version number.

  writeVersion :−
   (form () void:
     writeln(''Bracy cross referencer, version '' & version & ''.''))

!  MAIN. Main program. Visit each option from the command line, and do whatever
!  it says. Then write the cross reference table.

  main :−
   (init(root)
    (for char option, string value in command(''beglpv'', '' cn'')
     do (case option
         of ' ': readFile(value)
            'b': setWheres(: inGoto, inLabel)
            'c': setColumnsPerLabel(value)
            'e': setExternal()
            'g': setWheres(: inGoto)
            'l': setWheres(: inLabel)
            'n': setNumbersPerLine(value)
            'p': setWritingLines()
            'v': writeVersion()))
    writeTree())
)

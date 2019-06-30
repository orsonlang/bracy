!
!  BRACY/TRANSLATE. Translate internal form to HTML.
!
!  Copyright © 2017 James B. Moen.
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

!  TRANSLATE. Translate a chain of SCOPES to HTML.

  translate :−
   (proc (ref object scopes) void:
    (with var ref object scopes :− (past scopes)

!  TRANSLATE CLOTHED. Translate SCOPES to a complete HTML document, including a
!  HERALD comment, HTML tags, HEAD tags, and BODY tags.

      translateClothed :−
       (form () void:
         emitHerald()
         (if scopes ≠ nil
          then (in tags(''html'')
                do (in tags(''head'')
                    do (if isScope(scopes↑.car) ∧ scopes↑.car↑.car = titleAtom
                        then translateTitle(scopes↑.car)
                             scopes := scopes↑.cdr)
                       translateStyle())
                   (in tags(''body'', ''bgcolor="#'' & background & '"')
                    do translateNaked()))))

!  TRANSLATE NAKED. Translate SCOPES without those tags, so we can INSERT it in
!  a complete HTML document later.

      translateNaked :−
       (form () void:
        (while scopes ≠ nil
         do translateScope(scopes↑.car)
            scopes := scopes↑.cdr))

!  Translate SCOPES, then send them back to the heap.

     do (if nakeding
         then translateNaked()
         else translateClothed()))
    unmake(scopes))

!  FIXED. Wrapper. Render text produced by BODY in a fixed width font.

  fixed :−
   (form () foj:
    (form (form () obj body) obj:
      emitOpeningTag(''tt'', ϵ:)
      body()
      emitClosingTag(''tt'')))

!  IS INDENTED. Here SCOPE is a scope containing only words. Test if any of the
!  words (but not the first word) begins with a blank.

  isIndented :−
   (proc (ref object scope) bool:
    (with
      var bool       found :− false
      var ref object items :− scope↑.cdr
     do (if items ≠ nil
         then items := items↑.cdr
              (while ¬ found ∧ items ≠ nil
               do found := items↑.car{ref word}↑.chars↑ = ' '
                  items := items↑.cdr))
        found))

!  ITEMS. Iterator. Call BODY on each object in the chain of pairs ITEMS, or on
!  each object and the object following it (NIL if there isn't one) in ITEMS.

  items :−
   (form (ref object items) foj:
    (alt
     (form (form (ref object) obj body) obj:
      (with var ref object items :− (past items)
       do (while items ≠ nil
           do (with item :− items↑.car
               do body(item))
              items := items↑.cdr))),
     (form (form (ref object, ref object) obj body) obj:
      (with var ref object items :− (past items)
       do (while items ≠ nil
           do (with
                left  :− items↑.car
                right :− (if items↑.cdr = nil then nil else items↑.cdr↑.car)
               do body(left, right))
              items := items↑.cdr)))))

!  TAGS. Wrapper. Write TAG (followed by optional ATTRIBUTES) enclosed in <>'s.
!  If OBJECTS is present, then ATTRIBUTES is a format string for its members.

  tags :−
   (alt
    (form (string tag) foj:
      tags(tag, ϵ:)),
    (form (string tag, string attributes) foj:
      tags(tag, attributes:)),
    (form (string tag, string attributes, list objects) foj:
     (form (form () obj body) obj:
       emitOpeningTag(tag, attributes, objects)
       body()
       emitClosingTag(tag))))

!  TRANSLATE SCOPE. Translate SCOPE to HTML.

  translateScope :−
   (proc (ref object scope) void:
    (with

!  TRANSLATE BULLET. Translate a BULLET scope to HTML.

      translateBullet :−
       (form () void:
        (in tags(''ul'')
         do (for ref object item in items(scope↑.cdr)
             do emitLoneTag(''li'')
                (if isScope(item)
                 then translateScope(item)
                 else if isWord(item)
                      then translateWord(item)
                      else fail(''Bullet error.'')))))

!  TRANSLATE CENTER. Translate a CENTER scope to HTML.

      translateCenter :−
       (form () void:
        (in tags(''p'', ''align="center"'')
         do (for ref object left, ref object right in items(scope↑.cdr)
             do (if isWord(left)
                 then emitWord(left, right)
                 else if isScope(left)
                      then translateScope(left)
                      else if left = endLineAtom
                           then emitLoneTag(''br /'')
                           else if left = endParaAtom
                                then emitLoneTag(''br /'')
                                     emitLoneTag(''br /'')
                                else fail(''Center error.'')))))

!  TRANSLATE COLUMN. Translate a COLUMN scope to HTML.

      translateColumn :−
       (form () void:
        (for ref object item in items(scope↑.cdr)
         do translateScope(item)))

!  TRANSLATE DISPLAY. Translate a DISPLAY scope to HTML.

      translateDisplay :−
       (form () void:
        (in tags(''p'')
         do (for ref object left, ref object right in items(scope↑.cdr)
             do emitLine(left)
                (if right ≠ nil
                 then emitLoneTag(''br /'')))))

!  TRANSLATE GOTO. Translate a GOTO scope to HTML.

      translateGoto :−
       (form () void:
        (with

!  TRANSLATING GOTO. Do all the work for TRANSLATE GOTO.

          translatingGoto :−
           (proc (ref object words, string prefix, string chars) void:
            (in tags(''a'', ''href="%s%s"'': prefix, chars)
             do (if words↑.cdr = nil
                 then (for ref object left, ref object right
                       in items(words)
                       do emitWord(left, right))
                 else (for ref object left, ref object right
                       in items(words↑.cdr)
                       do emitWord(left, right)))))

!  This is TRANSLATE GOTO's body. If the first word looks like a URL, then emit
!  an external link, otherwise emit an internal link (with a "#" prefix).

         do (with
              ref object words :− scope↑.cdr
              string     chars :− words↑.car{ref word}↑.chars
             do (if isInternal(words↑.car)
                 then translatingGoto(words, ''#'', chars)
                 else translatingGoto(words, ϵ, chars)))))

!  TRANSLATE IMAGE. Translate an IMAGE scope to HTML.

  translateImage :−
   (form () void:
    (with string opts :− ''border="0" src="%s"''
     do (in tags(''p'', ''align="center"'')
         do emitLoneTag(''img'', opts: scope↑.cdr↑.car{ref word}↑.chars))))

!  TRANSLATE INDENT. Translate an INDENT scope to HTML.

  translateIndent :−
   (form () void:
    (in tags(''p'')
     do (if proofing ∧ isIndented(scope)
         then (in fixed()
               do (for ref object left, ref object right in items(scope↑.cdr)
                   do emitLine(left)
                      (if right ≠ nil
                       then emitLoneTag(''br /''))))
         else (with var ref object items :− scope↑.cdr
               do (if items ≠ nil
                   then emitFirstIndentedLine(items↑.car)
                        items := items↑.cdr
                        (while items ≠ nil
                         do emitLoneTag(''br /'')
                            emitNextIndentedLine(items↑.car)
                            items := items↑.cdr))))))

!  TRANSLATE INSERT. Translate an INSERT scope to HTML. We FAIL if PATH doesn't
!  point to a file now, even though it did when we were parsing.

      translateInsert :−
       (form () void:
        (with
          string path :− scope↑.cdr↑.car{ref word}↑.chars
          opened :− open(path, ''r'')
         do (if opened.success
             then (with var int ch :− read(opened.stream)
                   do (while ch ≠ eos
                       do emitBlanks(html, htmlIndent)
                          (while ch ≠ eol ∧ ch ≠ eos
                           do write(html, ch{char})
                              ch := read(opened.stream))
                          (if ch = eol
                           then ch := read(opened.stream))
                          writeln(html))
                      (if ¬ close(opened.stream)
                       then fail(''Cannot close '%s''': path)))
             else fail(''Cannot open '%s''': path))))

!  TRANSLATE ITEMIZE. Translate an ITEMIZE scope to HTML.

      translateItemize :−
       (form () void:
        (in tags(''dl'', ''compact="compact"'')
         do (with var ref object items :− scope↑.cdr
             do (while items ≠ nil
                 do (in tags(''dt'')
                     do (with ref object item :− items↑.car
                         do (if isScope(item)
                             then translateScope(item)
                             else if isWord(item)
                                  then translateWord(item)
                                  else fail(''Itemize error''))))
                    items := items↑.cdr
                    (in tags(''dd'')
                     do (with ref object item :− items↑.car
                         do (if isScope(item)
                             then translateScope(item)
                             else if isWord(item)
                                  then translateWord(item)
                                  else fail(''Itemize error''))))
                    items := items↑.cdr))))

!  TRANSLATE JUSTIFY. Translate a JUSTIFY scope to HTML.

      translateJustify :−
       (form () void:
        (in tags(''p'', ''align="justify"'')
         do (for ref object left, ref object right in items(scope↑.cdr)
             do (if isWord(left)
                 then emitWord(left, right)
                 else if isScope(left)
                      then translateScope(left)
                      else if left = endParaAtom
                           then emitParagraphIndent()
                           else fail(''Justify error.'')))))

!  TRANSLATE LABEL. Translate a LABEL scope to HTML.

      translateLabel :−
       (form () void:
        (in tags(''a'', ''name="%s"'': scope↑.cdr↑.car{ref word}↑.chars)
         do skip))

!  TRANSLATE LAYOUT. Translate a LAYOUT scope to HTML.

      translateLayout :−
       (form () void:
        (in tags(''table'', ''cellpadding="0" cellspacing="0"'')
         do (for ref object scope in items(scope↑.cdr)
             do (in tags(''tr'', ''valign="top"'')
                 do (for ref object item in items(scope↑.cdr)
                     do (in tags(''td'')
                         do (if isScope(item)
                             then translateScope(item)
                             else if isWord(item)
                                  then translateWord(item)
                                  else fail(''Layout error.''))))))))

!  TRANSLATE LEFT. Translate a LEFT scope to HTML.

      translateLeft :−
       (form () void:
        (in tags(''p'')
         do (for ref object left, ref object right in items(scope↑.cdr)
             do (if isWord(left)
                 then emitWord(left, right)
                 else if isScope(left)
                      then translateScope(left)
                      else if left = endParaAtom
                           then emitParagraphIndent()
                           else fail(''Left error.'')))))

!  TRANSLATE NARROW. Translate a NARROW scope to HTML.

      translateNarrow :−
       (form () void:
        (in tags(''blockquote'')
         do (for ref object item in items(scope↑.cdr)
             do translateScope(item))))

!  TRANSLATE NUMBER. Translate a NUMBER scope to HTML.

      translateNumber :−
       (form () void:
        (in tags(''ol'')
         do (for ref object item in items(scope↑.cdr)
             do emitLoneTag(''li'')
                (if isScope(item)
                 then translateScope(item)
                 else if isWord(item)
                      then translateWord(item)
                      else fail(''Number error'')))))

!  TRANSLATE ORSON. Translate an ORSON scope to HTML.

  translateOrson :−
   (form () void:
    (in tags(''p'')
     do (if proofing ∧ isIndented(scope)
         then (in fixed()
               do (for ref object left, ref object right in items(scope↑.cdr)
                   do orsonize(left)
                      emitLine(left)
                      (if right ≠ nil
                       then emitLoneTag(''br /''))))
         else (with var ref object items :− scope↑.cdr
               do (if items ≠ nil
                   then orsonize(items↑.car)
                        emitFirstIndentedLine(items↑.car)
                        items := items↑.cdr
                        (while items ≠ nil
                         do emitLoneTag(''br /'')
                            orsonize(items↑.car)
                            emitNextIndentedLine(items↑.car)
                            items := items↑.cdr))))))

!  TRANSLATE OVER. Translate an OVER scope to HTML.

  translateOver :−
   (form () void:
    (with
      string         opts   :− ''cellpadding="0" cellspacing="0"''
      var ref object scopes :− scope↑.cdr
     do (in tags(''table'', opts)
         do (in tags(''tr'')
             do (in tags(''td'')
                 do (while scopes ≠ nil
                     do (in tags(''table'', ''align="center" '' & opts)
                         do (in tags(''tr'', ''valign="bottom"'')
                             do (in tags(''td'')
                                 do translateScope(scopes↑.car))
                                (in tags(''td'')
                                 do skip))
                            scopes := scopes↑.cdr
                            (if scopes ≠ nil
                             then (in tags(''tr'', ''valign="bottom"'')
                                   do (in tags(''td'')
                                       do emitLoneTag(''hr /''))
                                      (in tags(''td'')
                                       do translateScope(scopes↑.car)))
                                  scopes := scopes↑.cdr
                                  (if scopes ≠ nil ∧ scopes↑.cdr = nil
                                   then (in tags(''tr'', ''valign="bottom"'')
                                         do (in tags(''td'')
                                             do translateScope(scopes↑.car)))
                                        scopes := nil)))))))))

!  TRANSLATE RIGHT. Translate a RIGHT scope to HTML.

      translateRight :−
       (form () void:
        (in tags(''p'', ''align="right"'')
         do (for ref object left, ref object right in items(scope↑.cdr)
             do (if isWord(left)
                 then emitWord(left, right)
                 else if isScope(left)
                      then translateScope(left)
                      else if left = endParaAtom
                           then emitLoneTag(''br /'')
                           else fail(''Right error'')))))

!  TRANSLATE RULE. Translate a RULE scope to HTML.

      translateRule :−
       (form () void:
         emitLoneTag(''hr /''))

!  TRANSLATE TABLE. Translate a TABLE scope to HTML.

      translateTable :−
       (form () void:
        (with
          string tableOpts    :− ''border="1" cellpadding="3" cellspacing="0"''
          string firstRowOpts :− ''bgcolor="#'' & gray & ''" valign="top"''
          string laterRowOpts :− ''valign="top"''
         do (in tags(''table'', ''align="center" '' & tableOpts)
             do (with var ref object scopes :− scope↑.cdr
                 do (if scopes ≠ nil
                     then (in tags(''tr'', firstRowOpts)
                           do (for ref object scope
                               in items(scopes↑.car↑.cdr)
                               do (in tags(''td'')
                                   do translateScope(scope))))
                          scopes := scopes↑.cdr
                          (while scopes ≠ nil
                           do (in tags(''tr'', laterRowOpts)
                               do (for ref object scope
                                   in items(scopes↑.car↑.cdr)
                                   do (in tags(''td'')
                                       do translateScope(scope)))
                                  scopes := scopes↑.cdr)))))))

!  This is TRANSLATE SCOPE's body. We dispatch to a translator according to the
!  atom in the CAR of SCOPE. We ignore all illegal scopes, since we scolded the
!  user about them earlier.

     do (case scope↑.car{ref atom}↑.label
         of bulletLabel: translateBullet()
            centerLabel: translateCenter()
            columnLabel: translateColumn()
           displayLabel: translateDisplay()
              gotoLabel: translateGoto()
             imageLabel: translateImage()
            indentLabel: translateIndent()
            insertLabel: translateInsert()
           itemizeLabel: translateItemize()
           justifyLabel: translateJustify()
             labelLabel: translateLabel()
            layoutLabel: translateLayout()
              leftLabel: translateLeft()
            narrowLabel: translateNarrow()
            numberLabel: translateNumber()
             orsonLabel: translateOrson()
              overLabel: translateOver()
             rightLabel: translateRight()
              ruleLabel: translateRule()
             tableLabel: translateTable())))

!  TRANSLATE STYLE. Write a STYLE block. It specifies the colors used by links,
!  the lines used by RULEs, and the borders used by TABLEs.

  translateStyle :−
   (form () void:
    (in tags(''style'')
     do emitStrings(:
         ''a:active'',
         '' { color: purple;'',
         ''   font-weight: bold;'',
         ''   text-decoration: none }'',
         ''a:link'',
         '' { color: blue;'',
         ''   font-weight: bold;'',
         ''   text-decoration: none }'',
         ''a:visited'',
         '' { color: purple;'',
         ''   font-weight: bold;'',
         ''   text-decoration: none }'',
         ''hr'',
         '' { background-color: black;'',
         ''   border: none;'',
         ''   color: black;'',
         ''   height: 1px }'',
         ''table'',
         '' { border-collapse: collapse }'')))

!  TRANSLATE TITLE. Translate a TITLE scope to HTML.

  translateTitle :−
   (form (ref object scope) void:
    (in tags(''title'')
     do (for ref object item in items(scope↑.cdr)
         do emitTitleWord(item))))

!  TRANSLATE WORD. Translate the word ITEM to HTML.

  translateWord :−
   (form (ref object item) void:
    (in tags(''p'')
     do emitWord(item, nil)))
)

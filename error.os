!
!  BRACY/ERROR. Handle syntax errors.
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

!  Errors.

  foj makeErr :− enum(limit(set) + 1)   !  Return a new error number.

  inj backslashErr        :− makeErr()  !  Illegal backslashed character.
  inj braceExpectedErr    :− makeErr()  !  '}' expected.
  inj charErr             :− makeErr()  !  Illegal character.
  inj charExpectedErr     :− makeErr()  !  Character expected.
  inj duplicateLabelErr   :− makeErr()  !  Duplicate label.
  inj endLineExpectedErr  :− makeErr()  !  End of line expected.
  inj evenScopesErr       :− makeErr()  !  Even number of scopes expected.
  inj externalExpectedErr :− makeErr()  !  External label expected.
  inj labelErr            :− makeErr()  !  Illegal label.
  inj lineTooLongErr      :− makeErr()  !  Line too long.
  inj pathErr             :− makeErr()  !  Cannot access file.
  inj rowExpectedErr      :− makeErr()  !  'row' scope expected.
  inj rowWidthErr         :− makeErr()  !  'row' scope has wrong width.
  inj scopeExpectedErr    :− makeErr()  !  Scope expected.
  inj textExpectedErr     :− makeErr()  !  Text expected inside scope.
  inj unexpectedBraceErr  :− makeErr()  !  Unexpected '}'.
  inj unexpectedScopeErr  :− makeErr()  !  Unexpected scope.
  inj unexpectedWordErr   :− makeErr()  !  Unexpected word.
  inj unknownScopeErr     :− makeErr()  !  Misplaced or unknown scope.
  inj unknownTokenErr     :− makeErr()  !  Unexpected character.

!  LINE ERR. Record that the errors in ERRS occurred at line number LINE in the
!  current SOURCE file. LINE ERRs are linked into a linear chain via their NEXT
!  slots, in increasing order of their LINE slots.

  lineErr :−
   (tuple
     int             line,
     var set         errs,
     var ref lineErr next)

!  MAKE LINE ERR. Return a LINE ERR that asserts no errors occurred at LINE.

  makeLineErr :−
   (proc (int line) ref lineErr:
    (with ref var lineErr this :− fromHeap(var lineErr)
     do this↑.line := line
        this↑.errs := ∅
        this↑.next := nil
        this{ref lineErr}))

!  SYNTAX ERROR. Assert that ERR occurred at line number SOURCE LINE of SOURCE.
!  FIRST is the dummy head node in a linear chain of LINE ERRs, and LAST is the
!  last such node.

  ref lineErr     first :− makeLineErr(0)
  var ref lineErr last  :− first

  syntaxError :−
   (proc (int err) void:
    (if sourceErrs > 0
     then sourceErrs −= 1
          (if last↑.line ≠ sourceLine
           then (with ref lineErr this :− makeLineErr(sourceLine)
                 do last↑.next := this
                    last := this))
          last↑.errs ∪= err
          hasErrs := true))

!  WRITE MESSAGES. Write all error messages in the chain of LINE ERRs, starting
!  after FIRST.

  writeMessages :−
   (proc () void:
    (with var ref lineErr this :− first↑.next

!  MESSAGE. Return the message string that corresponds to ERR.

      message :−
       (form (int err) string:
        (case err
         of backslashErr:        ''Illegal backslashed character''
            braceExpectedErr:    '''}' expected''
            charErr:             ''Illegal character''
            charExpectedErr:     ''Character expected''
            duplicateLabelErr:   ''Duplicate label''
            endLineExpectedErr:  ''End of line expected''
            evenScopesErr:       ''Even number of scopes expected''
            externalExpectedErr: ''External label expected''
            labelErr:            ''Illegal label''
            lineTooLongErr:      ''Line too long''
            pathErr:             ''Cannot access file''
            rowExpectedErr:      '''row' scope expected''
            rowWidthErr:         '''row' scope has wrong width''
            scopeExpectedErr:    ''Scope expected''
            textExpectedErr:     ''Text expected inside scope''
            unexpectedBraceErr:  ''Unexpected '}'''
            unexpectedScopeErr:  ''Unexpected scope''
            unexpectedWordErr:   ''Unexpected word''
            unknownScopeErr:     ''Misplaced or unknown scope''
            unknownTokenErr:     ''Unexpected character''
            none:                fail(''Unknown err %i.'': err)))

!  This is WRITE MESSAGES's body. Visit every LINE ERR, write its messages, and
!  send the LINE ERR back to the heap. Finally restore FIRST and LAST.

     do (while this ≠ nil
         do (for inj err in elements(this↑.errs)
             do writeln(''%s:%i: %s.'': sourcePath, this↑.line, message(err)))
            this := (this↑.next also toHeap(this)))
        first↑.next := nil
        last := first))
)

!
!  BRACY/GLOBAL. Global constant and variable declarations.
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

!  Constants.

  string background          :− ''FFFFFF''  !  Background color.
  char   endChar             :− eos{char}   !  End of stream.
  char   eofChar             :− '\#01'      !  End of source file.
  char   eolChar             :− '\#02'      !  End of source line.
  string gray                :− ''D3D3D3''  !  Shade of gray.
  char   linefeedChar        :− '\N'        !  Because it's ugly.
  char   returnChar          :− '\R'        !  Because it's ugly.
  int    maxSourceLineLength :− 1024        !  Max length of a source line.
  string version             :− ''0.4''     !  Version number.

!  Variables.

  var char   ch         :− ' '    !  Current char from LINE.
  var bool   hasErrs    :− false  !  Have we seen syntax errors?
  var stream html                 !  Stream to object file.
  var int    maxErrs    :− 5      !  (-e) Max error messages for a source.
  var bool   nakeding   :− false  !  (-n) Rendering HTML for later INSERTs?
  var bool   proofing   :− false  !  (-p) Rendering HTML in proof mode?
  var bool   quoting    :− false  !  (-q) Writing single quotes?
  var bool   seenTitle  :− false  !  Have we seen a TITLE scope?
  var stream source               !  Stream to source file.
  var int    sourceErrs           !  Errors left to report in SOURCE.
  var int    sourceLine           !  Current line number in SOURCE.
  var string sourcePath           !  Path to STREAM's file.
  var bool   uparrowing :− false  !  (-u) Writing uparrows?

!  Buffer variables.

  char0Buffer :− buffer(maxSourceLineLength + 1, char0)
  char1Buffer :− buffer(maxSourceLineLength + 1, char1)
  setBuffer   :− buffer(maxSourceLineLength + 1, set)

  var char1Buffer line        !  Current line from SOURCE.
  var char0Buffer wordChars   !  Chars of current word.
  var setBuffer   wordStyles  !  Styles of current word.
)

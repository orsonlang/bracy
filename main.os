!
!  BRACY/MAIN. Main program.
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

  string bracySuffix  :− ''by''    !  Bracy source files end in this.
  inj    directoryMax :− 4096      !  Max size of a directory name.
  inj    fileMax      :− 256       !  Max size of a file name.
  string htmlSuffix   :− ''html''  !  Bracy object files end in this.
  inj    suffixMax    :− 16        !  Max size of a file suffix name.
  string version      :− ''0.5''   !  Version number.

!  FILE OPTION. (1) Open a source file, whose pathname is in VALUE. (2) Open an
!  HTML object file corresponding to that source file. (3) Translate the source
!  FILE, writing the results to the HTML file. (4) Close both files.

  fileOption :−
   (form (string value) void:
    (with var buffer(directoryMax + fileMax + suffixMax + 2) htmlPath
     do sourcePath := value
        (if sourcePath↑ = '-'
         then fail(''Illegal pathname '%s'.'': sourcePath))
        (if ¬ isEnd(sourcePath, '.' & bracySuffix)
         then fail(''Unexpected suffix in '%s'.'': sourcePath))
        (if ¬ open(source, sourcePath, ''r'')
         then fail(''Cannot open '%s'.'': sourcePath))
        (for string sourceDirectory, string sourceFile, string
         in pathname(sourcePath, directoryMax, fileMax, suffixMax)
         do empty(htmlPath)
            (if ¬ isEmpty(sourceDirectory)
             then append(htmlPath, sourceDirectory)
                  append(htmlPath, '/'))
            append(htmlPath, sourceFile)
            append(htmlPath, '.')
            append(htmlPath, htmlSuffix)
            (if ¬ open(html, htmlPath{string}, ''w'')
             then fail(''Cannot open '%s'.'': htmlPath{string}))
            seenTitle  := false
            sourceErrs := maxErrs
            sourceLine := 0
            translate(nextSource())
            writeMessages()
            (if ¬ close(html)
             then fail(''Cannot close '%s'.'': htmlPath{string})))
        (if ¬ close(source)
         then fail(''Cannot close '%s'.'': sourcePath))))

!  ERROR OPTION. Set the maximum number of error messages for each source.

  errorOption :−
   (form (string value) void:
    (if value = ''all''
     then maxErrs := high(int)
     else (for bool ok, int count in convert(int, value)
           do (if ok
               then maxErrs := count
               else fail(''-e must be an integer or 'all'.'')))))

!  FLAG OPTION. Set FLAG to TRUE.

  flagOption :−
   (form (var bool flag) void:
     flag := true)

!  VERSION OPTION. Write version information.

  versionOption :−
   (form () void:
     writeln(''Bracy to HTML translator, version '' & version & ''.''))

!  MAIN. Main program. Visit each command line option and do what it says.

  main :−
   (for char option, string value in command(''npquv'', '' e'')
    do (case option
        of ' ': fileOption(value)
           'e': errorOption(value)
           'n': flagOption(nakeding)
           'p': flagOption(proofing)
           'q': flagOption(quoting)
           'u': flagOption(uparrowing)
           'v': versionOption()))
)

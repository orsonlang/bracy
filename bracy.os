!
!  BRACY. A simple document compiler that produces HTML.
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

!  Bracy is a simple document compiler. It outputs HTML files that are supposed
!  to look like printed pages. It doesn't always succeed, because most browsers
!  aren't good at rendering text, but it gets as close as it can. It also tries
!  to make HTML that Humans can read, so you can edit a file made by Bracy even
!  if you don't have the Bracy source for it.

(load ''lib.ascii'')     !  Operations on ASCII characters.
(load ''lib.buffer'')    !  Fixed length linear queues.
(load ''lib.command'')   !  Process command line arguments.
(load ''lib.convert'')   !  Convert a string to an integer or a real.
(load ''lib.dynamic'')   !  Dynamic memory.
(load ''lib.fail'')      !  Terminate a program with an error message.
(load ''lib.file'')      !  Input and output on file streams.
(load ''lib.headtail'')  !  Traverse arrays using pointers.
(load ''lib.path'')      !  Operations on Unix pathnames.
(load ''lib.plain'')     !  Operations on Orson plain names.
(load ''lib.select'')    !  Simulate a CASE clause whose labels are strings.
(load ''lib.sequence'')  !  Iterators that expand to sequences.
(load ''lib.string'')    !  Operations on strings.
(load ''lib.time'')      !  Dates and times.
(load ''lib.width'')     !  Determine columns needed to display a character.

(load ''set'')           !  Sets of small nonnegative integers.
(load ''code'')          !  Character style codes.
(load ''style'')         !  Stacks of style codes.
(load ''global'')        !  Global declarations.
(load ''internal'')      !  Internal form of a Bracy source.
(load ''label'')         !  Test labels.
(load ''error'')         !  Handle syntax errors.
(load ''char'')          !  Read chars from a Bracy source file.
(load ''subtoken'')      !  Read subtokens from a Bracy source file.
(load ''token'')         !  Read tokens from a Bracy source file.
(load ''debug'')         !  Methods for debugging.
(load ''parse'')         !  Translate a Bracy source file to internal form.
(load ''emit'')          !  Write fragments of HTML code.
(load ''orsonize'')      !  Modify styles to display Orson source text.
(load ''translate'')     !  Translate internal form to HTML.
(load ''main'')          !  Main program.

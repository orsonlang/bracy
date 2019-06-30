#
#  BRACY/MAKEFILE. Compile, install, uninstall Bracx and Bracy.
#
#  Copyright © 2014 James B. Moen.
#
#  This  program is free  software: you  can redistribute  it and/or  modify it
#  under the terms  of the GNU General Public License as  published by the Free
#  Software Foundation,  either version 3 of  the License, or  (at your option)
#  any later version.
#
#  This program is distributed in the  hope that it will be useful, but WITHOUT
#  ANY  WARRANTY;  without even  the  implied  warranty  of MERCHANTABILITY  or
#  FITNESS FOR  A PARTICULAR PURPOSE.  See  the GNU General  Public License for
#  more details.
#
#  You should have received a copy of the GNU General Public License along with
#  this program.  If not, see <http://www.gnu.org/licenses/>.
#

#  Each action has a comment that describes exactly what it does to your system
#  and whether you must be root to do it. Please read these comments carefully!
#  This Makefile assumes that an Orson compiler is installed. An Orson compiler
#  is available for free from the author.
#
#  These directories are where Bracx and Bracy will be installed.
#
#    PREFIX/BINDIR   Bracx and Bracy will be installed here.
#    PREFIX/MANDIR   The Bracx and Bracy "man" pages will be installed here.
#
#  If these directories do not exist, then they will be created.

prefix = /usr/local
bindir = $(prefix)/bin
mandir = $(prefix)/man/man1

#  ALL. Compile Bracx and Bracy. Leave them in the current directory.  You need
#  not be root to do this.

all:
	orson bracx.os
	mv a.out bracx
	orson bracy.os
	mv a.out bracy

#  CLEAN. Undo MAKE ALL, MAKE BRACX and MAKE BRACY.  You need not be root to do
#  this.

clean:
	rm -f Out.c a.out bracx bracy

#  BRACX. Compile Bracx from the source file in the current directory. Leave it
#  in the current directory. You need not be root to do this.

bracx:
	orson bracx.os
	mv a.out bracx

#  BRACY. Compile Bracy from the source file in the current directory. Leave it
#  in the current directory. You need not be root to do this.

bracy:
	orson bracy.os
	mv a.out bracy

#  INSTALL. Install Bracx and Bracy, by doing these things.
#
#    01. Make BIN DIRECTORY if it doesn't exist.
#    02. Make MAN DIRECTORY if it doesn't exist.
#    03. Compile Bracx.
#    04. Move Bracx to BIN DIRECTORY.
#    05. Make root the owner of Bracx.
#    06. Let nonroots read and run Bracx, not write it.
#    07. Copy the Bracx man page to MAN DIRECTORY.
#    08. Make root the owner of the Bracx man page.
#    09. Let nonroots read the Bracx man page, not run or write it.
#    10. Compile Bracy.
#    11. Move Bracy to BIN DIRECTORY.
#    12. Make root the owner of Bracy.
#    13. Let nonroots read and run Bracy, not write it.
#    14. Copy the Bracy man page to MAN DIRECTORY.
#    15. Make root the owner of the Bracy man page.
#    16. Let nonroots read the Bracy man page, not run or write it.
#
#  You must be root to do this.

install:
	mkdir -p $(bindir)
	mkdir -p $(mandir)
	orson bracx.os
	mv a.out $(bindir)/bracx
	chown root $(bindir)/bracx
	chmod go-w+rx $(bindir)/bracx
	cp bracx.1 $(mandir)/
	chown root $(mandir)/bracx.1
	chmod go-wx+r $(mandir)/bracx.1
	orson bracy.os
	mv a.out $(bindir)/bracy
	chown root $(bindir)/bracy
	chmod go-w+rx $(bindir)/bracy
	cp bracy.1 $(mandir)/
	chown root $(mandir)/bracy.1
	chmod go-wx+r $(mandir)/bracy.1

#  UNINSTALL. Undo the effects of MAKE INSTALL, by doing these things.
#
#    1. Undo the effects of MAKE ALL, MAKE BRACX, and MAKE BRACY.
#    2. Delete Bracx from BIN DIRECTORY.
#    3. Delele the Bracx man page from MAN DIRECTORY.
#    4. Delete Bracy from BIN DIRECTORY.
#    5. Delete the Bracy man page from MAN DIRECTORY.
#
#  Note that BIN DIRECTORY and MAN DIRECTORY will still exist. You must be root
#  to do this.

uninstall:
	rm -f Out.c a.out bracx bracy
	rm -f $(bindir)/bracx
	rm -f $(mandir)/bracx.1
	rm -f $(bindir)/bracy
	rm -f $(mandir)/bracy.1

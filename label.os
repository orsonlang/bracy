!
!  BRACY/LABEL. Test labels.
!
!  Copyright © 2018 James B. Moen.
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

!  LABEL NODE. A node in an unbalanced binary search tree. LABEL is the key and
!  LEFT and RIGHT are its subtrees.

  labelNode :−
   (tuple
     string            label,
     var ref labelNode left,
     var ref labelNode right)

!  IS INTERNAL. Test if the word ITEM can be an internal label. It can, if it's
!  made up entirely of digits and Roman letters.

  isInternal :−
   (form (ref object item) bool:
    (with var string chars :− item{ref word}↑.chars
     do (while isLetterOrDigit(chars↑)
         do chars += 1)
        chars↑ = '\0'))

!  MAKE LABEL TREE. Initialize LABEL TREE with a new label tree.

  var ref labelNode labelTree :− nil

  makeLabelTree :−
   (form () void:
    (with

!  UNMAKE TREE. Recursively delete the tree rooted at ROOT. We assume there are
!  no circular pointers.

      unmakeTree :−
       (proc (ref labelNode root) void:
        (with var ref labelNode subtree :− root
         do (while subtree ≠ nil
             do unmakeTree(subtree↑.left)
                toHeap(subtree↑.label)
                subtree := (subtree↑.right also toHeap(subtree)))))

!  This is MAKE LABEL TREE's body. Destroy the existing search tree LABEL TREE,
!  if there is one. Then reset LABEL TREE to a sentinel node.

     do unmakeTree(labelTree)
        labelTree := makeLabelNode(ϵ)))

!  MAKE LABEL NODE. Return a new LABEL NODE with empty LEFT and RIGHT subtrees.
!  It holds LABEL.

  makeLabelNode :−
   (proc (string label) ref labelNode:
    (with ref var labelNode this :− fromHeap(var labelNode)
     do this↑.label := copy(label)
        this↑.left  := nil
        this↑.right := nil
        this{ref labelNode}))

!  IS DUPLICATE. Test if LABEL is a key in the tree rooted at LABEL TREE. If so
!  then return true. If not then add a nee node to LABEL TREE that holds a copy
!  of LABEL and return false.

  isDuplicate :−
   (proc (string label) bool:
    (with
      var ref labelNode subtree :− labelTree
      var int test
     do (while
          test := comp(label, subtree↑.label)
          (if test < 0
           then (if subtree↑.left = nil
                 then subtree↑.left := makeLabelNode(label)
                      false
                 else subtree := subtree↑.left
                      true)
           else if test > 0
                then (if subtree↑.right = nil
                      then subtree↑.right := makeLabelNode(label)
                           false
                      else subtree := subtree↑.right
                           true)
                else false))
        test = 0))
)

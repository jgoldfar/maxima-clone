#!/usr/bin/wish
# SPDX-License-Identifier: GPL-2.0-or-later
# Simple GUI for selecting the default Lisp for Windows users.

#  @lispXY_ENABLED@ will be replaced by 0 or 1 in the final program.
set clisp @CLISP_ENABLED@
set sbcl @SBCL_ENABLED@
set abcl @ABCL_ENABLED@
set ccl @CCL_ENABLED@

proc selectclisp {} {
    set maximarc [file join $::env(USERPROFILE) maxima maximarc]
    file mkdir [file dirname $maximarc]
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=clisp"
    close $f
    tk_messageBox -type ok -message "CLISP was selected as default Lisp interpreter for Maxima." -icon info
}

proc selectsbcl {} {
    set maximarc [file join $::env(USERPROFILE) maxima maximarc]
    file mkdir [file dirname $maximarc]
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=sbcl"
    close $f
    tk_messageBox -type ok -message "SBCL was selected as default Lisp interpreter for Maxima." -icon info
}

proc selectabcl {} {
    set maximarc [file join $::env(USERPROFILE) maxima maximarc]
    file mkdir [file dirname $maximarc]
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=abcl"
    close $f
    tk_messageBox -type ok -message "ABCL was selected as default Lisp interpreter for Maxima." -icon info
}

proc selectccl {} {
    set maximarc [file join $::env(USERPROFILE) maxima maximarc]
    file mkdir [file dirname $maximarc]
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=ccl64"
    close $f
    tk_messageBox -type ok -message "CCL was selected as default Lisp interpreter for Maxima." -icon info
}

set binpath [file dirname [file normalize [info script]]]

set documentation "One can use different LISP (the programming language, in which Maxima is (mostly) written) compilers for running Maxima.

Currently this Windows installer supports:
"

if {$clisp == 1} { append documentation "- ABCL (https://www.clisp.org)" }
if {$sbcl == 1} { append documentation "- ABCL (https://www.sbcl.org)" }
if {$abcl == 1} { append documentation "- ABCL (https://www.abcl.org)" }
if {$ccl == 1} { append documentation "- CCL (https://ccl.clozure.com)" }

append documentation "
Which Lisp you select, may depend on your needs:

SBCL is usually faster, but there were issues with DEP (data execution prevention) reported. 
It might be necessary to disable DEP for $binpath/sbcl.exe. 
Due to memory problems, some packages (e.g. Lapack) may not work.

CLISP may be slower, but these problems do not occur there. In command line Maxima CLISP 
provides advanced editing features (a history of previous commands is accessible with the cursor keys).

"

if {$abcl == 1} { append documentation "Armed Bear Common Lisp (ABCL) is a full implementation of the Common Lisp language running in the JVM. 
Java must be installed, if you use ABCL.

" }

if {$ccl == 1} { append documentation "Clozure Common Lisp (CCL) is a free Common Lisp implementation with a long history. CCL support is new in the crosscompiled Windows installer.

" }

append documentation "If you select a Lisp, a configuration file 'maximarc' will be created
in the 'maxima' directory of your user profile directory with your default Lisp selection.
If the configuration file already exists, it will be overwritten.
"

# Buttons (clisp, sbcl, exit)
frame .toolbar
if {$clisp == 1} { button .toolbar.clisp -text "Select CLISP" -command "selectclisp" }
if {$sbcl == 1} { button .toolbar.sbcl -text "Select SBCL" -command "selectsbcl" }
if {$abcl == 1} { button .toolbar.abcl -text "Select ABCL" -command "selectabcl" }
if {$ccl == 1} { button .toolbar.ccl -text "Select CCL" -command "selectccl" }
button .toolbar.exit -text "Exit" -command "exit"
if {$clisp == 1} { pack .toolbar.clisp -side left }
if {$sbcl == 1} { pack .toolbar.sbcl -side left }
if {$abcl == 1} { pack .toolbar.abcl -side left }
if {$ccl == 1} { pack .toolbar.ccl -side left }
pack .toolbar.exit -side right

# Documentation area
frame .docu
label .docu.label -text $documentation
pack .docu.label -padx 50 -pady 50


wm title . "Maxima Open Source Computer Algebra System - Select the LISP Compiler"
grid config .toolbar -column 0 -row 1 -sticky "snew"
grid config .docu    -column 0 -row 2


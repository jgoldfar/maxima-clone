#!/usr/bin/wish
# SPDX-License-Identifier: GPL-2.0-or-later
# Simple GUI for selecting the default Lisp for Maxima.

# @lispXY_ENABLED@ will be replaced by 0 or 1 in the final program.
set clisp @CLISP_ENABLED@
set sbcl @SBCL_ENABLED@
set abcl @ABCL_ENABLED@
set ccl @CCL_ENABLED@
set ecl @ECL_ENABLED@

# Allow use it on Unix too (if the variables above are set...)
if {$tcl_platform(platform) eq "windows"} {
    set maximarc [file join $::env(USERPROFILE) maxima maximarc]
} else {
    set maximarc [file join $::env(HOME) .maxima maximarc]
}

file mkdir [file dirname $maximarc]

proc selectclisp {} {
    global maximarc
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=clisp"
    close $f
    tk_messageBox -type ok -message "CLISP was selected as default Lisp interpreter for Maxima." -icon info
}

proc selectsbcl {} {
    global maximarc
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=sbcl"
    close $f
    tk_messageBox -type ok -message "SBCL was selected as default Lisp interpreter for Maxima." -icon info
}

proc selectabcl {} {
    global maximarc
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=abcl"
    close $f
    tk_messageBox -type ok -message "ABCL was selected as default Lisp interpreter for Maxima." -icon info
}

proc selectccl {} {
    global maximarc
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=ccl64"
    close $f
    tk_messageBox -type ok -message "CCL was selected as default Lisp interpreter for Maxima." -icon info
}

proc selectecl {} {
    global maximarc
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=ecl"
    close $f
    tk_messageBox -type ok -message "ECL was selected as default Lisp interpreter for Maxima." -icon info
}

set binpath [file dirname [file normalize [info script]]]

set documentation "One can use different LISP (the programming language, in which Maxima is (mostly) written) compilers for running Maxima.

Currently this Windows installer supports:
"

if {$clisp == 1} { append documentation "- CLISP (https://www.clisp.org)\n" }
if {$sbcl == 1} { append documentation "- SBCL (https://www.sbcl.org)\n" }
if {$abcl == 1} { append documentation "- ABCL (https://www.abcl.org)\n" }
if {$ccl == 1} { append documentation "- CCL (https://ccl.clozure.com)\n" }
if {$ecl == 1} { append documentation "- ECL (https://ecl.common-lisp.dev)\n" }

append documentation "\n"

if {$sbcl == 1} { append documentation "Steel Bank Common Lisp (SBCL) is a high performance Common Lisp compiler.
It is open source / free software, with a permissive license. In addition to the compiler and runtime system
for ANSI Common Lisp, it provides an interactive environment including a debugger, a statistical profiler,
a code coverage tool, and many other extensions.

" }

if {$clisp == 1} { append documentation "GNU CLISP is a Common Lisp implementation.
It conforms to the ANSI Common Lisp standard, and offers many extensions.
It runs on all desktop operating systems (GNU and Unix systems, macOS,
Windows) and is particularly memory-efficient.

" }

if {$abcl == 1} { append documentation "Armed Bear Common Lisp (ABCL) is a full implementation of the Common Lisp language running in the JVM.
Java must be installed, if you use ABCL.

" }

if {$ccl == 1} { append documentation "Clozure Common Lisp (CCL) is a free Common Lisp implementation with a long history.

" }

if {$ecl == 1} { append documentation "Embeddable Common-Lisp (ECL) aims to produce an implementation of the Common-Lisp language which complies
to the ANSI X3J13 definition of the language.

" }

append documentation "If you select a Lisp, a configuration file '$maximarc' will be created
with your default Lisp selection.
If the configuration file already exists, it will be overwritten.
"

# Buttons (enabled Lisp versions, exit)
frame .toolbar
if {$clisp == 1} { button .toolbar.clisp -text "Select CLISP" -command "selectclisp" }
if {$sbcl == 1} { button .toolbar.sbcl -text "Select SBCL" -command "selectsbcl" }
if {$abcl == 1} { button .toolbar.abcl -text "Select ABCL" -command "selectabcl" }
if {$ccl == 1} { button .toolbar.ccl -text "Select CCL" -command "selectccl" }
if {$ecl == 1} { button .toolbar.ecl -text "Select ECL" -command "selectecl" }
button .toolbar.exit -text "Exit" -command "exit"
if {$clisp == 1} { pack .toolbar.clisp -side left }
if {$sbcl == 1} { pack .toolbar.sbcl -side left }
if {$abcl == 1} { pack .toolbar.abcl -side left }
if {$ccl == 1} { pack .toolbar.ccl -side left }
if {$ecl == 1} { pack .toolbar.ecl -side left }
pack .toolbar.exit -side right

# Documentation area
frame .docu
label .docu.label -text $documentation
pack .docu.label -padx 50 -pady 50


wm title . "Maxima Open Source Computer Algebra System - Select the LISP Compiler"
grid config .toolbar -column 0 -row 1 -sticky "snew"
grid config .docu    -column 0 -row 2


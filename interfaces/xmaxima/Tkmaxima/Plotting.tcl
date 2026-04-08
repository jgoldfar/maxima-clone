###### plotting.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

global axisGray
if { "[winfo screenvisual .]" == "staticgray" } {
    set axisGray black
} else  {
    set axisGray gray60
}
if {[catch { set doExit }] } { set doExit ""}

## endsource plotting.tcl

############################################################
# Gui.tcl                                                  #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
#     Modified by Jaime E. Villate                         #
############################################################

# Creates the browser if it doesn't exist
proc createBrowser {bname} {
    if {[winfo exists $bname]} {
        focus $bname
    } else {
        toplevel $bname
        wm title $bname [mc {Xmaxima: browser}]
        if {[file isfile [string map {file:/ ""} $::xmaxima_priv(firstUrl)]]} {
            OpenMathOpenUrl $::xmaxima_priv(firstUrl) -toplevel $bname
        } else {
            set ::xmaxima_priv(error) \
                "<h1>Error</h1>\
                <p>Maxima primer not found in: $::xmaxima_priv(firstUrl)</p>"
             OpenMathOpenUrl "xmaxima://error" -toplevel $bname
        }
        set ::xmaxima_priv(cBrowser) $bname
        set ::xmaxima_default(browser) 1
        # Adds the menubar and the toolbar to the browser
        vMAXAddBrowserMenu $bname}}

# Creates the Maxima console
proc createConsole {cname} {
    # Makes the status panel....
    set st .status
    frame $st
    set ::xmaxima_priv(cStatusWindow) $st
    label $st.rate -width 35 -bd 1 -relief sunken  -justify left \
        -textvariable ::xmaxima_priv(load_rate) -anchor w
    scale $st.scale -showvalue 0 -length 200  -orient horizontal
    pack $st.rate -side left -fill x -expand 1 -anchor w
    pack $st.scale -side left
    pack $st -side bottom -fill x -anchor w
    set ::xmaxima_priv(cStatusLabel) $st.rate

    # Adds the toolbar to the Maxima console
    vMAXAddSystemBar
    frame $cname
    pack $cname -expand 1 -fill both -side top
    set w $cname.text
    clearLocal $w
    oset $w heightDesired 80%
    set ::xmaxima_priv(maximaWindow) $w
    closeMaxima $w
    clearLocal $w

    # oset $w program $program
    oset $w prompt "% "
    if {[winfo exists $w]} {catch { destroy $w }}
    frame $cname.bottom -height 2
    $cname.bottom config -cursor double_arrow
    bind  $cname.bottom <B1-Motion> "changeSize $w %Y"
    pack $cname.bottom -side bottom -fill x
    text $w -yscrollcommand "$cname.scroll set" \
	    	-selectbackground yellow -selectforeground blue
    set ::xmaxima_priv($w,inputTag) input
    # resetMaximaFont $w
    scrollbar $cname.scroll -command "$w yview"
    pack $cname.scroll -side right -fill y
    pack $cname.text -expand 1 -fill both -side left
    $w mark set lastStart end
    $w mark gravity lastStart left
    bind $w <Configure> "resizeSubPlotWindows $w %w %h; resizeMaxima $w %w %h"
    $w configure -background "#FFFAEE"
    $w configure -foreground "#008600"
    $w tag configure input -foreground blue
    $w tag configure output -foreground black
    $w tag configure mprompt -foreground red

    # binding order will be: window bindings, CNtext bindings,
    # OpenMathText bindings and default bindings (usually Text . all)
    # CNtext ans OpenMathText bindings are set up in Bindings.tcl
    bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

    if {![regexp -- input $::xmaxima_priv(sticky)] } {
        append ::xmaxima_priv(sticky) {|^input$}}
    set ::xmaxima_priv(cConsoleText) $cname.text
    vMAXSetCNTextBindings $w
    wm protocol . WM_DELETE_WINDOW [list maxExit $w]

    # Sets up the console size and font
    $w configure -height $::xmaxima_default(iConsoleHeight) \
        -width $::xmaxima_default(iConsoleWidth)
    font configure ConsoleFont -family [lindex $::xmaxima_default(ConsoleFont) 0] \
        -size [lindex $::xmaxima_default(ConsoleFont) 1]
    $w configure -font ConsoleFont

    # Adds the menu bar to the Maxima console
    vMAXAddSystemMenu $cname $cname.text

    # Reads the history from previous runs
    if {[file isfile $::xmaxima_priv(history)]} {
        set fileId [open $::xmaxima_priv(history) r]
        set commands [read $fileId]
        close $fileId
        regsub {oset[^\n]*\n} $commands {} $commands
        regsub {\n\s*\}\s*\n} $commands {} $commands
        if {[catch {oset {.maxima.text} inputs $commands} err]} {
            tk_messageBox -title Error -icon error -message \
                [mc "Error sourcing %s\n%s" [file native $histfile] $err]
        }
    }
    return $w}

# Updates the information in the status bar at the bottom of the console
proc maxStatus {mess} {
    set ::xmaxima_priv(load_rate) $mess
    $::xmaxima_priv(cStatusLabel) configure -text $mess
}

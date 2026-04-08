# Copyright (c) 2009-2026, Jaime E. Villate <villate@fe.up.pt>
#
# Procedures to work with colors
# (the license information can be found in COPYING.tcl)
#
##############################################################################
# Three notations are used for colors:                                       #
#                                                                            #
# 1. A list of red, green and blue, being all integers between 0 and 255.    #
#                                                                            #
# 2. A list of hue, saturation and value, where hue is an integer between 0  #
# and 360, while saturation and value are numbers between 0 and 1.           #
#                                                                            #
# 3. A string "#rrggbb", where rr, gg and bb are 2-digit hexadecimal numbers #
# (the red, green and blue components).                                      #
##############################################################################

# Transforms a (hue, saturation, value) set into a (red, green, blue) set
proc hsv2rgb {hue sat val} {
    if { $sat < 0 } { set sat [expr 1 -  $sat] }
    if { $val < 0 } { set val [expr 1 -  $val] }
    if { $val > 1 } { set val [expr $val - int($val)] }
    if { $sat > 1 } { set sat [expr $sat - int($sat)] }
    set v  [expr round($val*255)]
    if {$sat == 0} {
        return [format "\#%02x%02x%02x" $v $v $v]
    } else {
        set h [expr {round($hue)%360/60.0}]
        set i [expr {round($hue)%360/60}]
        set f [expr {$h-$i}]
        set u [expr {round($v)}]
        set p [expr {round($v*(1-$sat))}]
        set q [expr {round($v*(1-$sat*$f))}]
        set t [expr {round($v*(1-$sat*(1-$f)))}]
        switch $i {
            0 {set r $u; set g $t; set b $p}
            1 {set r $q; set g $u; set b $p}
            2 {set r $p; set g $u; set b $t}
            3 {set r $p; set g $q; set b $u}
            4 {set r $t; set g $p; set b $u}
            5 {set r $u; set g $p; set b $q}}
        return [format "\#%02x%02x%02x" $r $g $b]
    }
}

# Transforms a color code #rrggbb into a list of three components (red,
# green and blue) between 0 and 255
proc rgb2list {rgb} {
    set num "0x"
    set r [append num [string range $rgb 1 2]]
    set num "0x"
    set g [append num [string range $rgb 3 4]]
    set num "0x"
    set b [append num [string range $rgb 5 6]]
    return [format "%d %d %d" $r $g $b]
}

# Transforms a list c with three components between 0 and 255 (red,
# green and blue components) into a color code #rrggbb
proc list2rgb {c} {
    return [format "\#%02x%02x%02x" [lindex $c 0] [lindex $c 1] [lindex $c 2]]}

# Given a fraction f between 0 and 1 and at least two color codes in the form
# #rrggbb returns another color code in the form #rrggbb obtained by linear
# interpolation of the given colors
proc interpolatecolor {f rgb1 rgb2 args} {
    set colors [list [rgb2list $rgb1]]
    lappend colors [rgb2list $rgb2]
    foreach color $args {lappend colors [rgb2list $color]}
    set l [llength $colors]
    set x [expr {$f*($l-1.0)}]
    set n [expr {int($x)}]
    set r [expr {$x-int($x)}]
    if {$n == [expr {$l-1}]} {
        return [lindex $colors end]
    } else {
        set c1 [lindex $colors $n]
        set c2 [lindex $colors $n+1]
        set r1 [lindex $c1 0]
        set g1 [lindex $c1 1]
        set b1 [lindex $c1 2]
        set r2 [lindex $c2 0]
        set g2 [lindex $c2 1]
        set b2 [lindex $c2 2]
        return [list2rgb [list [expr {round($r1+$r*($r2-$r1))}] [expr {round($g1+$r*($g2-$g1))}] [expr {round($b1+$r*($b2-$b1))}]]]
    }
}


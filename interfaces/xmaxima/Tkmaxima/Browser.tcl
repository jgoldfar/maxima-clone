############################################################
# Browser.tcl                                              #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
#     Modified by Jaime E. Villate                         #
############################################################

proc peekLastCommand {win} {
    if { [info exists ::xmaxima_priv(lastcom,$win)] } {
	return $::xmaxima_priv(lastcom,$win)
    }
}

proc pushCommand { win command arglist } {
    set ::xmaxima_priv(lastcom,$win) [list $command $arglist]
}
#
#-----------------------------------------------------------------
#
# tkTextInsert --  we add some things to the default tkTextInsert
#  so that tags present before or after the insert, which are sticky
#  are added to the inserted string.   As usual, ones on both sides
#  are added.
#
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc tkTextInsert { w s } {
    set after [$w tag names insert]
    set before [$w tag names "insert-1char"]
    set both [intersect $after $before]
    # puts "after=$after"
    # puts "before=$before"

    foreach v [concat $after $before] {
	if { [regexp -- $::xmaxima_priv(sticky) $v] } {
	    lappend both $v
	}
    }

    if { [info exists ::xmaxima_priv($w,inputTag) ] } {
	lappend both $::xmaxima_priv($w,inputTag)
    }

    if {($s == "") || ([$w cget -state] == "disabled")} {
	return
    }
    catch {
	if {[$w compare sel.first <= insert]
	    && [$w compare sel.last >= insert]} {
	    $w delete sel.first sel.last
	}
    }
    $w insert insert $s $both
    $w see insert
}

proc getRange { win a b }  {
    if { [$win compare $a < $b ] } {
	return "$a $b" 
    } else { 
	return "$b $a"
    }
}
#
#-----------------------------------------------------------------
#
# tagRanges --  find ranges on WINDOW for TAG from FROMINDEX below TOINDEX
#
#  Results: a list of ranges start1 stop1 start2 stop2 ..
# which are contained in [fromindex,toindex] such that TAG is on from
# start1 to stop1 etc.
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc tagRanges { win tag begin end } {
    if {  [$win  compare $begin <= 1.0 ]  && \
	      [$win  compare $end >= end ] } {
	return [$win tag ranges $tag ] 
    } else {
	set answer ""
	    set begin [$win index $begin]
	    set end [$win index $end]
	    if { [lsearch [$win tag names $begin] $tag ]>=0 } {
		set prev [$win tag prevrange $tag $begin+1chars]
		set to [lindex $prev 1]
		if { [$win compare $to > $end ] } {
		    set to $end
		}
		append answer "$begin $to "
		set begin $to
	    }
	    #puts "<$begin $end>"
	    while { [$win compare $begin < $end ] } {
		set next [$win tag nextrange $tag $begin]
		#puts "next=$next"
		if { "$next" == "" } { return $answer }
		if { [$win compare [lindex $next 1] <= $end]} {
		    append answer "$next "
		    set begin [lindex $next 1]
		} elseif {[$win compare [lindex $next 0] < $end ]} {
		    append answer "[lindex $next 0] $end"
		    return $answer
		} else {
		    return $answer
		}
	    }
	    return $answer
	}
}
#
#-----------------------------------------------------------------
#
# quoteBraces --  given a STRING such that
# puts $file "set new [quoteBraces $string]"
# when re read by eval would make value of NEW identical to STRING
#
#  Results: a string
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc quoteBraces {string } {
    regsub -all {[{}]} $string {\\&} val
    return [list $val]
}

proc thisRange { win tag index } {
    set prev [$win tag prevrange $tag $index]
    if { "$prev" != "" && [$win compare [lindex $prev 1] >= $index] } {
	return $prev
    }
    set next [$win tag nextrange $tag $index]
    if { "$next" != ""  && [$win compare [lindex $next 0] <= $index] } {
	return $next
    }
    return ""
}
#
#-----------------------------------------------------------------
#
# insertRichText --  insert rich text in TEXTWINDOW at INDEX according
# to commands and data in LIST.   The latter must be of the form
#  command1 arg1 ..argn command2 arg1 ..argn2 ..
# for example if `Tins' takes two args
#  and the commands must be in
# since the rich text might come from a selection or some or an untrusted
# file we want to be careful not to do any bad evals.
#  Results: none
#
#  Side Effects:  the rich text commands are invoked to do insertions
# on the window.
#
#----------------------------------------------------------------
#
proc insertRichText {win index list } {
    set ::xmaxima_priv(currentwin) $win
    set ::xmaxima_priv(point) $index
    foreach v $::xmaxima_priv(richTextCommands) {
	set ::xmaxima_priv($v,richTextCommand) [llength [info args $v]]
    }
    set i 0
    set ll [llength $list]
    while { $i < $ll } {
	set com [lindex $list $i]
	incr i
	if { [catch { set n $::xmaxima_priv($com,richTextCommand)} ] } {
	    return -code error -errorinfo [concat [mc "illegal command in rich text:"] "$com"]
	}
	set form [concat $com [lrange $list $i [expr {$i +$n -1}]]]
	if { [catch {eval $form } ] } {
	    return -code error -errorinfo [concat [mc "unable to evaluate command:"] "`$form'"] }
	
	incr i $n
    }
}
proc Tins { tags text } {
    # foreach v $args { append text $v }
    $::xmaxima_priv(currentwin) insert $::xmaxima_priv(point) $text  $tags
}

proc TinsSlashEnd { tags text } {
    # foreach v $args { append text $v }
    $::xmaxima_priv(currentwin) insert $::xmaxima_priv(point) "$text\\"  $tags
}

proc underTop {top win} {
    if { "$top" == "." } {
	return $win
    } else {
	return $top$win
    }
}

proc deleteAllTraces {var} {
    foreach v [uplevel "#0" trace vinfo $var] {
	uplevel "#0" trace vdelete $var [lindex $v 0] [list [lindex $v 1]]
    }
}

# now unused
proc resetHistory { win list args } {
    set action [lindex $args 1]
    if { [catch {
	if { "$action" == "history" } {
	    $list delete 0 end
	    if { [winfo exists $list] } {
		foreach v [oget $win history] {
		    $list insert end [oget $v location]
		}
	    }
	}
	$list selection clear 0 end
	$list selection set [oget $win historyIndex]
	after 200 raise [winfo parent $list]

    } ] } {
	deleteAllTraces [oloc $win history]
	deleteAllTraces [oloc $win historyIndex]
    }
}

proc startDragPlacedWindow { win x y } {
    oset $win placeinfo [list $x $y [place info $win]]
}

proc dragPlacedWindow { win w1 x y } {
    global me recursive
    makeLocal $win placeinfo
    catch { after cancel [oget $win after]}
    set me [oget $win placeinfo]
    #puts "have=[oget $win placeinfo]"
    desetq "px py pinfo" [oget $win placeinfo]
    set dx [expr {$x - $px}]
    set dy [expr {$y - $py}]
    set nx [expr {$dx + [assoc -x $pinfo]}]
    set ny [expr {$dy + [assoc -y $pinfo]}]
    set new "-x $nx -y $ny"
    eval place $win $new
    oset $win placeinfo [list $x $y $new]
}

# now unused
proc OpenMathMoveHistory { win  n } {
    makeLocal $win history historyIndex
    incr historyIndex $n
    if { $historyIndex >= [llength $history] } {
	set historyIndex  [expr {[llength $history] -1}] 
    }
    if { $historyIndex < 0 } { set historyIndex 0}
    if { "[lindex $history $historyIndex]" != ""} {
	OpenMathGetWindow $win [lindex $history $historyIndex]
	oset $win historyIndex $historyIndex
    }
}

proc toLocalFilename { url } {
    set type [assoc type $url]
    switch -- $type {
	http {
	    return [assoc filename $url]
	}
	file {
	    return [file join / [assoc dirname $url] [assoc filename $url] ]

	}
        xmaxima {
            return [assoc filename $url]
        }
	default {error "unknown type: $type"}
    }
}

proc OpenMathGetWindow { commandPanel win } {
    if { "[winfo parent [oget $commandPanel textwin]]" != "$win" } {
	catch { pack forget [winfo parent [oget $commandPanel textwin]] }
	pack $win -expand 1 -fill both
	# pack $win
	oset $commandPanel textwin $win.text
	oset $commandPanel location [oget $win location]
	set tem [toLocalFilename [decodeURL [oget $win location]]]
	oset $commandPanel savefilename  [file root $tem].txt
    }
}

proc getw { s } {
    eval pack forget [winfo children . ] ; pack $s
}

proc try1 { file } {
    global ccc
    eval pack forget [winfo children . ]
    mkOpenMath [set w .t[incr ccc]]
    uplevel "#0" source $file
}

proc filesplit { x } {
    set l [split $x /]
    set n [llength $l ]
    set dir [lrange $l 0 [expr {$n - 2}]]
    set file [lindex $l [expr {$n - 1}]]
    return [list [join $dir /] $file]
}

proc decodeURL { name } {
    set server ""
    if { [regexp  {([^#]*)#(.*)$} $name junk name anchor] } {
	lappend answer anchor $anchor
	# puts "answer=$answer"
    }
    if { [regexp {^([a-z]+)[(]?([0-9]*)[)]?:/(.+)$} $name all type port path ] } {
	lappend answer type $type
    } else {
	set path $name ; set type ""
    }

    set path [removeDotDot $path]
    #puts "path=$path"
    desetq "dirname filename" [filesplit $path]
    #puts "dirname=$dirname,path=$path,filename=$filename"
    set po [assoc $type {http 80 nmtp 4443} ]
    if { "$po" != "" } {
	if { "$port" == "" } {set port $po }

	if { [regexp {^/([^/:]*)(:([0-9]+))?(.*)$} $dirname all server \
		  jun po dirname] } {
	    # puts "hi ther,server=$server"
	    if { "$po" != ""} {set port $po}
	    if { "$dirname" == "" } {set dirname / }
	} elseif { "$server" == "" } {
	    set server $filename
	    set dirname /
	    set filename {}
	}
	lappend answer port $port server $server
    }
    lappend answer dirname $dirname filename $filename
    return $answer
}

proc removeDotDot { path } {
    while { [regsub  {/[^/]+/[.][.](/|$)} $path "\\1" path] } {list}
    return $path
}

proc appendSeparate { var before item separator } {
    if { "$item" != "" } {
	uplevel 1 append $var $before $item $separator
    }
}

proc dirnamePlusFilename { lis } {
    return  [string trimright [assoc dirname $lis ""] /]/[assoc filename $lis ""]
}
proc encodeURL { lis } {
    set type [assoc type $lis ""]
    switch -- $type {
	nmtp {
	    if { [ set port [assoc port $lis 4443]] != 4443 } {
		append type "($port)"
	    }
	    appendSeparate ans "" $type ://[assoc server $lis ""]
	    append ans [dirnamePlusFilename $lis]
	    appendSeparate ans "#" [assoc anchor $lis ""] ""
	}
	http  {
	    if { [ set port [assoc port $lis 80]] != 80 } {
		append type "($port)"
	    }
	    appendSeparate ans "" $type ://[assoc server $lis ""]
	    append ans [dirnamePlusFilename $lis]
	    #appendSeparate ans "" [assoc dirname $lis ""]
	    #appendSeparate ans "/" [assoc filename $lis ""] ""
	    appendSeparate ans "#" [assoc anchor $lis ""] ""
	}
	file {
	    appendSeparate ans "" $type :/
	    append ans  [dirnamePlusFilename $lis]
	    #	   appendSeparate ans "" [assoc dirname $lis ""] "/"
	    #	   appendSeparate ans "" [assoc filename $lis ""] ""
	    appendSeparate ans "#" [assoc anchor $lis ""] ""
	}
        xmaxima {
            appendSeparate ans "" $type :/
            append ans  [dirnamePlusFilename $lis]
        }
	default {error "unsupported url type: $type"}
    }
    return $ans
}

proc resolveURL { name current {post ""} } {
    set decode [decodeURL $name]
    #puts "name=$name,current=$current"
    set ans ""
    set relative 0
    if { "[assoc type $decode {} ]" == "" } {set relative 1}
    if { $relative == 0 } {
	set ans  $decode
    } else {
	foreach {x y } $current {
	    switch -- $x {
		dirname {
		    set ndir [assoc dirname $decode ""]
		    set cdir [assoc dirname $current ""]
		    if { [string match /* $ndir] } {
			set new $ndir
		    } elseif { "$ndir" != "" } {
			if { "$cdir" != ""  } {
			    set new [string trimright $cdir /]/$ndir
			} else {
			    set new $ndir
			}
		    } else {
			set new $cdir
		    }
		    lappend ans dirname [removeDotDot $new]
		}
		filename {
		    if { "[assoc filename $decode]" == "" && "[assoc anchor $decode]" != "" } {
			lappend ans $x $y
		    }
		}
		post {
		    list
		}
		default {
		    lappend ans $x  [assoc $x $decode $y]
		}
	    }
	}
	foreach { key val } $decode {
	    if { "[assoc $key $ans --none--]" == "--none--" } {
		lappend ans $key $val
	    }
	}
    }
    if { "$post" != "" } {
	set ans [putassoc post $ans $post]
    }
    return $ans
}

proc getURLrequest { path server port types {post ""} {meth ""} } {
    if { "$meth" != "" } {
	set method $meth 
    } else {
	set method GET
	if { "$post" != "" } {set method POST}
    }
    #puts "getURLrequest $path $server $port [list $types]"
    foreach {v handler}  $::xmaxima_priv(urlHandlers) {
	lappend types $v,
    }
    set ans "$method $path HTTP/1.0\nConnection: Keep-Alive\nUser-agent: netmath\nHost: $server:$port\nAccept: $types\n"
    if { "$post" != "" } {
	# append ans "Content-length: [string length $post]\n\n$post"
	append ans "Content-type: application/x-www-form-urlencoded\nContent-length: [string length $post]\n\n$post"
    }
    return $ans
}

proc canonicalizeContentType { type } {
    regexp -nocase {([---a-zA-Z]+)/([---a-zA-Z]+)} $type type
    return [string tolower $type]
}

proc getURL { resolved type {mimeheader ""} {post ""} } {
    set res $resolved
    set ans ""
    set method ""
    if { "$mimeheader" != ""} {
	uplevel 1 set $mimeheader \[list\]
    }
    uplevel 1 set $type "unknown"
    #puts "getting $resolved,post=<$post>"
    switch [assoc type $res] {
	http {
	    #mike FIXME: replace with http get
	    # puts $res
	    # puts "socket [assoc server $res] [assoc port $res 80]"
	    if { [info exists ::xmaxima_priv(proxy,http) ] } {
		set sock [eval socket $::xmaxima_priv(proxy,http)]
		#		puts "opening proxy request socket $::xmaxima_priv(proxy,http)"
	    } else {
		set server [assoc server $res]
		set port [assoc port $res 80]
		#mike FIXME - use async sockets and dns
		if {[catch {socket $server $port} sock]} {
		    global errorInfo
		    tk_messageBox -title Error -icon error -message \
                        [mc "Error connecting to %s on %s\n%s" \
				      $server $port $sock]
		    return
		}
	    }
	    fconfigure $sock -blocking 0
	    ##DO NOT DELETE THE FOLLOWING !!!!!puts!!!!!!!!
	    #puts request=[getURLrequest [dirnamePlusFilename $res] [assoc server $res] [assoc port $res] image/gif $post]
	    #	    set path [dirnamePlusFilename $res]
	    set path [encodeURL $res]
	    set server [assoc server $res]
	    set port  [assoc port $res]
	    puts $sock [getURLrequest $path $server $port image/gif $post]
	    if { "$post" == "" } {
		oset $sock cachename "http://$server:$port$path"
	    } else {
		oset $sock cachename ""
	    }
	    flush $sock
	    if { [readAllData $sock -tovar ::xmaxima_priv(url_result) \
		      -translation binary -mimeheader ::xmaxima_priv(mimeheader)  \
		      -timeout 120000 -chunksize 2024] > 0 } {
		
		#puts "length=[string length $::xmaxima_priv(url_result)]"
		#	flush stdout
		set contentType [canonicalizeContentType [assoc content-type $::xmaxima_priv(mimeheader) text/plain]]
		uplevel 1 set $type [list $contentType]
		if { "$mimeheader" != "" } {
		    uplevel 1 set $mimeheader \[ uplevel "#0" set ::xmaxima_priv(mimeheader) \]
		}
		set ans $::xmaxima_priv(url_result)
		unset ::xmaxima_priv(url_result)
		return $ans
	    } else {
		return "had error"
	    }
	}
	file {
	    set name [toLocalFilename $res]
            if {[file isfile $name]} {
                set fi [open $name r]
            } else {
                set answer "<h1>Error</h1>\n<p>File: $name not found.</p>"
                set contentType text/html
                uplevel 1 set $type $contentType
                return $answer
            }
	    set answer [read $fi]
	    if { [regexp -nocase {[.]html?$} $name ] || [regexp -nocase "^(\[ \n\t\r\])*<html>" $answer] } {
		set contentType text/html
	    } elseif {  [regexp {[.]gif([^/]*)$} $name ] } {
		set contentType image/gif
	    } elseif {  [regexp {[.]png([^/]*)$} $name ] } {
		set contentType image/png
	    } elseif {  [regexp {[.]jpe?g([^/]*)$} $name ] } {
		set contentType image/jpeg
	    } else {
		set contentType text/plain
	    }
	    uplevel 1 set $type $contentType

	    close $fi
	    return $answer
	}
        xmaxima {
            set answer $::xmaxima_priv(error)
            set contentType text/html
            uplevel 1 set $type $contentType
            return $answer
        }
	default {
	    #mike dirpath?
	    error [concat [mc "not supported"] "[lindex $res 0]"]
	}
    }
}

proc getImage { resolved width height} {
    set res $resolved
    #puts [list getImage [list $resolved] $width $height]
    set ans ""
    catch {
	if { "" != "[image type $::xmaxima_priv(image,$res,$width,$height)]" } {
	    set ans $::xmaxima_priv(image,$res,$width,$height)
	}
    }
    if { "$ans" != "" } { return $ans }

    set image [image create photo -width $width -height $height]
    after 10 backgroundGetImage $image [list $resolved] $width $height
    set ::xmaxima_priv(image,$res,$width,$height) $image
    return $image
}

proc backgroundGetImage  { image res width height }   {
    #puts [list backgroundGetImage  $image $res $width $height ]
    if { [catch { backgroundGetImage1 $image $res $width $height } err ] } {
        set im ::img::brokenimage
	$image config -width [image width $im] -height [image height $im]
	$image copy $im
    }
}

proc backgroundGetImage1  { image res width height }   {
    #puts  "resolved=$res"
    #puts [list backgroundGetImage $image $res $width $height]
    switch [assoc type $res] {
	http {
	    set server [assoc server $res]
	    set port [assoc port $res 80]
	    if { [info exists ::xmaxima_priv(proxy,http) ] } {
		set s [eval socket $::xmaxima_priv(proxy,http)]
		#		puts "opening proxy request socket $::xmaxima_priv(proxy,http)"
	    } else {
		set s [socket [assoc server $res] [assoc port $res 80]]
	    }
	    fconfigure $s -blocking 0
	    ##DO NOT DELETE THE FOLLOWING !!!!!puts!!!!!!!!
	    puts $s [getURLrequest [encodeURL $res] \
			 $server $port {image/gif image/png image/jpeg image/x-bitmap}]
	    flush $s


	    if { [regexp -nocase $::xmaxima_priv(imgregexp) [assoc filename $res] mm extension] } {
		fconfigure $s -translation binary
		set tmp xxtmp[incr ::xmaxima_priv(imagecounter)].$extension

		if { [info exists ::xmaxima_priv(inbrowser)] ||  [catch {set out [open $tmp w] } ] } {
		    # if have binary..
		    if { "[info command binary]" != "binary" } {
			error [mc "need version of tk with 'binary' command for images"]}
		    #puts "hi binary" ; flush stdout
		    if {  [readAllData $s -tovar \
			       ::xmaxima_priv($s,url_result) -mimeheader \
			       ::xmaxima_priv($s,mimeheader)
			  ] > 0  && [string match *$extension [assoc content-type $::xmaxima_priv($s,mimeheader)]] } {
			set ans $image
			$image configure -data [tobase64 $::xmaxima_priv($s,url_result)]

			unset ::xmaxima_priv($s,mimeheader)
			unset ::xmaxima_priv($s,url_result)
			
		    } else  {
			error [mc "could not get image"]
		    }
		} else {
		    fconfigure $out -translation binary -blocking 0
		    if { [readAllData $s -tochannel $out \
			      -translation binary \
			      -mimeheader \
			      ::xmaxima_priv($s,mimeheader) -timeout 15000 -chunksize 2024 ] > 0 } {
			set ans $image
			$image config  -file \
			    $tmp
			unset ::xmaxima_priv($s,mimeheader)
		    }
		    # all the below just to try to remove the file..
		    #  depending on versions and in environments..
		}
	    }
	}
	file {
	    $image config -file [toLocalFilename $res]
	    set ans $image
	    # puts "$image config -file [toLocalFilename $res]"
	    #set ans [image create photo -file [toLocalFilename $res]]
	
	
	}
	default { error [mc "unknown type of image"] }
    }
    ## if we opened an out channel try hard to remove the tmp file.
    if { [info exists out] &&
	 [catch { file delete $tmp } ] && [catch { rm $tmp }]
	 && [catch { exec rm $tmp }] } {
	puts [concat [mc "cant remove tmp file"] "$tmp"]
    }
    if { "$ans" == "" } {
	error [concat [mc "Unable to open an image for"] "[encodeURL $res]"]
    }

}
#
#-----------------------------------------------------------------
#
# readData --  read data from S, storing the result in
# ::xmaxima_priv($s,url_result).   It times out after TIMEOUT without any
# data coming.
# It can be aborted by setting set ::xmaxima_priv($s,done)  -1
#
#
#  Results: -1 on failure and 1 on success.
#
#  Side Effects: it initially  empties ::xmaxima_priv($s,url_result) and then
#  adds data to it as read.   ::xmaxima_priv($s,done) is initialized to 0
#
#----------------------------------------------------------------
#
proc readData { s { timeout 10000 }} {
    after $timeout "set ::xmaxima_priv($s,done) -1"
    fconfigure $s  -blocking 0
    set ::xmaxima_priv($s,done) 0
    set ::xmaxima_priv($s,url_result) ""

    #mike FIXME: this is a wrong use of after cancel
    fileevent $s readable \
	"after cancel {set ::xmaxima_priv($s,done) -1} ; after $timeout {set ::xmaxima_priv($s,done) -1} ; set da \[read $s 8000] ; append ::xmaxima_priv($s,url_result) \$da; if { \[string length \$da] < 8000  && \[eof $s] } {after cancel {set ::xmaxima_priv($s,done) -1} ; set ::xmaxima_priv($s,done) 1; fileevent $s readable {} ;  }"
    myVwait ::xmaxima_priv($s,done)
    catch { close $s }
    #mike FIXME: this is a wrong use of after cancel
    after cancel "set ::xmaxima_priv($s,done) -1"
    return $::xmaxima_priv($s,done)
}

proc doRead { sock } {
    #puts reading; flush stdout;
    set tem [read $sock]
    append ::xmaxima_priv(url_result)  $tem
    # puts read:<$tem>
    # flush stdout
    if { [eof $sock] } {
	set ::xmaxima_priv(done) 1
	close $sock
    }
}

proc tempName { name extension } {
    set count [pid]
    while { [file exists $name[incr count].$extension] } { list }
    return $name$count.$extension
}

proc ws_outputToTemp { string file ext encoding } {
    upvar 1 $string result
    set tmp [tempName $file $ext ]
    set open $tmp
    if { [lsearch {x-gzip x-compress}  $encoding] >= 0 } { 
	# FIXME: Unix only
	lappend dogzip |gzip -dc > $open ; set open $dogzip
    }
    set fi [open $open w]
    fconfigure $fi -translation binary
    puts -nonewline $fi $result
    flush $fi
    close $fi
    return $tmp
}

proc OpenMathOpenUrl { name args} {
    if {![winfo exists .browser]} {createBrowser .browser}
    
    # Removes any white spaces at the end of the Url given
    set name [string trimright $name]

    maxStatus [concat [mc "Opening"] "$name"]

    #puts "OpenMathOpenUrl  $name $args "
    set history "" ; set historyIndex -1 ; set currentUrl ""
    set prevwindow ""
    set commandPanel [assoc -commandpanel $args ]
    if { "$commandPanel" == "" } {
	linkLocal . omPanel
	if { [info exists omPanel] } {
	    set commandPanel $omPanel
	}
    }
    set toplevel [assoc -toplevel $args ""]
    if { "$toplevel" == "" } {set toplevel ".browser"}
    if { "$toplevel" == "." } {set toplevel ""}
    set reload [assoc -reload $args 0]
    set post [assoc -post $args ""]
    #puts "post=$post"
    if { [winfo exists $commandPanel ] }  {
	makeLocal $commandPanel history historyIndex textwin
#	set toplevel [winfo paren $commandPanel]
#	if { "$toplevel" == "." } {set toplevel ""}
	# eval pack forget [winfo parent $textwin ]
	set prevwin [winfo parent $textwin]
	set currentUrl [oget $textwin currentUrl]
	catch { set currentUrl [decodeURL [oget $textwin baseurl]] }

	if { $reload == 0} {
	
	    set new [resolveURL $name $currentUrl $post]
	    if { [set anchor [assoc anchor $new]] != "" } {
		set new [delassoc anchor $new]
	    }
	    set ii -1
	    foreach v $history {
		incr ii
		if { "[delassoc post $new]" == "[delassoc post [oget $v.text currentUrl]]" } {
		    # puts "new=$new\nold=[oget $v.text currentUrl]"
		}
		if   { "$new" == "[delassoc anchor [oget $v.text currentUrl]]" } {
		    OpenMathMoveHistory $commandPanel [expr {$ii - $historyIndex }]
		    if { "$anchor" != "" } {
			update
			catch {  $v.text yview anchor:$anchor }
		    }
		
		    #    OpenMathGetWindow $commandPanel $v
		    #    pushHistory $commandPanel $v
		    return
		}
	    }
	} else {
	    # reload=1
	    list
	}
    }
    set count 5
    while { [incr count -1] > 0 } {
	set new [resolveURL $name $currentUrl $post]
	set result [getURL $new contentType mimeheader $post]
	if { [set tem [assoc location $mimeheader]] == "" } {
	    break
	}
	set name $tem
    }
    #puts "contentType defined:[info exists contentType]"
    set handler [assoc $contentType $::xmaxima_priv(urlHandlers)]
    if { "$handler" != "netmath" && "$handler" != "" } {
	set tmp [ws_outputToTemp result netmath ps "[assoc content-encoding $mimeheader]"]
	# to do fix this for windows #####
	exec sh -c "[format $handler $tmp] ; rm -f $tmp" &
	return
    }
    #puts contentType=$contentType
    #puts "got [string length $result] bytes"
    #puts ", result= [string range $result 0 70] .."

    if { [catch { set baseprogram [oget $textwin baseprogram] }] } {
	set baseprogram [decodeURL [getBaseprogram]]
    }
    # puts "using  $baseprogram"
    if { $reload } {   forgetCurrent $commandPanel }

    #puts "::xmaxima_priv(counter)=$::xmaxima_priv(counter)"
    set win [mkOpenMath [set w $toplevel.t[incr ::xmaxima_priv(counter)]] ]

    #puts "::xmaxima_priv(counter)=$::xmaxima_priv(counter)"
    makeLocal $w commandPanel
    #puts "resolveURL $name $currentUrl"
    if { [set anchor [assoc anchor $new]] != "" } {
	set new [delassoc anchor $new]
    }
    if { "[assoc filename $new]" == "" } {
	set new [putassoc  filename $new index.html]
    }
    # puts "...> $new"
    oset $w.text currentUrl $new
    oset $commandPanel location [encodeURL $new]
    oset $commandPanel textwin $win
    oset $w location  [encodeURL $new]
    # puts "new=$new"
    oset $commandPanel savefilename [file root [toLocalFilename $new]].txt
    set tem [assoc filename $new ""]
    #puts $contentType
    if { "$contentType" != "text/html" } {
	if { [string match "image/*" $contentType] } {
	    set im [image  create photo -data $result]
	    $win image create 0.0 -image $im
	    set err 0
	} else {
	    set err [catch {   $win insert 0.0 $result } ]
	}
    } elseif { 1 }  {
	xHMinit_win $win
	xHMset_state $win url [encodeURL $new]
	oset $win baseprogram $baseprogram
	# puts win=$win,lengres=[string length $result]
	set errmsg1 ""
	set err 0
	global debugParse
	if { $debugParse } {
	    xHMparse_html $result "xHMrender $win"
	    set err 0
	} else {
	    set err [catch {
		xHMparse_html $result "xHMrender $win"
	    } errmsg1 ]
	}
	catch {
	    if { "$anchor" != "" } {
		update
		$win yview anchor:$anchor
	    }
	}
	#   foreach v {Tresult Teval} {  $win tag raise $v}	
    } else {
	###Never get here.. must change to make be the rich text case..	
	# drop comment lines
	regsub -all "(^|\n)#\[^\n\]*\n" $result \n result ;
	#puts input=$result
	
	# note netscape would just truncate the history
	# at historyIndex, and start to grow it there,
	# losing the record of all files you have visited after..
	# maybe we should do this.
	#puts "history=$history"
	set err [catch { insertRichText $win insert $result }]
    }
    if { $err == 0 } {
	pushHistory $commandPanel $w
    }
    if { $err } {
	global errorInfo
	#puts "======begin======"
	#puts $result
	#puts "======end========"
	puts "$errmsg1"
	error [concat [mc "unable to evaluate"] "[encodeURL $new]\n$errmsg1\n$errorInfo"]
    }
}

proc pushHistory { commandPanel win } {
    global [oarray $commandPanel]
    makeLocal $commandPanel history historyIndex

    if { [llength $history] == 0 } {
	oset $commandPanel historyIndex -1
    }
    if { "[lindex $history $historyIndex ]" != "$win" } {
	oset $commandPanel history [linsert $history [incr [oloc $commandPanel historyIndex]] $win]
    }
}
#
#-----------------------------------------------------------------
#
# omScrollPage --  scroll the page by N pages, keeping the insert
# cursor visible.
#
#  Results: none
#
#  Side Effects: page scrolls
#
#----------------------------------------------------------------
#
proc omScrollPage { win n } {
    tkTextScrollPages $win $n
    set bbox [$win bbox insert]
    if { "" == "$bbox" } {
	if { $n > 0 } {
	    $win mark set insert @0,0
	} else {$win mark set insert @0,[$win cget -height]}
    }
}

proc addTagSameRange { win oldtag newtag index } {
    if { [lsearch [$win tag names $index] $oldtag ] >= 0 } {
	set this [$win tag prevrange $oldtag $index+1char]
	if { "$this" != "" && [$win compare $index < [lindex $this 1]] } {
	    $win tag remove $newtag 0.0 end
	    $win tag add $newtag [lindex $this 0] [lindex $this 1]
	    $win tag raise $newtag
	}
    }
}

proc getBaseprogram { } {
    return [lindex  $::xmaxima_default(defaultservers) 0]
}

#mike FIXME: This is an abomination
proc fileBaseprogram { textwin parent x y } {
    set e $textwin.e
    catch { destroy $e }
    set x [expr {[winfo rootx $parent] + $x +30 - [winfo rootx $textwin]} ]
    set x 30
    set y [expr {[winfo rooty $parent] + $y - [winfo rooty $textwin]} ]
    global xHMpriv
    set xHMpriv(baseprogram) [encodeURL [oget $textwin baseprogram]]
    entry $e -width 40 -textvariable xHMpriv(baseprogram)
    place $e -in $textwin -x $x -y $y
    raise $e
    set com "destroy $e ; oset $textwin baseprogram \[decodeURL \$xHMpriv(baseprogram)] "
    bind $e <Leave> $com
    bind $e <Return> $com
}

proc fontDialog { top } {
    set font [xHMmapFont font:propor:normal:r:3]
    if {[winfo exists $top]} {catch { destroy $top }}

    toplevel $top
    wm iconify  $top

    set win $top.text
    text $win -font [list [font config $font -family] [font config $font -size]] -height 20
    wm deiconify $top

    foreach fam {propor fixed} {
	set lis ""
	set i 0
	while { $i <= 8 } {
	    lappend lis [expr {$i - 3}]
	    incr i
	}
	if { "$fam" == "fixed" } { set fixed 1 } else {
	    set fixed 0
	}
	mkLabelListBoxChooser $win.size$fam "list $lis" ::xmaxima_default($fam,adjust)
	mkLabelListBoxChooser $win.family$fam "getFontFamilies $fixed " ::xmaxima_default($fam)
	set fo [xHMmapFont "font:$fam:normal:r:3"]
	catch { set ::xmaxima_default($fam) [assoc -family [font actual $fo]]}
    }
    $win insert insert [mc "Font Settings\nThe proportional font is "]
    $win window create insert -window $win.familypropor
    $win insert insert [mc "with a size adjustment of "]
    $win window create insert -window $win.sizepropor
    $win insert insert [mc "\nThe fixed font is "]
    $win window create insert -window $win.familyfixed
    $win insert insert [mc "with a size adjustment of "]
    $win window create insert -window $win.sizefixed
    $win insert insert "\n"
    $win insert insert [mc "Default nmtp servers  "]
    global _servers
    set _servers $::xmaxima_default(defaultservers)
    entry $win.entry -textvariable _servers -width 40
    $win window create insert -window $win.entry
    $win insert insert "\n\n"
    $win insert insert [mc "http Proxy host and port:"]
    entry $win.entryproxy  -width 40
    catch { $win.entryproxy insert 0 $::xmaxima_priv(proxy,http) }
    $win window create insert -window $win.entryproxy
    $win insert insert [mc "\nIf you are behind a firewall enter the name of your http proxy host and port,\n eg: `some.server.example.org 3128', otherwise leave this blank"]

    set men [tk_optionMenu $win.plottype ::xmaxima_default(plotwindow) embedded separate multiple ]
    $win insert insert [mc "\nShould plot windows be "]
    $win window create insert -window $win.plottype
    $win insert insert "?"
    $win insert insert "\n\n\n"
    $win insert insert [mc " Apply and Quit "] "bye raised"
    $win insert insert "      "
    $win insert insert [mc " Apply "] "click raised"
    $win insert insert "      "
    $win insert insert [mc " Cancel "] "cancel raised"
    proc _FontDialogApply { win } {
	global _servers ::xmaxima_priv
	set ::xmaxima_default(defaultservers) $_servers
	catch {xHMresetFonts .}
	if { [llength [$win.entryproxy get]] == 2 } {
	    set ::xmaxima_priv(proxy,http) [$win.entryproxy get]
	}
    }
    $win tag bind click <1> "_FontDialogApply $win"
    $win tag bind bye <1> "_FontDialogApply $win ; destroy $top"
    $win tag bind cancel <1> "destroy $top"
    $win tag configure raised -relief raised -borderwidth 2
    $win insert insert "      "
    $win insert insert [mc " Save Preferences "] "save raised"
    $win tag bind save <1> "_FontDialogApply $win ; savePreferences"
    pack $win
    #    place $win -in [oget [omPanel .] textwin] -x 10 -y 10
}

proc savePreferences {} {
    makeLocal {.maxima.text} inputs

    # Save current console size in ::xmaxima_default
    set console [lindex [array get ::xmaxima_priv cConsoleText] end]
    set ::xmaxima_default(iConsoleWidth) [textWindowWidth $console]
    set ::xmaxima_default(iConsoleHeight) [textWindowHeight $console]

    catch {
        # Save the state of the browser (1=opened, 0=closed)
        if {[winfo exists .browser]} {
            set ::xmaxima_default(browser) 1
        } else {
            set ::xmaxima_default(browser) 0}

        # Save the ::xmaxima_default array into $::xmaxima_priv(conffile)
        # Each line will contain a key and value separated by space
        # and the keys will be in alphabetical order
        #
        set fileId [open $::xmaxima_priv(conffile) w]
        foreach {k v} [array get ::xmaxima_default *] {
            lappend all [list $k $v]
        }
        set all [lsort $all]
        foreach v $all { puts $fileId $v }

        #mike FIXME: make this a _default
        # if { [info exists ::xmaxima_priv(proxy,http)] && \
        #          [llength $::xmaxima_priv(proxy,http)] == 2   } {
        #     puts $fileId [list array set ::xmaxima_priv [array get ::xmaxima_priv proxy,http]
	# 	 ]
        # }   Villate: This block seems wrong to me.
        
        close $fileId

        # Save the command history to $::xmaxima_priv(history)
        # Up to 200 last commands are saved, each one as a Tcl list
        #
        set fileId [open "$::xmaxima_priv(history)" w]
        foreach v [lrange $inputs end-199 end] { puts $fileId [list $v] }
        close $fileId
    }
}
#
#-----------------------------------------------------------------
#
# mkLabelListBoxChooser --  creates a button called WIN with textvariable
#  $TEXTVAR.  When clicked on the WIN, brings down
#  a list of items, and clicking on one of them selects that item. and
#  resets $TEXTVAR
#
#  Results: none
#
#  Side Effects: the TEXTVAR value is changed, and so consequently the label.
#
#----------------------------------------------------------------
#
proc mkLabelListBoxChooser { win items  textvar} {
    button $win -textvariable $textvar -command "listBoxChoose $win [list $items] $textvar"
}

proc listBoxChoose { win  items textvar  } {
    set whei [winfo height $win]
    set items [eval $items]
    set hei [llength $items]
    set fr ${win}frame
    frame ${win}frame
    set list $fr.list
    set scroll $fr.scroll
    scrollbar $scroll -command "$list yview"
    listbox $list -yscroll "$scroll set" -setgrid 1 -height 8
    pack $scroll -side right -fill y
    pack $list -side left -expand 1 -fill both
    set wid 0
    foreach v $items {
	set xx [string length $v] ;
	set wid [expr {($xx > $wid ? $xx : $wid)}]
    }
    eval [concat $list insert 0 $items]
    catch { $list selection set [lsearch $items [set $textvar]] }
    bind $list <1> "set $textvar \[$list get \[$list nearest %y\]\]; destroy $fr"
    place $fr -in $win -x 0  -y 0 -anchor n
}

proc quoteForRegexp { s } {
    regsub -all {[\]\[$+()\\.?*]} $s {\\\0}  ans
    return $ans
}


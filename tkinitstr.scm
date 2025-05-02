;; tkinitstr.scm

(define tkinitstr #<<END
package require Tk
if {[package version tile] != ""} {
    package require tile
}

wm state . iconic

namespace eval AutoName {
    variable c 0
    proc autoName {{result \#\#}} {
        variable c
        append result [incr c]
    }
    namespace export *
}

namespace import AutoName::*

proc callToScm {callKey args} {
    global scmVar
    try {
        set resultKey [autoName]
        puts "(call $callKey \"$resultKey\" $args)"
        flush stdout
        vwait scmVar($resultKey)
        set result $scmVar($resultKey)
        unset scmVar($resultKey)
        set result            
    } on error {e} {
        exit 1
    }
}

proc tclListToScmList {l} {
    switch [llength $l] {
        0 {
            return ()
        }
        1 {
            if {[string range $l 0 0] eq "\#"} {
                return $l
            }
            if {[regexp {^[0-9]+$} $l]} {
                return $l
            }
            if {[regexp {^[.[:alpha:]][^ ,\"'\[\]\\;]*$} $l]} {
                return $l
            }
            set result \"
            append result\
                [string map [list \" \\\" \\ \\\\] $l]
            append result \"

        }
        default {
            set result {}
            foreach el $l {
                append result " " [tclListToScmList $el]
            }
            set result [string range $result 1 end]
            return "($result)"
        }
    }
}

proc evalCmdFromScm {cmd {properly 0}} {
    try {
        set result [uplevel \#0 $cmd]                                      
        if $properly {
            puts "(return [tclListToScmList $result])"
        } else {
            puts "(return \"[string map [list \\ \\\\ \" \\\"] $result]\")"
        }
    } on error {err} {
        puts "(error \"[string map [list \\ \\\\ \" \\\"] $err]\")"
    }
    flush stdout
}
END
)

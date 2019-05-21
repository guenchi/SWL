set op [open tclversion.ss "w"]
set vers [set tcl_version]
puts $op "(define tcl_version \"$vers\")"
exit

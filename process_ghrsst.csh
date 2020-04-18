#!/bin/csh -f

if ($#argv == 0 | "$1" =~ -*h*) then
    echo "Usage: $0:t filelist"
    echo "Processes a list of GHRSST files, one per line, by calling"
    echo "'ghrsst --sst -g geo_em.d01.nc file' on each, then calling"
    echo "interp-intermediate to interpolate to interval_seconds as"
    echo "found in namelist.wps in the current directory."
    echo "Designed to be run in a WPS runtime directory."
    exit
endif

if !(-e geo_em.d01.nc) then
    echo "Error: No geo_em.d01.nc file found. "
    echo "Edit this script to (pick one):"
    echo "  1. Remove the '-g geo_em.d01.nc' to process globally"
    echo "  2. Give a path to geo_em.d01.nc"
    echo "  3. Make a soft link (ls -s) to geo_em.d01.nc."
    exit
endif

if !(-e namelist.wps) then
    echo "Error: No namelist.wps file found. "
    echo "Edit this script to hard-code the time interval, instead of "
    echo "converting from interval_seconds grepped from namelist.wps."
    exit
endif

set verb = 0 # 0 = shut up, 1 = be verbose
if ("$1" =~ -*v* | "$1" =~ -*d*) then # matches -v, --verbose, -d, --debug
    set verb = 1
    shift # get rid of -v
endif

set debug = ""
if ($verb) set debug = "--debug "

set ghrsst_files = (`cat $1`)

foreach file ($ghrsst_files)
    if ($verb) echo "Converting $file"
    ghrsst-to-intermediate $debug --nolandsea --sst -g geo_em.d01.nc $file >>&! process_ghrsst.log
end

set files = (SST:*) # FIXME: this needs to be more general, to handle SST: and SEAICE:

if ($verb) then
    echo "Done with ghrsst-to-intermediate, have these files:"
    ls -1 $files
endif

set interval_seconds = `grep interval_seconds namelist.wps | cut -d= -f2`
set interval_hours = `echo "$interval_seconds / 3600" | bc`

set param = `echo $files[1] | cut -d: -f1` # just SST from SST:YYYY-MM-DD_HH

#set n = `echo "$#files - 1" | bc` # could use '@ n = $#files - 1'
@ n = $#files - 1

if ($verb) \
    echo "Interpolating to $interval_hours hours between $#files files ($n calls)."

foreach i (`seq 1 $n`)

    @ ip1 = $i + 1
    if ($verb) echo -n "  Interval $i, "

    set time1 = `echo $files[$i]   | cut -d: -f2` # starting and ending times
    set time2 = `echo $files[$ip1] | cut -d: -f2` # for these two files

    if ($verb) echo -n "between $time1 and $time2, "

    set now  = `add_time -1 $time1 $interval_hours` # first output, YYYYMMDDHH
    set end2 = `add_time -1 $time2 0` # convert from YYYY-MM-DD_HH to YYYYMMDDHH

    if ($verb) echo "starting at $now, ending $end2"

    while ($now < $end2) # needs YYYYMMDDHH to loop correctly

	set timeOut = `add_time --wrf $now 0` # convert to YYYY-MM-DD_HH
	
	if ($verb) echo "    Creating ${param}:$timeOut from $files[$i] and $files[$ip1]"
	interp-intermediate $debug -i $files[$i] $files[$ip1] -o ${param}:$timeOut
	echo Interpolated to create ${param}:$timeOut >>&! process_ghrsst.log

	set now = `add_time -1 $now $interval_hours` # YYYYMMDDHH

    end
end

# WPS-ghrsst-to-intermediate
Converts JPL PODAAC GHRSST netCDF files to WPS Intermediate version 5 files

This program is designed to replace running WPS's ungrib program on an
SST dataset. It works on the netCDF files from the GDS version 2
GHRSST datasets available at the JPL PODAAC:

https://podaac.jpl.nasa.gov/datasetlist?ids=ProcessingLevel&values=*4*&search=GHRSST&view=list

This program may also work on GHRSST datasets that don't have
"GDS version 2" in the name. If you try it and it works, or have some
code changes that make it work, please let me know.

There are both global and local GHRSST datasets.

If your chosen dataset is local, and some of your WRF domains are
outside of the GHRSST data coverage area, then you'll want to retain
the SST fields in whatever other datasets you're using for the rest of
the required inputs to WRF.

If your chosen dataset is global, then you may optionally NOT ungrib the 
SST fields from whatever other datasets you're using for the rest of the
required intput to WRF. Do that by eliminating the line containing
"SST" from the Vtable:
      sed -i '/\<SST\>/d' Vtable

Type "ghrsst-to-intermediate --help" for usage.

Pass a text file with a list of path/to/GHRSST_files, one per line, to
"process_ghrsst.csh" to convert all the GHRSST files in the list, and
interpolate to the value of interval_seconds grepped from
namelist.wps. It also assumes the file (or a link to) geo_em.d01.nc in
the current directory, to extract the minimum domain in the
SST:YYYY-MM-DD_HH files. I call the script from my WPS generator
script.

COMPILING

A sample makefile is included, with some commented-out blocks for different
compilers.

I have one report that "integer, external" should be "integer, intrinsic" 
for gfortran (gcc). That's the only change to the *.f90 files you might
need to make to compile the program.

Bart Brashers
bbrashers@ramboll.com
2020-06-02

!da ------  
 2020-06-10  
 The 'LANDSEA' was repalced with 'SST_MASK'. 
 I need to move forward call to read 'analysed_sst' and create "mask_in", instead of reading 'mask' from ghrsss,
 because mur_sst file   has no mask field, only the 'bad' grid-cells it has.
!da-------- 

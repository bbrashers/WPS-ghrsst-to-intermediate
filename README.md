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

Bart Brashers
bbrashers@ramboll.com
2020-04-16

###################
20230504:!da v1.5 updates at https://github.com/dmitryale/WPS-ghrsst-to-intermediate
!cd ~/MUR_SST/WPS-interp-intermediate-master/; make; cp -p ghrsst-to-intermediate ../. ;
!cd .. ; ./ghrsst-to-intermediate --sst -l -d -g geo_em.d01.nc out/JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1_20230503.nc
!da: 2023-05-04 version 1.5  : MUR_SST updated on 20230425 (real SST in-situ degC; seconds since 1970-01-01;
!da: 'where' replaced with do i=1,nx ... cycles; Use new repository  wget_mur_sst.sh =>
!da: di=3; CDAY5=$(date +"%Y%m%d" -d "-${di} day");  CDAY_5=$(date +"%Y-%m-%d" -d "-${di} day") ; day="${CDAY_5}T09:00:00Z" ;
! fnm=JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1_${CDAY5}.nc ; lon1=-23.0 ; lon2=0.0 ; lat1=22.0 ; lat2=38.0 ;
! URL="https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst%5B($day):1:($day)%5D%5B($lat1):1:($lat2)%5D%5B($lon1):1:($lon2)%5D"
! wget $URL -O out/$fnm

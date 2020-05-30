program ghrsst_to_intermediate
!------------------------------------------------------------------------------
! Written by Bart Brashers, Ramboll, bbrashers@ramboll.com.
! Last update: 2020-05-29 version 1.2
! Converts the data found at 
!   https://podaac-opendap.jpl.nasa.gov/opendap/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/
!   https://podaac-opendap.jpl.nasa.gov/opendap/allData/ghrsst/data/L4/GLOB/JPL_OUROCEAN/G1SST/
!   (and probably many more of their GHRSST datasets)
! to WPS intermediate format (metgrid-ready format).
! See 
!   https://podaac-tools.jpl.nasa.gov/drive/files/OceanTemperature/ghrsst/docs/GDS20r5.pdf
!   https://podaac.jpl.nasa.gov/datasetlist?ids=ProcessingLevel&values=*4*&search=GHRSST&view=list
!   https://podaac.jpl.nasa.gov/dataset/JPL_OUROCEAN-L4UHfnd-GLOB-G1SST
!   https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1
!
!----------------------------------------------------------------------
! This program is free software, using GNU General Public License v3.0.
! See the file COPYING, or https://www.gnu.org/licenses/gpl-3.0.en.html
!----------------------------------------------------------------------

  implicit none
  include 'netcdf.inc'

  logical :: wind_grid_rel = .false.  ! Flag indicating whether winds are  
                                      !   relative to source grid (TRUE) or
                                      !   relative to earth (FALSE)
  logical :: debug = .false.          ! set to .true. to debug
  logical :: write_sst = .false.      ! Flag to output SST:YYYY-MM-DD_HH files
  logical :: write_ice = .false.      ! Flag to output SST:YYYY-MM-DD_HH files
  logical :: write_landsea = .false.  ! Flag to output LANDSEA:YYYY-MM-DD_HH files
  logical :: append_landsea = .true.  ! Flag to append the LANDSEA to SST: and ICE:
  logical :: crosses_180E = .false.   ! Flag that the WRF domain crosses lon = 180
  integer, external  :: iargc
  integer            :: iarg          ! arguement index
  integer, parameter :: version = 5   ! Format version (must =5 for WPS format)
  integer, parameter :: iProj = 0     ! Projection type is lat-lon
  integer            :: nx,ny         ! size of the array
  integer            :: bad_value     ! ncfile's bad value flag
  integer            :: year,month,day,hour,jday  ! time-related
  integer            :: ibeg,iend,jbeg,jend       ! output sub-domain range
  integer            :: imid                      ! the "middle" of i-range, lons
  integer            :: i,j,rcode, ivtype 
  integer            :: cdfid, id, ndims,natts, idims(10),dimids(10)
  integer            :: istart1,iend1, istart(3),istop(3)
  integer            :: geoflag       ! 0 = none, 1 = geo_em given, 2 = bounds given

  integer (kind=2), allocatable, dimension(:,:) :: sst_in   ! 16-bit signed integer
  integer (kind=1), allocatable, dimension(:,:) :: ice_in   !  8-bit integer (byte)
  integer (kind=1), allocatable, dimension(:,:) :: mask_in  !  8-bit integer (byte)
  real, allocatable, dimension(:,:) :: sst         ! output (scaled) SST
  real, allocatable, dimension(:,:) :: ice         ! output (scaled) sea ice fraction
  real, allocatable, dimension(:,:) :: landsea     ! output land-sea mask
  real, allocatable, dimension(:,:) :: landseaice  ! output land-seaice mask
  real, allocatable, dimension(:)   :: lats,lons   ! output locs, 1D vectors
  real, allocatable, dimension(:)   :: temp1d      ! for re-shaping 
  real, allocatable, dimension(:,:) :: temp2d      ! for re-shaping 
  real, allocatable, dimension(:,:) :: lat2d,lon2d ! geo_em XLAT_V,XLONG_U

  real,    parameter :: bad = -1.e+30 ! value found in FILE:* from NAM12
  real,    parameter :: earth_radius = 6371.
  real,    parameter :: tlat1 = 0.    ! not used for iProj = 0 (lat-lon)
  real,    parameter :: tlat2 = 0.    ! not used for iProj = 0 (lat-lon)
  real,    parameter :: xlonc = 0.    ! only used form iProj = 3 (LCC) or 5 (PS)
  real :: add_offset, scale_factor    ! to convert sst_in (int) to sst (real)
  real :: valid_min,  valid_max       ! check range returned from NC file

! The variables needed to sub-set output using geo_em.d01.nc:

  real, dimension(16) :: corner_lats, corner_lons ! geo_em global attributes
  real                :: min_lat, max_lat, min_lon, max_lon
  real                :: sw_lon,  nw_lon,  se_lon,  ne_lon

! The variables needed to write in WPS intermediate file format:

  real :: nlats                       ! Number of latitudes north of equator
  real :: xfcst                       ! Forecast hour of data
  real :: xlvl                        ! Vertical level of data in 2-d array
  real :: startlat, startlon          ! Lat/lon of point in array of startloc
  real :: deltalat, deltalon          ! Grid spacing, degrees
  real :: dx, dy                      ! Grid spacing, km
                                                                        
  character (len=80)  :: varnam       ! returned variable name from NC file
  character (len=8)   :: startloc     ! Which point in array is given by
                                      !   startlat/startlon; set either 
                                      !   to 'SWCORNER' or 'CENTER  '
  character (len=9)   :: field        ! Name of the field
  character (len=10)  :: date_str     ! "YYYY-MM-DD"
  character (len=12)  :: time_str     ! "HH:mm:ss UTC"
  character (len=50)  :: res_str      ! resolution description
  character (len=24)  :: hdate        ! Valid date for data YYYY:MM:DD_HH:00:00
  character (len=25)  :: units        ! Units of data
  character (len=32)  :: map_source   ! Source model / originating center
  character (len=46)  :: desc         ! Short description of data
  character (len=256) :: ncfile       ! input netCDF file downloaded from PODAAC
  character (len=256) :: geofile      ! output from WPS' geogrid.exe (outer domain)
  character (len=256) :: arg,filename ! temporary storage
  character (len=100) :: prog_version ! 

!----Set defaults

  geoflag = 0 ! default to no geo_em or bounds specified
  prog_version = "ghrsst_to_intermediate-0.2"

!----Process the command line

  if (iargc() == 0) call usage
  iarg = 0
  do while (iarg < iargc())
     iarg = iarg + 1
     call getarg(iarg,arg)
     if (arg == '-h' .or. arg == '--help') then
        call usage
     else if (arg == '-d' .or. arg == '--debug') then
        debug = .true.
        write(*,*) "Debug mode turned on."
     else if (arg == '-V' .or. arg == '--version') then
        write(*,*) prog_version
        stop
     else if (arg == '-s' .or. arg == '--sst') then
        write_sst = .true.
     else if (arg == '-i' .or. arg == '--seaice') then
        write_ice = .true.
     else if (arg == '-l' .or. arg == '--landsea') then
        write_landsea = .true.
     else if (arg == '-n' .or. arg == '--nolandsea') then
        append_landsea = .false.
     else if (arg == '-g') then
        iarg = iarg + 1
        call getarg(iarg,geofile)
        geoflag = 1 ! a geo_em.nc file was given
     else if (arg == '-b') then
        iarg = iarg + 1
        call getarg(iarg,arg)
        read(arg,*) min_lat,max_lat,min_lon,max_lon
        min_lat = floor(min_lat) ; max_lat = ceiling(max_lat) ! round
        min_lon = floor(min_lon) ; max_lon = ceiling(max_lon) ! up/down
        geoflag = 2 ! bounds were given
     else
        call getarg(iarg,ncfile)
     endif
  end do

  if ((.not. write_sst) .and. (.not. write_ice) .and. (.not. write_landsea)) then
     write(*,*) 
     write(*,*) "No outputs requested, nothing to do, quitting."
     write(*,*) "Specify one of -s|--sst, -i|--ice, or -l|--landsea."
     write(*,*) 
     call usage ! calls stop
  endif

! Process the geo_em.d01.nc file, to find the domain extents by taking the
! minval and maxval of XLAT_V and XLONG_U. Check the cornerns of the domain
! to detect if the WRF domain spans lon = 180. 

  if (geoflag == 1) then

     rcode = nf_open(geofile,NF_NOWRITE,cdfid)
     if (rcode == 0) then
        if (debug) write(*,*) 'Open geo_em file ',trim(geofile)
     else
        write(*,*) 'Error openiing geo_em file ',trim(geofile)
        stop
     end if

     rcode = nf_inq_dimid(cdfid,'west_east',id)
     rcode = nf_inq_dimlen(cdfid,id,nx)

     rcode = nf_inq_dimid(cdfid,'south_north',id)
     rcode = nf_inq_dimlen(cdfid,id,ny)

     allocate( lat2d(nx,ny+1), lon2d(nx+1,ny) )

     call get_var_2d_real_cdf(cdfid,'XLAT_V', lat2d,nx,ny+1,1,debug)
     call get_var_2d_real_cdf(cdfid,'XLONG_U',lon2d,nx+1,ny,1,debug)
     rcode = nf_get_att_real(cdfid,nf_global,'corner_lons',corner_lons)
     rcode = nf_close(cdfid) ! done with the geo_em.d01.nc file

! We need to detect if the geo_em domain crosses the dateline (180E).
! See http://www.mmm.ucar.edu/wrf/users/docs/user_guide_V3/users_guide_chap3.htm
! and search down for "global attributes corner_lats and corner_lons"

     sw_lon = corner_lons(13) ! just easier to read 
     nw_lon = corner_lons(14)
     ne_lon = corner_lons(15)
     se_lon = corner_lons(16)

     if (debug) then
        write(*,*) "Corners of geo_em file:"
        write(*,*) "sw_lon  = ",sw_lon
        write(*,*) "nw_lon  = ",nw_lon
        write(*,*) "ne_lon  = ",ne_lon
        write(*,*) "se_lon  = ",se_lon
     end if

! Handle the case of WRF domain crossing lon = 180 by changing lon range from
! -180:180 to 0:360. If lon < 0, add 360. And we'll have to re-build the 
! data from the GHRSST file.

     if (any((/ &
          sw_lon > se_lon, &
          sw_lon > ne_lon, &
          nw_lon > se_lon, &
          nw_lon > ne_lon /))) then

        crosses_180E = .true. 
        if (debug) write(*,*) "geo_em domain crosses the line lon = +/-180"
     
        if (sw_lon  < 0.) sw_lon  = sw_lon  + 360.
        if (nw_lon  < 0.) nw_lon  = nw_lon  + 360.
        if (se_lon  < 0.) se_lon  = se_lon  + 360.
        if (ne_lon  < 0.) ne_lon  = ne_lon  + 360.
   
        where (lon2d < 0.) lon2d = lon2d + 360. 

     endif

     min_lat = minval(lat2d)
     max_lat = maxval(lat2d)
     min_lon = minval(lon2d)
     max_lon = maxval(lon2d)
     
     deallocate( lat2d, lon2d ) ! Done with these

     if (debug) then
        write(*,*) "Initial sub-domain:"
        write(*,*) "min_lat = ",min_lat
        write(*,*) "max_lat = ",max_lat
        write(*,*) "min_lon = ",min_lon
        write(*,*) "max_lon = ",max_lon
     endif

! Add a buffer of 0.5 degrees (lat or lon) to make sure we have enough data coverage.
! And round up/down to the nearest integer

     min_lon = real(  floor(min_lon - 0.5))
     max_lon = real(ceiling(max_lon + 0.5))
     min_lat = real(  floor(min_lat - 0.5))
     max_lat = real(ceiling(max_lat + 0.5))

     if (min_lat <  -90.) min_lat =  -90. ! range check
     if (max_lat >   90.) max_lat =   90.
     if (crosses_180E) then
        if (min_lon <    0.) min_lon =    0.
        if (max_lon >  360.) max_lon =  360.
     else
        if (min_lon < -180.) min_lon = -180.
        if (max_lon >  180.) max_lon =  180.
     endif

     if (debug) then
        write(*,*) "Extended/rounded sub-domain:"
        write(*,*) "min_lat = ",min_lat
        write(*,*) "max_lat = ",max_lat
        write(*,*) "min_lon = ",min_lon
        write(*,*) "max_lon = ",max_lon
        write(*,*) "sw_lon  = ",sw_lon
        write(*,*) "nw_lon  = ",nw_lon
        write(*,*) "ne_lon  = ",ne_lon
        write(*,*) "se_lon  = ",se_lon
     end if

  elseif (geoflag == 2) then ! user specified a lat-lon range

! Handle the case of WRF domain crossing lon = 180 by changing lon range from
! -180:180 to 0:360. If lon < 0, add 360. And we'll have to re-build the 
! data from the GHRSST file.
! *** FIXME: this is un-tested!

     if ( min_lon > max_lon) then

        crosses_180E = .true. 
     
        if (min_lon  < 0.) min_lon = min_lon  + 360.
        if (max_lon  < 0.) max_lon = max_lon  + 360.
   
        where (lon2d < 0.) lon2d = lon2d + 360. 

        if (debug) then
           write(*,*) "Requested lat-lon box crosses the line lon = +/-180"
           write(*,*) "Revised output sub-domain:"
           write(*,*) "min_lat = ",min_lat
           write(*,*) "max_lat = ",max_lat
           write(*,*) "min_lon = ",min_lon
           write(*,*) "max_lon = ",max_lon
        endif

     endif

  end if ! if (geoflag == 1) then

!*****************************************************************************
! Open the PODAAC GHRSST netCDF file
!*****************************************************************************

  rcode = nf_open(ncfile,NF_NOWRITE,cdfid)
  if (rcode == 0) then
     if (debug) write(*,*) 'Opened netcdf file ',trim(ncfile)
  else
     write(*,*) 'Error ',rcode,'opening netcdf file ',trim(ncfile)
     stop
  end if

!*****************************************************************************
! Get the Global Attributes we need
!*****************************************************************************

  rcode = nf_get_att_text(cdfid,NF_GLOBAL,"start_time",time_str) ! eg "20190101T090000Z"
  read(time_str,'(i4,i2,i2)') year,month,day
  hour = 12 ! middle of the day, even though GHRSST default is 09
  write(hdate,'(i4.4,"-",i2.2,"-",i2.2,"_",i2.2,":00:00")') year,month,day,hour

! Get dimensions we need, and allocate only required arrays

  rcode = nf_inq_dimid(cdfid,'lat',id)
  rcode = nf_inq_dimlen(cdfid,id,ny)

  rcode = nf_inq_dimid(cdfid,'lon',id)
  rcode = nf_inq_dimlen(cdfid,id,nx)

  if (debug) write(*,*) "Allocating for nx,ny = ",nx,ny
  allocate( lats(ny), lons(nx))
  if (crosses_180E) allocate( temp1d(nx), temp2d(nx,ny) )

! Get the 1D vector of values for lat

  rcode = nf_inq_varid(cdfid,'lat',id)
  rcode = nf_inq_var(cdfid,id,varnam,ivtype,ndims,dimids,natts)
  if (debug) write(*,*) 'ID, Number of dims for lat  ',id, ndims
  rcode = nf_get_vara_real(cdfid,id,1,ny,lats)

! Get the 1D vector of values for lon

  rcode = nf_inq_varid(cdfid,'lon',id)
  rcode = nf_inq_var(cdfid,id,varnam,ivtype,ndims,dimids,natts)
  if (debug) write(*,*) 'ID, Number of dims for lon  ',id, ndims
  rcode = nf_get_vara_real(cdfid,id,1,nx,lons)

  if (crosses_180E) then     ! swap the left & right halves of the array

     if (debug) write(*,*) "Output domain crosses lon=180E line, rearranging array."

     imid = nx/2+1
     if (debug) write(*,*) "1, nx-imid+1, imid, nx = ", 1, nx-imid+1, imid, nx

! Copy the right half, imid:nx,     to the left  half. 
! Copy the left  half, 1:nx-imid+1, to the right half.

     temp1d(1:(nx-imid+1)) = lons(imid:nx)
     temp1d(imid:nx)       = lons(1:(nx-imid+1))
     lons = temp1d
     where (lons <= 0.) lons = lons + 360.

  endif

!*****************************************************************************
! Get the 2D grid of values for LANDSEA MASK
! Make one mask for land vs sea (for SST), and one for land/ice vs sea (for SEAICE)
!*****************************************************************************

  if (write_landsea .or. append_landsea) then

     allocate( mask_in(nx,ny) )

     rcode = nf_inq_varid(cdfid,'mask',id)
     rcode = nf_inq_var(cdfid,id,varnam,ivtype,ndims,dimids,natts)
     if (debug) write(*,*) 'ID, Number of dims for mask ',id,ndims
     do i = 1,ndims
        rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
        if (debug) write(*,*) 'Dimension ',i,idims(i)
     enddo

     if (nx /= idims(1)) print*, "nx (lon) and idims(1) don't match."
     if (ny /= idims(2)) print*, "ny (lat) and idims(2) don't match."
     if ( 1 /= idims(3)) print*, "idims(3) is not 1."

     istart(1) = 1
     istop(1)  = nx
     istart(2) = 1
     istop(2)  = ny
     istart(3) = 1
     istop(3)  = 1
     rcode     = nf_get_vara_int1(cdfid,id,istart,istop,mask_in)

     if (write_sst .or. write_landsea) then
        allocate( landsea(nx,ny) )
        landsea    = bad
        landsea    = mask_in ! change to a regular interger

        if (crosses_180E) then
           temp2d(1:(nx-imid+1),:) = landsea(imid:nx,:)
           temp2d(imid:nx,:)       = landsea(1:(nx-imid+1),:)
           landsea = temp2d
        endif

! convert from GHRSST's values:
! flag_meanings = "1=open-sea, 2=land, 5=open-lake, 9=open-sea with ice in the grid, \
!   13=open-lake with ice in the grid"
! to WRF's 1 (land) or 0 (sea) values

        where (landsea ==  1) landsea = 0 ! open-sea
        where (landsea ==  2) landsea = 1 ! land
        where (landsea ==  5) landsea = 0 ! open-lake, maybe should be 2 to use avg_tsfc?
        where (landsea ==  9) landsea = 0 ! open-sea with ice in the grid
        where (landsea == 13) landsea = 0 ! open-lake with ice in the grid"

        if (debug) then
           write(*,*) "rcode = ",rcode
           write(*,*) "minval(mask_in) = ",minval(mask_in)
           write(*,*) "maxval(mask_in) = ",maxval(mask_in)
           write(*,*) "minval(landsea)  = ",minval(landsea)
           write(*,*) "maxval(landsea)  = ",maxval(landsea)
        end if

     end if

     if (write_ice) then
        allocate( landseaice(nx,ny) )
        landseaice = bad
        landseaice = landsea ! we'll apply different codes below

        if (crosses_180E) then
           temp2d(1:(nx-imid+1),:) = landsea(imid:nx,:)
           temp2d(imid:nx,:)       = landsea(1:(nx-imid+1),:)
           landsea = temp2d
        endif

        where (landseaice ==  1) landseaice = 0 ! open-sea
        where (landseaice ==  2) landseaice = 1 ! land
        where (landseaice ==  5) landseaice = 0 ! open-lake, maybe should be 2 to use avg_tsfc?
        where (landseaice ==  9) landseaice = 1 ! open-sea  with ice in the grid
        where (landseaice == 13) landseaice = 1 ! open-lake with ice in the grid"
     end if

  end if ! if (write_landsea .or. append_landsea) then

!************************************************************************************
! Set some defaults, to prepare for writing
!************************************************************************************
 
  xlvl = 200100.
  dx = 0     ! km, not used in iProj = 0 (latlon)
  dy = 0     ! km
  map_source = 'JPL PODAAC GHRSST'
  xfcst = 0.

! set metadata

  if (geoflag == 0) then ! output full grid

     startloc = 'CENTER' ! SWCORNER means (1,1), CENTER means (nx/2,ny/2)
     startlat = 0. ! lat ranges from  -90 to  90, or -80 to 80, center is 0.
     startlon = 0. ! lon ranges from -180 to 180, center is 0.
     ibeg     = 1  ! write the full dataset
     jbeg     = 1
     iend     = nx
     jend     = ny
     if (debug) then
        write(*,*) "Will output FULL grid:"
        write(*,*) "  i-range: ",ibeg," to ",iend
        write(*,*) "  j-range: ",jbeg," to ",jend
     endif

  else ! output a sub-set of the full grid

     startloc = 'SWCORNER' ! SWCORNER means (1,1), CENTER means (nx/2,ny/2)
     startlat = min_lat
     startlon = min_lon

! Not all GHRSST products are the same, so rather than calculate we'll search

     ibeg = 1
     do while (lons(ibeg) <  min_lon)
        ibeg = ibeg + 1
     end do
     iend = ibeg
     do while (lons(iend) <= max_lon .and. iend < nx)
        iend = iend + 1
     end do

     jbeg = 1
     do while (lats(jbeg) <  min_lat)
        jbeg = jbeg + 1
     end do
     jend = jbeg
     do while (lats(jend) <= max_lat .and. jend < ny)
        jend = jend + 1
     end do
     if (debug) then
        write(*,*) "Will output SUB-grid:"
        write(*,*) "  i-range: ",ibeg," to ",iend
        write(*,*) "  j-range: ",jbeg," to ",jend
     endif

  endif

! Figure out the grid dx,dy even though there's a global attribute (text)
! that says e.g. geospatial_lat_resolution = "0.01 degrees"

  i = nx/2 ; j = ny/2  
  deltalat = nint((lats(j+1) - lats(j))*10000.)/10000. ! round to 0.0001
  deltalon = nint((lons(i+1) - lons(i))*10000.)/10000.
  if (debug) then
     write(*,*) "lats(1:10)", lats(1:10)
     write(*,*) "lons(1:10)", lons(1:10)
  endif

  if (write_landsea) then ! different from append_landsea, instead make a file

!************************************************************************************
! Create LANDSEA:YYYY-MM-DD_HH, following metgrid/src/read_met_module.f90. 
!************************************************************************************

! set the filename, field, description, and units, and open the files

     filename = 'LANDSEA:'//hdate(1:13)
     field    = 'LANDSEA'
     desc     = 'Land Sea Mask'
     units    = ' '
     write(*,*) "Creating ",trim(filename)
     open(12,file=filename,form='unformatted',convert='big_endian')

! Write to the file, assuming iProj = 0 Cylindrical equidistant = latlon

     write(12) version
     write(12) hdate,xfcst,map_source,field,units,desc,xlvl, &
          iend-ibeg+1,jend-jbeg+1,iproj
     write(12) startloc,startlat,startlon,deltalat,deltalon,earth_radius

     if (debug) then
        write(*,*) "hdate             ",hdate              
        write(*,*) "xfcst             ",xfcst             
        write(*,*) "map_source        ",map_source        
        write(*,*) "field,units       ",field,units       
        write(*,*) "desc              ",desc              
        write(*,*) "xlvl              ",xlvl              
        write(*,*) "nx,ny             ",nx,ny
        write(*,*) "iProj             ",iProj              
        write(*,*) "startloc          ",startloc          
        write(*,*) "startlat,startlon ",startlat,startlon 
        write(*,*) "deltalat,deltalon ",deltalat,deltalon 
        write(*,*) "earth_radius      ",earth_radius      
     endif

!  3) WRITE WIND ROTATION FLAG

     write(12) wind_grid_rel

!  4) WRITE 2-D ARRAY OF DATA
     
     if (debug) write(*,1) "Writing LANDSEA, i-range = ",ibeg,iend," j-range = ",jbeg,jend
     
     write(12) landsea(ibeg:iend,jbeg:jend)
     
     close(12)
     
  end if ! if (write_landsea) then

  if (write_sst) then

!*****************************************************************************
! Get the variable attributes add_offset, scale_factor, and _FillValue for SST
!*****************************************************************************

     allocate( sst_in(nx,ny), sst(nx,ny) )

     rcode = nf_inq_varid(cdfid,'analysed_sst',id)
     if (debug) write(*,*) "rcode,id of analysed_sst =",rcode,id
     rcode = nf_get_att_int (cdfid,id,"_FillValue",  bad_value)
     rcode = nf_get_att_real(cdfid,id,"add_offset",  add_offset)
     rcode = nf_get_att_real(cdfid,id,"scale_factor",scale_factor)
     rcode = nf_get_att_real(cdfid,id,"valid_min",   valid_min)
     rcode = nf_get_att_real(cdfid,id,"valid_max",   valid_max)
     if (debug) then
        write(*,*) "For SST:"
        write(*,*) "_FillValue  ", bad_value
        write(*,*) "bad_value   ", bad, " (in sst)"
        write(*,*) "add_offset  ", add_offset
        write(*,*) "scale_factor", scale_factor
        write(*,*) "valid_min   ", valid_min
        write(*,*) "valid_max   ", valid_max
     end if

! Get the 2D grid of values for SST

     rcode = nf_inq_var(cdfid,id,varnam,ivtype,ndims,dimids,natts)
     if (debug) write(*,*) 'ID, Number of dims for SST  ',id,ndims
     do i = 1,ndims
        rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
        if (debug) write(*,*) 'Dimension ',i,idims(i)
     enddo

     if (nx /= idims(1)) print*, "nx (lon) and idims(1) don't match."
     if (ny /= idims(2)) print*, "ny (lat) and idims(2) don't match."
     if ( 1 /= idims(3)) print*, "idims(3) is not 1."

     istart(1) = 1
     istop(1)  = nx
     istart(2) = 1
     istop(2)  = ny
     istart(3) = 1
     istop(3)  = 1
     rcode = nf_get_vara_int2(cdfid,id,istart,istop,sst_in)

     if (debug) then
        write(*,*) "rcode = ",rcode
        write(*,*) "minval(sst_in) = ",minval(sst_in)
        write(*,*) "maxval(sst_in) = ",maxval(sst_in)
     end if

! scale the sst_in to find output sst values

     sst = bad
     where (sst_in /= bad_value) sst = add_offset + scale_factor * sst_in

     if (crosses_180E) then
        temp2d(1:(nx-imid+1),:) = sst(imid:nx,:)
        temp2d(imid:nx,:)       = sst(1:(nx-imid+1),:)
        sst = temp2d
     endif

     if (debug) then
        i = nx/5 ; j = ny/2
        write(*,*) "(i,j)       = ",i,j
        write(*,*) "sst_in(i,j)  = ",sst_in(i,j)
        write(*,*) "sst(i,j) = ",sst(i,j)
        write(*,*) "sst_in(nx,nx)  = ",sst_in(nx,ny)
        write(*,*) "sst(nx,nx) = ",sst(nx,ny)
        write(*,*) "minval(sst) = ",minval(sst)
        write(*,*) "maxval(sst) = ",maxval(sst)
     end if

!************************************************************************************
! Create SST:YYYY-MM-DD_HH, following metgrid/src/read_met_module.f90. 
!************************************************************************************

     filename = 'SST:'//hdate(1:13)
     field    = 'SST'
     desc     = 'Sea Surface Temperature'
     units    = 'K'
     write(*,*) "Creating ",trim(filename)
     open(12,file=filename,form='unformatted',convert='big_endian')

! Write to the file, assuming iProj = 0 Cylindrical equidistant = latlon

     write(12) version
     write(12) hdate,xfcst,map_source,field,units,desc,xlvl, &
          iend-ibeg+1,jend-jbeg+1,iproj
     write(12) startloc,startlat,startlon,deltalat,deltalon,earth_radius

     if (debug) then
        write(*,*) "hdate             ",hdate              
        write(*,*) "xfcst             ",xfcst             
        write(*,*) "map_source        ",map_source        
        write(*,*) "field,units       ",field,units       
        write(*,*) "desc              ",desc              
        write(*,*) "xlvl              ",xlvl              
        write(*,*) "nx,ny             ",nx,ny
        write(*,*) "iProj             ",iProj              
        write(*,*) "startloc          ",startloc          
        write(*,*) "startlat,startlon ",startlat,startlon 
        write(*,*) "deltalat,deltalon ",deltalat,deltalon 
        write(*,*) "earth_radius      ",earth_radius      
     endif

!  3) WRITE WIND ROTATION FLAG
! This line is in 
! https://www2.mmm.ucar.edu/wrf/users/docs/user_guide_v4/v4.0/users_guide_chap3.html#_Writing_Meteorological_Data
! but not in https://www2.mmm.ucar.edu/wrf/src/netcdf-to-intermediate.f
! I asked on the WRF Forum, and Ming Chen said it's OK to leave it out. But the first ref is version 5, the
! netcdf-to-intermediate.f writes version 3. 

     write(12) wind_grid_rel 

!  4) WRITE 2-D ARRAY OF DATA

     if (debug) write(*,1) "Writing SST, i-range = ",ibeg,iend," j-range = ",jbeg,jend
1    format(1x,a,2i7,a,2i7)

     write(12) sst(ibeg:iend,jbeg:jend)

     if (append_landsea) then

! See https://forum.mmm.ucar.edu/phpBB3/viewtopic.php?f=31&t=9032, where I was told to 
! write the LANDSEA mask in WPS Intermediate files.

        field    = 'LANDSEA'
        desc     = 'Land Sea Mask'
        units    = ' '
        write(12) version
        write(12) hdate,xfcst,map_source,field,units,desc,xlvl, &
             iend-ibeg+1,jend-jbeg+1,iproj
        write(12) startloc,startlat,startlon,deltalat,deltalon,earth_radius
        write(12) wind_grid_rel 
        write(12) landsea(ibeg:iend,jbeg:jend)
        
        close(12)

     endif  ! if (append_landsea) then

     deallocate( sst_in, sst ) ! done with these

  endif

  if (write_ice) then

!************************************************************************************
!-----Get the variable attributes add_offset, scale_factor, and _FillValue for SEAICE
!************************************************************************************

     allocate( ice_in(nx,ny), ice(nx,ny) )

     rcode = nf_inq_varid(cdfid,'sea_ice_fraction',id)
     if (debug) write(*,*) "rcode,id of ice_fraction =",rcode,id
     rcode = nf_get_att_int (cdfid,id,"_FillValue",  bad_value)
     rcode = nf_get_att_real(cdfid,id,"add_offset",  add_offset)
     rcode = nf_get_att_real(cdfid,id,"scale_factor",scale_factor)
     rcode = nf_get_att_real(cdfid,id,"valid_min",   valid_min)
     rcode = nf_get_att_real(cdfid,id,"valid_max",   valid_max)
     if (debug) then
        write(*,*) "For SEAICE:"
        write(*,*) "_FillValue  ", bad_value
        write(*,*) "add_offset  ", add_offset
        write(*,*) "scale_factor", scale_factor
        write(*,*) "valid_min   ", valid_min
        write(*,*) "valid_max   ", valid_max
     end if

! Get the 2D grid of values for SEAICE

     rcode = nf_inq_var(cdfid,id,varnam,ivtype,ndims,dimids,natts)
     if (debug) write(*,*) 'ID, Number of dims for SEAICE ',id,ndims
     do i = 1,ndims
        rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
        if (debug) write(*,*) 'Dimension ',i,idims(i)
     enddo

     if (nx /= idims(1)) print*, "nx (lon) and idims(1) don't match."
     if (ny /= idims(2)) print*, "ny (lat) and idims(2) don't match."
     if ( 1 /= idims(3)) print*, "idims(3) is not 1."

     istart(1) = 1
     istop(1)  = nx
     istart(2) = 1
     istop(2)  = ny
     istart(3) = 1
     istop(3)  = 1
     rcode = nf_get_vara_int1(cdfid,id,istart,istop,ice_in)

     if (debug) then
        write(*,*) "rcode = ",rcode
        write(*,*) "minval(ice_in) = ",minval(ice_in)
        write(*,*) "maxval(ice_in) = ",maxval(ice_in)
     end if

! scale the ice_in to find output ice values

     ice = bad
     where (ice_in /= bad_value) ice = add_offset + scale_factor * ice_in

     if (crosses_180E) then
        temp2d(1:(nx-imid+1),:) = ice(imid:nx,:)
        temp2d(imid:nx,:)       = ice(1:(nx-imid+1),:)
        ice = temp2d
     endif

     if (debug) then
        i = nx/5 ; j = ny/2
        write(*,*) "(i,j)          = ",i,j
        write(*,*) "ice_in(i,j)    = ",ice_in(i,j)
        write(*,*) "ice(i,j)   = ",ice(i,j)
        write(*,*) "ice_in(nx,nx)  = ",ice_in(nx,ny)
        write(*,*) "ice(nx,nx) = ",ice(nx,ny)
        write(*,*) "minval(ice) = ",minval(ice)
        write(*,*) "maxval(ice) = ",maxval(ice)
     end if

!************************************************************************************
! Create ICE:YYYY-MM-DD_HH, following metgrid/src/read_met_module.f90. 
!************************************************************************************
  
     filename = 'SEAICE:'//hdate(1:13)
     field    = 'SEAICE'
     desc     = 'Sea Ice Flag'
     units    = '0/1 Flag'
     write(*,*) "Creating ",trim(filename)
     open(12,file=filename,form='unformatted',convert='big_endian')

! Write to the file, assuming iProj = 0 Cylindrical equidistant = latlon

     write(12) version
     write(12) hdate,xfcst,map_source,field,units,desc,xlvl, &
          iend-ibeg+1,jend-jbeg+1,iproj
     write(12) startloc,startlat,startlon,deltalat,deltalon,earth_radius

     if (debug) then
        write(*,*) "hdate             ",hdate              
        write(*,*) "xfcst             ",xfcst             
        write(*,*) "map_source        ",map_source        
        write(*,*) "field,units       ",field,units       
        write(*,*) "desc              ",desc              
        write(*,*) "xlvl              ",xlvl              
        write(*,*) "nx,ny             ",nx,ny
        write(*,*) "iProj             ",iProj              
        write(*,*) "startloc          ",startloc          
        write(*,*) "startlat,startlon ",startlat,startlon 
        write(*,*) "deltalat,deltalon ",deltalat,deltalon 
        write(*,*) "earth_radius      ",earth_radius      
     endif

!  3) WRITE WIND ROTATION FLAG

     write(12) wind_grid_rel

!  4) WRITE 2-D ARRAY OF DATA

     if (debug) write(*,1) "Writing SEAICE, i-range = ",ibeg,iend," j-range = ",jbeg,jend

     write(12) ice(ibeg:iend,jbeg:jend)

     if (append_landsea) then

! See https://forum.mmm.ucar.edu/phpBB3/viewtopic.php?f=31&t=9032, where I was told to 
! write the LANDSEA mask in WPS Intermediate files.

        field    = 'LANDSEA'
        desc     = 'Land Sea Mask'
        units    = ' '
        write(12) version
        write(12) hdate,xfcst,map_source,field,units,desc,xlvl, &
             iend-ibeg+1,jend-jbeg+1,iproj
        write(12) startloc,startlat,startlon,deltalat,deltalon,earth_radius
        write(12) wind_grid_rel 
        write(12) landseaice(ibeg:iend,jbeg:jend) ! landseaice has ice = 1, like land

        deallocate( landseaice )

     end if  !      if (append_landsea) then

     close(12)

     deallocate( ice_in, ice )

  end if ! if (write_ice) then

! done with netcdf file, close it

  rcode = nf_close(cdfid)

  deallocate( lats, lons )
  if (allocated( mask_in )) deallocate( mask_in )
  if (allocated( landsea )) deallocate( landsea )

  write(*,*) " *** Successful completion of program ghrsst-to-intermediate ***"
  
end program ghrsst_to_intermediate
!
!------------------------------------------------------------------------------
!
subroutine get_var_2d_real_cdf(cdfid,var,data,i1,i2,time,debug)
!
!-----Read 2D real variable arrays
!
  implicit none
  include 'netcdf.inc'
  
  integer, intent(in)  ::  i1,i2,time
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2), intent(out) :: data
  
  integer cdfid,rcode,id_data
  character (len=80) :: varnam
  integer :: ndims,natts,idims(10),istart(10),iend(10),dimids(10)
  integer :: i,ivtype
!
!-----Entry point
!
  rcode = nf_inq_varid(cdfid,var,id_data)
  rcode = nf_inq_var(cdfid,id_data,varnam,ivtype,ndims,dimids,natts)
  if (debug) then
     write(*,*) 'Number of dims for ',var,' ',ndims
  endif
  do i = 1,ndims
     rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
     if (debug) write(*,*) 'Dimension ',i,idims(i)
  enddo
!
!-----Check the dimensions
!
  if ( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (time > idims(3)) )  then
     write(*,*) 'Error in 2d_var_real read, dimension problem'
     write(*,*) i1,idims(1)
     write(*,*) i2,idims(2)
     write(*,*) time,idims(4)
     write(*,*) 'Error stop'
     stop
  end if
!
!-----Get the data
!
  istart(1) = 1
  iend(1) = i1
  istart(2) = 1
  iend(2) = i2
  istart(3) = time
  iend(3) = 1
  
  rcode = nf_get_vara_real( cdfid,id_data,istart,iend,data)

end subroutine get_var_2d_real_cdf
!
!------------------------------------------------------------------------------
!
subroutine usage 

  write(*,*) "Usage:  ghrsst-to-intermediate [Options] file.nc"
  write(*,*) "Options:"
  write(*,*) "    -g geo_em.d01.nc        Output sub-grid that covers this domain."
  write(*,*) "    -b Slat,Nlat,Wlon,Elon  Specify sub-grid manually (no spaces)."
  write(*,*) "    -s | --sst              Output SST:YYYY-MM-DD_HH files."
  write(*,*) "    -i | --seaice           Output SEAICE:YYYY-MM-DD_HH files."
  write(*,*) "    -l | --landsea          Output LANDSEA:YYYY-MM-DD_HH files."
  write(*,*) "    -n | --nolandsea        Don't append LANDSEA mask to SST ICE files."
  write(*,*) "    -d | --debug            Print debug messages to the screen."
  write(*,*) "    -V | --version          Print version number and exit."
  write(*,*) "    -h | --help             Show this help message."
  write(*,*) "Required:"
  write(*,*) "    file.nc                GHRSST file downloaded from the JPL PODAAC."
  write(*,*) 
  write(*,*) "Converts GDS version 2 GHRSST datasets from JPL PODAAC (in netCDF format)"
  write(*,*) "to WPS intermediate format. See"
  write(*,*) "https://podaac.jpl.nasa.gov/datasetlist?ids=ProcessingLevel&values=*4*&search=GHRSST&view=list"
  write(*,*)
  write(*,*) "Example: "
  write(*,*) "ghrsst-to-intermediate -sst -l -g ../ERA5/geo_em.d01.nc ", & 
       "2014/20140101090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc"
  stop

end subroutine usage

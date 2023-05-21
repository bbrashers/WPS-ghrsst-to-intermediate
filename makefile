FC      = ftn
## FC      = pgf90
FFLAGS  = -g
# FFLAGS += -tp=istanbul
# FFLAGS += -mcmodel=medium
#FFLAGS += -Kieee                  # use exact IEEE math
# FFLAGS += -Mbounds                # for bounds checking/debugging
#FFLAGS += -Ktrap=fp               # trap floating point exceptions
# FFLAGS += -Bstatic_pgi            # to use static PGI libraries
#FFLAGS += -Bstatic                # to use static netCDF libraries
# FFLAGS += -mp=nonuma -nomp        # fix for "can't find libnuma.so"

# NETCDF should be compiled with the same compiler as this program
export HDF5=/opt/cray/pe/netcdf-hdf5parallel/4.7.4.2/crayclang/9.1
export netcdf_prefix=/opt/cray/pe/netcdf-hdf5parallel/4.7.4.2/crayclang/9.1
export NETCDF=${netcdf_prefix}
export PARALLEL_NETCDF_DIR=${netcdf_prefix}

INCL  = -I$(NETCDF)/include
INCL += -I$(HDF5)/include
LIBS  = -L$(NETCDF)/lib -lnetcdf -lnetcdff
LIBS += -L$(HDF5)/lib -lhdf5_hl -lhdf5 -lm
LIBS += -L/usr/lib64 -lz -ldl

PROGRAM = ghrsst-to-intermediate
SOURCES = ghrsst-to-intermediate.f90
OBJECTS = $(SOURCES:.f90=.o)

TODAY   = 2020-04-16
VERSION = 1.0

$(PROGRAM): $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) $(LIBS) -o $@

%.o : %.f90
	$(FC) $(FFLAGS) $(INCL) -c $< -o $@

install: $(PROGRAM)
	cp $(PROGRAM) /usr/local/bin

distro:
	zip $(PROGRAM)_v$(VERSION)_$(TODAY).zip \
		$(SOURCES) *.csh add_time makefile README.txt

clean:
	rm $(PROGRAM) $(OBJECTS)


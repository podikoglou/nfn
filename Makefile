FC=gfortran

all:
	$(FC) src/nfn.f90  -o nfn
	$(FC) src/nfn_datagen.f90  -o nfn_datagen

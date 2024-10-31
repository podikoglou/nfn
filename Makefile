FC=gfortran

all:
	$(FC) src/nfn_reg_lin.f90  -o nfn_reg_lin
	$(FC) src/nfn_datagen.f90  -o nfn_datagen

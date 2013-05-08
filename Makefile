
FC=gfortran

SRCMOD = mconst.f90
SRCF90 = ris_ana.f90
SRCF   = estrms.f skfun.f

OBJMOD = $(foreach a, $(SRCMOD), $(patsubst %.f90,%.o,$(a)))
OBJF90 = $(foreach a, $(SRCF90), $(patsubst %.f90,%.o,$(a)))
OBJF   = $(foreach a, $(SRCF)  , $(patsubst %.f  ,%.o,$(a)))

OBJECT = $(OBJMOD) $(OBJF90) $(OBJF)

FLAGS_LINK=
FLAGS_COM = -c
FLAGS_F90 =

SN=ris_ana

#making
install: $(SN)

$(SN): $(OBJECT)
	$(FC) -o $(SN) $(OBJECT) $(FLAGS_LINK)

clean:
	-rm -f *.o $(SN)

%.o: %.f90
	$(FC) $(FLAGS_COM) $(FF90) $< -o $@

%.o: %.f
	$(FC) $(FLAGS_COM) $< -o $@


#
# Automagically generated by Approximatrix Simply Fortran 2.41
#
FC="D:\SimplyFortran2\mingw-w64\bin\gfortran.exe"
CC="D:\SimplyFortran2\mingw-w64\bin\gcc.exe"
AR="D:\SimplyFortran2\mingw-w64\bin\ar.exe"
WRC="D:\SimplyFortran2\mingw-w64\bin\windres.exe"
RM=rm -f

IDIR=

LDIR=


OPTFLAGS= -g -Og -mtune=bdver4

SPECIALFLAGS=$(IDIR)

RCFLAGS=-O coff

PRJ_FFLAGS= -fcray-pointer -fopenmp -std=f2008

PRJ_CFLAGS=

# Auto-including AppGraphics libraries.
PRJ_LFLAGS=-lappgraphics -lgdi32 -lcomdlg32 -lcomctl32 -luuid -loleaut32 -lole32

FFLAGS=$(SPECIALFLAGS) $(OPTFLAGS) $(PRJ_FFLAGS) -Jmodules 

CFLAGS=$(SPECIALFLAGS) $(OPTFLAGS) $(PRJ_CFLAGS)

"build\control.o": ".\control.f90" "modules\m_flowa.mod" "modules\m_globals.mod" "modules\m_progressbar.mod"
	@echo Compiling .\control.f90
	@$(FC) -c -o "build\control.o" $(FFLAGS) ".\control.f90"
"modules\m_control.mod" : "build\control.o" .EXISTSONLY
	@echo Compiling .\control.f90
	@$(FC) -c -o "build\control.o" $(FFLAGS) ".\control.f90"

"build\flowa.o": ".\flowa.f90" "modules\m_globals.mod" "modules\m_sort.mod"
	@echo Compiling .\flowa.f90
	@$(FC) -c -o "build\flowa.o" $(FFLAGS) ".\flowa.f90"
"modules\m_flowa.mod" : "build\flowa.o" .EXISTSONLY
	@echo Compiling .\flowa.f90
	@$(FC) -c -o "build\flowa.o" $(FFLAGS) ".\flowa.f90"

"build\globals.o": ".\globals.f90"
	@echo Compiling .\globals.f90
	@$(FC) -c -o "build\globals.o" $(FFLAGS) ".\globals.f90"
"modules\m_globals.mod" : "build\globals.o" .EXISTSONLY
	@echo Compiling .\globals.f90
	@$(FC) -c -o "build\globals.o" $(FFLAGS) ".\globals.f90"

"build\main.o": ".\main.f90" "modules\m_control.mod" "modules\m_flowa.mod" "modules\m_globals.mod" "modules\m_progressbar.mod"
	@echo Compiling .\main.f90
	@$(FC) -c -o "build\main.o" $(FFLAGS) ".\main.f90"

"build\progressbar.o": ".\progressbar.f90"
	@echo Compiling .\progressbar.f90
	@$(FC) -c -o "build\progressbar.o" $(FFLAGS) ".\progressbar.f90"
"modules\m_progressbar.mod" : "build\progressbar.o" .EXISTSONLY
	@echo Compiling .\progressbar.f90
	@$(FC) -c -o "build\progressbar.o" $(FFLAGS) ".\progressbar.f90"

"build\sort.o": ".\sort.f90" "modules\m_globals.mod"
	@echo Compiling .\sort.f90
	@$(FC) -c -o "build\sort.o" $(FFLAGS) ".\sort.f90"
"modules\m_sort.mod" : "build\sort.o" .EXISTSONLY
	@echo Compiling .\sort.f90
	@$(FC) -c -o "build\sort.o" $(FFLAGS) ".\sort.f90"


"build\sf_default_resource.res": "build\sf_default_resource.rc"
	@echo Processing default resource
	@$(WRC) build\sf_default_resource.rc $(RCFLAGS) -o build\sf_default_resource.res

clean: .SYMBOLIC
	@echo Deleting build\control.o and related files
	@$(RM) "build\control.o" "modules\m_control.mod" "modules\m_control.smod"
	@echo Deleting build\flowa.o and related files
	@$(RM) "build\flowa.o" "modules\m_flowa.mod" "modules\m_flowa.smod"
	@echo Deleting build\globals.o and related files
	@$(RM) "build\globals.o" "modules\m_globals.mod" "modules\m_globals.smod"
	@echo Deleting build\main.o and related files
	@$(RM) "build\main.o"
	@echo Deleting build\progressbar.o and related files
	@$(RM) "build\progressbar.o" "modules\m_progressbar.mod" "modules\m_progressbar.smod"
	@echo Deleting build\sort.o and related files
	@$(RM) "build\sort.o" "modules\m_sort.mod" "modules\m_sort.smod"
	@echo Deleting default icon resource
	@$(RM) "build\sf_default_resource.res"
	@echo Deleting flowabrot.exe
	@$(RM) "flowabrot.exe"

"flowabrot.exe":  "build\control.o" "build\flowa.o" "build\globals.o" "build\main.o" "build\progressbar.o" "build\sort.o" "build\sf_default_resource.res" "build\flowabrot.prj.target"
	@echo Generating flowabrot.exe
	@$(FC) -o "flowabrot.exe" -static -fopenmp -mwindows "build\control.o" "build\flowa.o" "build\globals.o" "build\main.o" "build\progressbar.o" "build\sort.o" "build\sf_default_resource.res" $(LDIR) $(PRJ_LFLAGS)

all: "flowabrot.exe" .SYMBOLIC


cefine
======

This is the official repository of the `cefine` program developed by the 
Grimme group in Bonn.

`cefine` is a commandline wrapper around the `TURBOMOLE` `define` program. 
The program is distributed as is! 


Installing
----------

To compile `cefine` from source install Intel Parallel Studio 17 or later.

```bash
>ifort -o cefine cefine.f90
```

Usage
-----

Providing a clean folder with a tmole *coord* file, setting up a 
PW6B95-D3/def2-QZVP calculation can be done by calling:

```bash
> cefine -func pw6b95 -bas def2-QZVP -d3 -ri -grid m5 -scfconv 7

 Command line define V2.2, SG,HK 2006-18  August 2018 (-h for help) 
 /home/$USER/.cefinerc                                                          
  
cml-input: cefine -func pw6b95 -bas def2-QZVP -d3 -ri -grid m5 -scfconv 7           
 * RI-DFT(m5)-pw6b95/def2-QZVP * 
 define ended normally 
#atoms= 24
Number of MOs=     51, Electrons=    102.00, Symmetry: c1   
 HOMO/LUMO-SEPARATION :  0.153126
```

The total charge can be provided by the argument -chrg or by writing the charge
into the file `.CHRG`. The number of unpaired electrons can be provided by the 
argument -uhf or by writing the number to the file `.UHF`. Both files, if 
available, are read automatically when calling `cefine`.

Infomation on all flags is provided by:

```bash
> cefine -h
cml-input: cefine -h                    
 options:
    -hf (def: RI-DFT/TPSS)
    -func <string>
    -bas  <string>
    -grid <string>
    -mp2  (do RI-MP2)
    -scs  (do RI-SCS-MP2)
    -sos  (do RI-SOS-MP2)
    -lsos /-lap  (do RI-Laplace-SOS-MP2)
    -cc  (do RI-CCSD(T))
    -d3   ($disp3 -bj)
    -d3atm ($disp3 -bj -abc)
    -zero (D3 zero damping)
    -d4   ($disp4)
    -donl   ($donl, induces c1 sym)
    -ref  (reference SP)
    -vdw (DFT-D2)
    -quick  (PWLDA/SVP grid 1, no ired)
    -chrg <integer>
    -angst (read coords in Angstroem)
    -uhf <integer> (integer=# Na-Nb)
    -sym <string> (def: desy 0.03)
    -scfconv <integer>
    -abel (adjust for Abelian subgroups->e.g. pmp2)
    -noopt (def: ired optimization)
    -opt (switch on opt e.g. for MP2)
    -novdw (switch it off for B97-D due to EDA)
    -nori
    -ri
    -nofc (all e-corr. for MP2)
    -rijk (RI for HF/hybrids)
    -rijcosx (seminum. exchange w/ COS alg. for hybrids)
    -or (set flags for OR, escf)
    -ex (set flags UV/CD, escf)
    -fold (take forceapprox from previous run)
    -mold (take mos from previous run)
    -trold (takes hessian from previous run, activates TS)
    -trunc (truncated RIJK)
    -fon (Fermi smearing options switched on)
    -pol (set flags C6 computation, escf)
    -cp1 (counterpoise computation, frag1, calls splitmol)
    -cp2 (counterpoise computation, frag2, calls splitmol)
    -ts  (statpt TS search settings)
    -r12 (R12/F12 options for ricc2)
    -dftmrci (sets cbas, bhlyp etc)
    -cosmo <real> (COSMO with dk=real)
    -echo (write important parts of control)
    -keep (debuging)
    -co <coord_file>  (def: coord)
    -cox <xyz_file>  (writes "coord" file)
    -lib <integer> (use own basis set lib) 
     ($HOME/.definerc, basis=PATH)
    -auxlib <PATH>  (own jbas/cbas basis lib)
    -diff (add spd/sp diffuse functions)
    -test (do not call define)
    -nodiff (turns off diff density feature of TM)
    -gf  (remove g/f on H-Rn in def2-QZVP only)
 needs: <coord> file in TM format
 optional files    : <.SYM> with Schoenflies symbol
 (in <coord> dir)    <.UHF> integer number Na-Nb
                     <.CHRG> integer (charge)
 possible options in .cefinerc:
 func    STRING
 bas     STRING
 grid    STRING
 desythr REAL
 scfconv INTEGER
 ricore  INTEGER
 twoint  INTEGER
 maxcor  INTEGER
 fp REAL
 vdw     on   #  sets DFT-D2(BJ) 
 echo    on   #  more printing
 marij        #sets $marij 
 no-rij       #no RIJ for hybrids, if functional is known
 nodiff       #turns off diff density feature of TM
```


License
-------

`cefine` is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

`cefine` is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose. See the
GNU Lesser General Public License for more details.

Bugs
----
The code is distributed as is! You can report bugs by opening an issue, 
providing example of in- and output, and we will choose if we can 
follow up on it.

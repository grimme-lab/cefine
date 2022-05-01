! This file is part of cefine.
!
! Copyright (C) 2006-2019 Stefan Grimme
!
! cefine is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! cefine is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with cefine. If not, see <https://www.gnu.org/licenses/>.

module dyn_array 
   integer, allocatable :: idx(:)
end module

Program comand_line_define

use dyn_array
implicit none

integer maxarg,io,i,nn,err
parameter (maxarg=20)
character*80 list
character*80 arg(maxarg)    
character*80 out,run1,run2,run3
character*80 func           
character*80 grid          
character*80 bas            
character*80 sym            
character*80 atmp           
character*20 hsstmp
character*80 coord_name     
character*80 newlib
character*80 dbas   
character*80 homedir,cefinerc
character*2 baselem(85)
character*2 baselem2(60)
real*8 extol !tolerance of K integrals
logical RI, MP2, DFT, VDW, DESY, OPT, KEEP, UHF, MODEHT,QUICK,TROLD
logical DFPT2, FC, pr, OR, RIK, NOVDW, EX, CP1, CP2, POL, CC, ECP
logical COORD, FOLD, MOLD, RANGST, SCS, TRUNC,LIB,ALIB,LAP,NDIFF
logical da,FON,TS,R12,MRCI,COSMO,OPTI,ECHO,TEST,OLDMO,SOS,ZERO,FAKE
logical strange_elem,diffuse, egrid, XMOL, MARIJ,REF,nori,BJ,ATM,D4
logical deletion_failed, RMGF, RMF
logical cosx ! SAW: added seminumerical exchange = COSX
logical modbas  !basis defined in input
logical modgrid  !grid defined in input
logical modrad  !radsize defined in input
logical noauxg  !remove g-fkt from auxbasis
logical modaux !aux handling defined in input
logical hf3c ! perform HF-3c calculation
logical donl ! perform nl non-self-consistent
logical gcpinfo !for pbe-3c, pbe0-3c, b3-lyp-3c (echo into control)
integer ricore, scfconv, intmem, thime, maxcor, rpacor,radsize
integer charge, maxiter, nheavy, nopen, nat
integer kfrag, libnr, l, ntypes, irare, att(20)
real*8  desythr,xx(5),thize,cosmodk,dum,fp,mscale

!     coord file handling
real*8 xyz(3,10000)
integer iat(10000)

pr=.true. 
!    .'--------------------------------------------'
      if(pr) write(*,*)'Command line define V2.23, SG,HK 2006-18  August 2018 (-h for help) '
!     write(*,*)
!    .'--------------------------------------------'
      io=1

      out='def.inp'
!      run3='define_huge<def.inp>define.out'
      run3='define<def.inp>define.out'

! defaults
      maxiter=125 
      desythr=0.05
      scfconv=7
      grid   ='m4'
      ricore =1000
      intmem =1000
      maxcor =1000
      rpacor =1000
      thime  =4  
      thize  =0.0000001
!     func   ='tpss'
      func   ='pbeh-3c'
      bas    ='def2-TZVP'
      charge =0
      cosmodk=78.0
      fp     =-3.5
      extol  = -1.0d0      !negatice extol -> not used

      FAKE=.false.
      VDW  =.false.
      ECHO =.false.
      REF=.false.
      XMOL=.false.
      RI   =.true.
      RIK  =.false.
      COSX = .false. !SAW
      R12  =.false.
      MODEHT=.false.
      RANGST=.false.
      DFT  =.true. 
      DESY =.true. 
      CP1  =.false.
      CP2  =.false.
      OPT  =.true.
      MP2  =.false.
      OPTI =.false.
      KEEP =.false.
      UHF  =.false.
      DFPT2=.false.
      OR   =.false.
      NOVDW=.false.
      EX   =.false.
      POL  =.false.
      COORD=.false.
      FOLD =.false.
      MOLD =.false.
      SCS  =.false. 
      SOS  =.false.
      TRUNC=.false.
      FC   =.true. 
      LIB  =.false.
      ALIB =.false.
      TS   =.false. 
      FON  =.false. 
      MRCI =.false. 
      COSMO=.false. 
      diffuse=.false. 
      TEST =.false.
      TROLD =.false.
      OLDMO=.false.
      LAP  =.false.
      EGRID=.false.
      MARIJ=.false.
      QUICK=.false.
!       BJ=.false.
      RMGF=.false.
      RMF=.false.
!JGB BJ as default (!)
      BJ=.true.
      ZERO=.false.
!FB ATM
      ATM=.false.
!FB D4
      D4=.false.
      NDIFF=.false.
      ECP=.false.
      CC=.false.
      modbas=.false.
      modaux=.false.
      noauxg=.false.
      modgrid=.false.
      modrad=.false.
      hf3c=.false.
!FB NL
      donl=.false.
!FB gcpinfo
      gcpinfo=.false.

      !> check for existence of .cefinerc
      CALL get_environment_variable("HOME", homedir) !> get home directory to variable
      cefinerc=trim(homedir)//'/.cefinerc' !> to remove trailing character
      inquire(file=cefinerc,exist=da) !> check for existence
      if(da)then
         write(*,*) cefinerc
         open(unit=20,file=cefinerc)
 842     read(20,'(a)',end=942)atmp
         if(index(atmp,'desythr').ne.0)then         
            call readl(atmp,xx,nn)
            desythr=xx(nn)
         endif
         if(index(atmp,'scfconv').ne.0)then         
            call readl(atmp,xx,nn)
            scfconv=xx(nn)
         endif
         if(index(atmp,'fp').ne.0)then         
            call readl(atmp,xx,nn)
            fp     =xx(nn)
         endif
         if(index(atmp,'ricore').ne.0)then         
            call readl(atmp,xx,nn)
            ricore=idint(xx(nn))
         endif
         if(index(atmp,'maxcor').ne.0)then         
            call readl(atmp,xx,nn)
            maxcor=idint(xx(nn))
         endif
         if(index(atmp,'rpacor').ne.0)then
            call readl(atmp,xx,nn)
            rpacor=idint(xx(nn))
         endif
         if(index(atmp,'twoint').ne.0)then         
            call readl(atmp,xx,nn)
            intmem=idint(xx(nn))
         endif
         if(index(atmp,'func').ne.0)then         
            call backstring(atmp,func,4)
         endif
         if(index(atmp,'bas').ne.0)then         
            call backstring(atmp,bas,3)
            modbas=.true.
         endif
         if(index(atmp,'grid').ne.0)then         
            call backstring(atmp,grid,4)
            egrid=.true.
            modgrid=.true.
         endif
         if(index(atmp,'radsize').ne.0)then
            call readl(atmp,xx,nn)
            radsize=xx(nn)
            modrad=.true.
         endif
         if(index(atmp,'vdw').ne.0) then
            if(index(atmp,'on').ne.0)BJ=.true.   
         endif
         if(index(atmp,'marij').ne.0) then
            MARIJ=.true.   
            RI=.true.
         endif
         if(index(atmp,'no-rij').ne.0) then
             NORI=.true.   
          endif
         if(index(atmp,'echo').ne.0)then 
            if(index(atmp,'on').ne.0)ECHO=.true.   
         endif
         if(index(atmp,'nodiff').ne.0)NDIFF=.true. 
         goto 842
942     close(20)
      endif
            


      arg=''
      do i=1,maxarg
         call getarg(i,arg(i))
      enddo
      !FB print cml-input
      write(6,'(a,1x)',advance="no") 'cml-input: cefine'
      do i=1,maxarg
         write(6,'(a,1x)',advance="no")trim(arg(i))
      enddo
      write(*,*)

      if(arg(1).eq.'-h' .or. arg(1).eq.'?' .or. arg(1).eq.'-help')then
         write(*,*)'options:'
         write(*,*)'   -hf (def: RI-DFT/TPSS)'
         write(*,*)'   -func <string>'
         write(*,*)'   -bas  <string>'
         write(*,*)'   -grid <string>'
         write(*,*)'   -radsize <integer>'
         write(*,*)'   -mp2  (do RI-MP2)'
         write(*,*)'   -scs  (do RI-SCS-MP2)'
         write(*,*)'   -sos  (do RI-SOS-MP2)'
         write(*,*)'   -lsos /-lap  (do RI-Laplace-SOS-MP2)'
         write(*,*)'   -msc_exc  <string> (z.B. 1-9, list of atoms that should NOT be scaled) <real> (scaling factor, default 1000000)'
         write(*,*)'   -cc  (do RI-CCSD(T))'
         write(*,*)'   -d3   ($disp3 -bj)'
         write(*,*)'   -d3atm ($disp3 -bj -abc)' ! FB implemented
         write(*,*)'   -zero (D3 zero damping)'
         write(*,*)'   -d4   ($disp4)'
         write(*,*)'   -donl   ($donl, induces c1 sym)'
         write(*,*)'   -ref  (reference SP)'
!         write(*,*)'   -zero ($disp3   )'
!         write(*,*)'   -noscs (no SCS-MP2 when using ricc2)'
         write(*,*)'   -vdw (DFT-D2)'
         write(*,*)'   -quick  (PWLDA/SVP grid 1, no ired)'
         write(*,*)'   -chrg <integer>'
         write(*,*)'   -angst (read coords in Angstroem)'
         write(*,*)'   -uhf <integer> (integer=# Na-Nb)'
         write(*,*)'   -sym <string> (def: desy 0.03)'
         write(*,*)'   -scfconv <integer>'
         write(*,*)'   -abel (adjust for Abelian subgroups->e.g. pmp2)'
         write(*,*)'   -noopt (def: ired optimization)'
         write(*,*)'   -opt (switch on opt e.g. for MP2)'
         write(*,*)'   -novdw (switch it off for B97-D due to EDA)'
         write(*,*)'   -nori'                          
         write(*,*)'   -ri'
         write(*,*)'   -nofc (all e-corr. for MP2)'
         write(*,*)'   -rijk (RI for HF/hybrids)'  
         write(*,*)'   -rijcosx (seminum. exchange w/ COS alg. for hybrids)'   !SAW
         write(*,*)'   -or (set flags for OR, escf)'
         write(*,*)'   -ex (set flags UV/CD, escf)'
         write(*,*)'   -fold (take forceapprox from previous run)'
         write(*,*)'   -mold (take mos from previous run)'
         write(*,*)'   -trold (takes hessian from previous run, activates TS)'
         write(*,*)'   -trunc (truncated RIJK)'
         write(*,*)'   -fon (Fermi smearing options switched on)'
         write(*,*)'   -pol (set flags C6 computation, escf)'
         write(*,*)'   -cp1 (counterpoise computation, frag1, calls splitmol)'
         write(*,*)'   -cp2 (counterpoise computation, frag2, calls splitmol)'
         write(*,*)'   -ts  (statpt TS search settings)'
         write(*,*)'   -r12 (R12/F12 options for ricc2)'
         write(*,*)'   -dftmrci (sets cbas, bhlyp etc)'
         write(*,*)'   -cosmo <real> (COSMO with dk=real)' 
         write(*,*)'   -echo (write important parts of control)'
         write(*,*)'   -keep (debuging)'
         write(*,*)'   -co <coord_file>  (def: coord)'
         write(*,*)'   -cox <xyz_file>  (writes "coord" file)'
         write(*,*)'   -lib <integer> (use own basis set lib) '
         write(*,*)'    ($HOME/.definerc, basis=PATH)'
         write(*,*)'   -auxlib <PATH>  (own jbas/cbas basis lib)'
         write(*,*)'   -diff (add spd/sp diffuse functions)'
         write(*,*)'   -test (do not call define)'
         write(*,*)'   -nodiff (turns off diff density feature of TM)'
         write(*,*)'   -gf  (remove g/f on H-Rn in def2-QZVP only)'
         write(*,*)'   -fpol  (remove f on B-Rn in def2-TZVPD only)'
         write(*,*)'needs: <coord> file in TM format'
         write(*,*)'optional files    : <.SYM> with Schoenflies symbol'
         write(*,*)'(in <coord> dir)    <.UHF> integer number Na-Nb'
         write(*,*)'                    <.CHRG> integer (charge)'
         write(*,*)'possible options in .cefinerc:'
         write(*,*)'func    STRING'
         write(*,*)'bas     STRING'
         write(*,*)'grid    STRING'
         write(*,*)'desythr REAL'  
         write(*,*)'scfconv INTEGER'
         write(*,*)'ricore  INTEGER'
         write(*,*)'twoint  INTEGER'
         write(*,*)'maxcor  INTEGER'
         write(*,*)'rpacor  INTEGER'
         write(*,*)'fp REAL'
         write(*,*)'vdw     on   #  sets DFT-D2(BJ) '       
         write(*,*)'echo    on   #  more printing'       
         write(*,*)'marij        #sets $marij '
         write(*,*)'no-rij       #no RIJ for hybrids, if functional is known'
         write(*,*)'nodiff       #turns off diff density feature of TM'
         stop
      endif

      do i=1,maxarg
         if(arg(i).ne.'')then
            if(index(arg(i),'-ref').ne.0) REF=.true.
            if(index(arg(i),'-fake').ne.0) FAKE=.true.
            if(index(arg(i),'-ecp').ne.0) ECP=.true.
            !FB -d3atm
            if(index(arg(i),'-d3atm').ne.0) then
             BJ=.true.
             ATM=.true.
            endif
            if(index(arg(i),'-d3').ne.0) BJ=.true.
            if(index(arg(i),'-zero').ne.0) then
             ZERO=.true.
             BJ=.false.
            endif
            !FB d4
            if(index(arg(i),'-d4').ne.0)then
             D4=.true.
             BJ=.false.
             ATM=.false.
             ZERO=.false.
            endif
            !FB NL
            if(index(arg(i),'-donl').ne.0)then
              donl=.true.
            endif
            if(index(arg(i),'-quick').ne.0) QUICK=.true.
            if(index(arg(i),'-nodiff').ne.0) NDIFF=.true.
            if(index(arg(i),'-nolap').ne.0) LAP=.false.
            if(index(arg(i),'-noscs').ne.0) SCS=.false.
            if(index(arg(i),'-lap').ne.0.or.index(arg(i),'-lsos').ne.0) then 
             LAP=.true.
             DFT=.false.
             NOVDW=.true.
             func='tpss'
            endif    
            if(index(arg(i),'-scs').ne.0) then 
             SCS=.true.
             DFT=.false.
             NOVDW=.true.
             func='tpss'
            endif
            if(index(arg(i),'-sos').ne.0) then 
             SOS=.true.
             DFT=.false.
             NOVDW=.true.
             func='tpss'
            endif
            if(index(arg(i),'-gf').ne.0)    RMGF=.true.
            if(index(arg(i),'-fpol').ne.0)    RMF=.true.
            if(index(arg(i),'-marij ').ne.0) MARIJ=.true.
            if(index(arg(i),'-fold').ne.0)  FOLD=.true. 
            if(index(arg(i),'-mold').ne.0)  MOLD=.true. 
            if(index(arg(i),'-modeht').ne.0)MODEHT=.true. 
            if(index(arg(i),'-nofc').ne.0)  FC=.false.
            if(index(arg(i),'-or ').ne.0)    OR=.true. 
            if(index(arg(i),'-pol ').ne.0)   POL=.true. 
            if(index(arg(i),'-ex').ne.0)    EX=.true. 
            if(index(arg(i),'-nori').ne.0)  RI=.false.
            if(index(arg(i),'-ri').ne.0) then
            NORI=.false.
            RI=.true.
            endif
            if(index(arg(i),'-noopt').ne.0) OPT=.false.
            if(index(arg(i),'-opt').ne.0)   OPTI=.true. 
            if(index(arg(i),'-hf').ne.0)    DFT=.false.
            if(index(arg(i),'-mp2').ne.0)  then 
             MP2=.true. 
             DFT=.false.
             NOVDW=.true.
             func='tpss'
            endif
            if(index(arg(i),'-vdw').ne.0)   VDW=.true. 
            if(index(arg(i),'-novdw').ne.0) NOVDW=.true. 
            if(index(arg(i),'-cp1').ne.0)   CP1=.true. 
            if(index(arg(i),'-cp2').ne.0)   CP2=.true. 
            if(index(arg(i),'-rijk').ne.0)  RIK=.true. 
            if(index(arg(i),'-rijcosx').ne.0)  COSX=.true. 
            if(index(arg(i),'-trunc').ne.0) TRUNC=.true. 
            if(index(arg(i),'-fon').ne.0)   FON=.true. 
            if(index(arg(i),'-ts').ne.0)    TS=.true. 
            if(index(arg(i),'-r12').ne.0)   R12=.true.
            if(index(arg(i),'-dftmrci').ne.0) then 
             MRCI=.true.
             NOVDW=.true.
             func='bh-lyp'
            endif
            if(index(arg(i),'-echo').ne.0)  ECHO=.true.
            if(index(arg(i),'-diff').ne.0)  diffuse=.true.
            if(index(arg(i),'-test').ne.0)  TEST=.true.
            if(index(arg(i),'-oldmo').ne.0) OLDMO=.true.
            if(index(arg(i),'-pmos').ne.0) OLDMO=.true.
            if(index(arg(i),'-trold').ne.0) TROLD=.true. ! cbann: enable use of old hessian for TS search
            if(index(arg(i),'-cc ').ne.0) then 
             CC=.true.
             DFT=.false.
             NOVDW=.true.
             func='tpss'
            endif
!JGB include hf-3c compound key
            if(index(arg(i),'-hf3c').ne.0) then
              hf3c=.true.
              dft=.false.
              RI=.false.
            endif  
!JGB remove g functions from auxbasis
            if(index(arg(i),'-noauxg ').ne.0) then
                noauxg=.true.
                modaux=.true.
            endif
!JGB do not remove g functions in auxbasis
            if(index(arg(i),'-auxg ').ne.0) then
                noauxg=.false.
                modaux=.true.
            endif
            if(index(arg(i),'-co ').ne.0)then
               COORD=.true. 
               coord_name=arg(i+1)           
            endif
            if(index(arg(i),'-cox ').ne.0)then
!             call readl(arg(i+1),xx,nn)
             call xyzrd(xyz,iat,nat,arg(i+1))
             call wtm(xyz,iat,nat,'coord')
!                coord_name='coord'!arg(i+1)           
            endif
            if(index(arg(i),'-chrg').ne.0)then
               call readl(arg(i+1),xx,nn)
               charge=idint(xx(1))
            endif
            if(index(arg(i),'-cosmo').ne.0)then
               call readl(arg(i+1),xx,nn)
               dum=xx(1)
               if(dum.gt.0.999)cosmodk=dum           
               COSMO=.true.
            endif
            if(index(arg(i),'-scfconv').ne.0)then
               call readl(arg(i+1),xx,nn)
               scfconv=idint(xx(1))
            endif
!FB radsize
            if(index(arg(i),'-radsize').ne.0)then
               call readl(arg(i+1),xx,nn)
               radsize=idint(xx(1))
               modrad=.true.
            endif
!JGB K tolerance
            if(index(arg(i),'-ktol').ne.0)then
               call readl(arg(i+1),xx,nn)
               extol=xx(1)
            endif
            if(index(arg(i),'-uhf').ne.0)then
               call readl(arg(i+1),xx,nn)
               nopen=idint(xx(1))
! ???        if(nopen.gt.0)UHF=.true.
            write(*,*) ' *Switching UHF mode on*'
               UHF=.true.
            endif
            if(index(arg(i),'-sym').ne.0)then
               sym=arg(i+1)
               DESY=.false.
            endif
            if(index(arg(i),'-abel').ne.0)then
               call susy(sym,DESY)
            endif
            if(index(arg(i),'-grid').ne.0) then
              grid =arg(i+1)
              modgrid=.true.
            endif
            if(index(arg(i),'-bas').ne.0) then
               bas  =arg(i+1)
               modbas=.true.
            endif
            if(index(arg(i),'-func').ne.0) then
! c    .         index(arg(i),'-f').ne.0)then
               func=arg(i+1)           
               DFT=.true.
               write(*,*) func
               STOP
            endif
            if(index(arg(i),'-angst').ne.0)RANGST=.true. 

            if(index(arg(i),'-lib').ne.0) then  ! ... hok
            LIB=.true.
!  no need for more than one own basis set library?
!          call readl(arg(i+1),xx,nn)
!            libnr=idint(xx(1))
             libnr=3
            endif
            if(index(arg(i),'-auxlib').ne.0) then
            ALIB=.true.
            newlib=arg(i+1)
!            call readl(arg(i+2),xx,nn)
!            ntypes=idint(xx(1))
            endif                                ! ...
!c keep outputs for debuging purposes
            if(index(arg(i),'-keep').ne.0)KEEP=.true. 
            if(index(arg(i),'-msc_exc').ne.0)then
               !call to_list()
               !call readl(arg(i+1),xx,nn)
               list=arg(i+1)
               if ( len(list) .eq. 0 ) then
                   write (*,*) "The list of atoms are not provided, masses will not be scaled"
               else
                   !allocate(idx)
                   print *, size(idx)
                     call string_to_integer(list)!,idx)
               endif
                   
               read(arg(i+2),*,IOSTAT=err) mscale
               if (err .ne. 0) mscale=1000000
               
            endif
         endif
      enddo
!c read possible file .SYM and .UHF

      inquire(file='.SYM',exist=da)
      if(da)then
        open(unit=21,file='.SYM')
        read(21,'(a)') sym
        if(pr) write(*,'(''!!! symmetry enforced by user in <.SYM>   : '',a,'' !!!'')')trim(sym)
        DESY=.false.
      endif
      inquire(file='.UHF',exist=da)
      if(da)then
        open(unit=21,file='.UHF')
        read(21,*) nopen
        UHF=.true.
        if(pr) write(*,'(''!!! UHF enforced by user in <.UHF> . Na-Nb:'',i4,'' !!!'')')nopen
      endif
      inquire(file='.CHRG',exist=da)
      if(da)then
        open(unit=21,file='.CHRG')
        read(21,*) charge
        if(pr)write(*,'(''!!! charge in <.CHRG> :'',i4,'' !!!'')')charge
      endif

! cbann: if trold, check for hessian files. If present, also activate TS
! and save hessian
      if(TROLD) then 
        inquire(file='hessian',exist=da)
        if(da) then
           call system("mv hessian  hss.tmp")
           TS=.true. 
        endif
        inquire(file='hessian_driver',exist=da)
        if(da) then
           call system("cat hessian_driver|sed 's/$hessian.*/$hessian (projected)/' > hss.tmp")
           TS=.true. 
        endif
      endif

      if(MOLD)call system('mv mos mos.tmp')
      if(OLDMO) then
       call system('rm -fr TMP.MOS')
       write(*,*) '* projecting old mos to new basis *'
       call system("cpc TMP.MOS &> /dev/null ")
      endif

 
      call system('rm -rf control basis auxbasis mos alpha beta')

! c why?
!      if((.not.OPT).and.(.not.MP2))scfconv=6   
!      if(CP) scfconv=8
      if(POL) then
              scfconv=7
              grid='3'
      endif
! c correct nonsense options
      if(LAP) SOS=.true.
      if(MP2.and.DFT)DFT=.false.
      if(SCS.and.DFT)DFT=.false.
      if(SOS.and.DFT)DFT=.false.
      if(CC.and.DFT)DFT=.false.
      if(MP2        )VDW=.false.
      if(MP2        )BJ=.false.    
      if(MP2        )ZERO=.false.  
      if(MP2        )ATM=.false.
      if(MP2        )D4=.false.
      if(VDW.and.BJ)BJ=.false.    
      if(MP2.or.SCS.or.SOS)OPT=.false.
      if(MP2.or.SCS) LAP=.false.
      if(rik.and.cosx) cosx=.false. ! SAW: don't use both
      
! c dfpt2 special
      if(func.eq.'b2-plyp'.or.func.eq.'b2gp-plyp'.or.func.eq.'ptpss'.or.MRCI.or.func.eq.'pwpb95'.or.func.eq.'dsd-blyp') then
         MP2  =.true.
         SCS  =.false.
         DFPT2=.true.
         FC   =.false.
         OPT  =.false.
         if(ex)FC=.true. 
         if(.not.EGRID) grid='m5'
         scfconv=7
      endif
!      if(func.eq.'b97-d'.and.DFT)VDW=.true.
!      if(func.eq.'ptpss'.or.func.eq.'pwpb95')then     
!         SOS=.true.
!         SCS=.false.
!      endif
      if(NOVDW                  )then  
        VDW=.false.                   
        BJ=.false.                    
        ATM=.false.
        ZERO=.false.                  
        D4=.false.
        donl=.false.
      endif                            
      if(donl)then
        VDW=.false.
        BJ=.false.
        ATM=.false.
        ZERO=.false.
        D4=.false.
        sym='c1'     
        DESY=.false.
      endif

!FB define r2scan-3c defaults
if(func.eq.'r2scan3c') func='r2scan-3c'
if(func.eq.'r2scan-3c') then
    !uses gcp 
    write(*,*) "Use R2SCAN-3C with radsize 10 for gas-phase optimizations and grid m4!"
    if(.not.modbas) bas='def2-mTZVPP'
    if(.not.modgrid) grid='m4'
    if(.not.novdw) then
        D4=.true.
        donl=.false.
        BJ=.false.
        ATM=.false.
    endif
endif

!JGB define PBEh-3c defaults
if(func.eq.'pbeh3c') func='pbeh-3c'
if(func.eq.'pbeh-3c'.and.DFT) then
!FB needs .and.DFT else D4 turned off for HF
    if(.not.modbas) bas='def2-mSVP'
    if(.not.modgrid) grid='m4'
    if(extol.lt.0) extol= 2.5d0
    if(.not.novdw) bj=.true.
    if(.not.modaux) noauxg=.true.
    D4=.false.
    donl=.false.
endif
!FB define B97-3c defaults
if(func.eq.'b97-3c') then
    if(.not.modbas) bas='def2-mTZVP'
    if(.not.modgrid) grid='m4'
    if(.not.novdw) then
        BJ=.true.
        ATM=.true.
        D4=.false.
        donl=.false.
    endif
    ! not sure about noauxg (ask stefan)
elseif(func.eq.'b973c') then
    write(*,*) "Using the slower b973c because of XCFun e.g. for hessian calculation"
    ! b973c uses XCFun for hessian calculation
    if(.not.modbas) bas='def2-mTZVP'
    if(.not.modgrid) grid='m4'
    if(.not.novdw) then
        BJ=.true.
        ATM=.true.
        D4=.false.
    endif
endif

!JGB define HF-3c defaults
if(hf3c) then
    write(*,*)'Setting up HF-3c calculation!'    !FB
    if(.not.modbas) bas='minix'
    if(.not.novdw) bj=.true.
    if(.not.modaux.and.RI) noauxg=.true.
    D4=.false.
    donl=.false.
endif
!FB define PBE-3c defaults
if(func.eq.'pbe3c') func='pbe-3c'
if(func.eq.'pbe-3c')then
    write(*,*) 'Setting up PBE-3c calculation (NOT PBEh-3c)!'
    func='pbe'
    gcpinfo=.true.
    if(.not.novdw)then
        BJ=.true.
        ATM=.true.
        D4=.false.
        donl=.false.
    endif
    if(.not.modgrid)then
        grid='m3'
    endif
    if(.not.modbas) then
        bas='def2-mSVP'
    endif
    if(.not.modaux) noauxg=.true.
    if(extol.lt.0) extol= 2.5d0
endif
!FB define B3-LYP-3c defaults
if(func.eq.'b3-lyp-3c'.or.func.eq.'b3lyp-3c')then
    write(*,*) 'Setting up B3-LYP-3c calculation!'
    func='b3-lyp'
    gcpinfo=.true.
    if(.not.novdw)then
        BJ=.true.
        ATM=.true.
        D4=.false.
        donl=.false.
    endif
    if(.not.modgrid)then
        grid='m4'
    endif
    if(.not.modbas) then
        bas='def2-mSVP'
    endif
    if(.not.modaux) noauxg=.true.
    if(extol.lt.0) extol= 2.5d0
endif
!FB define PBE0-3c defaults
if(func.eq.'pbe0-3c'.or.func.eq.'pbe03c')then
    write(*,*) 'Setting up PBE0-3c calculation!'
    func='pbe0'
    gcpinfo=.true.
    if(.not.novdw)then
        BJ=.true.
        ATM=.true.
        D4=.false.
        donl=.false.
    endif
    if(.not.modgrid)then
        grid='m4'
    endif
    if(.not.modbas) then
        bas='def2-mSVP'
    endif
    if(.not.modaux) noauxg=.true.
    if(extol.lt.0) extol= 2.5d0
endif



! c do reference single point?
      if(REF) then
      OPT=.false.
       if (scfconv.lt.7) then
       scfconv=7
       write(*,*) 'increasing scfconv to 7'
       endif
      endif

! Quick&Dirty

if(QUICK) then
 func='pwlda'
 bas='SVP'
 grid='1'
 OPT=.false.
endif

! c dft/mrci
      if(MRCI)then
         func='bh-lyp'
         FC   =.false.
         VDW  =.false.
         BJ   =.false.
         ZERO =.false.
         D4   =.false.
      endif
! c override the default or derived setting via input
      if(OPTI) OPT=.true. 
      if(OPTI.and.MP2) RI=.false.
! c check for hybrid
      if(NORI) then
         if( index(func,'b3-lyp').ne.0 &
              .or.index(func,'bh-lyp').ne.0 &
              .or.index(func,'b2-plyp' ).ne.0 &
              .or.index(func,'b2gp-plyp' ).ne.0 &
              .or.index(func,'ptpss' ).ne.0 &
              .or.index(func,'dsd-blyp' ).ne.0 &
              .or.index(func,'tpssh' ).ne.0 &
              .or.index(func,'pbe38' ).ne.0 &
              .or.index(func,'pw6b95' ).ne.0 &
              .or.index(func,'pwpb95' ).ne.0 &
              .or.index(func,'pbe0'  ).ne.0 &
              .or.index(func,'pwb95'  ).ne.0 &
              .or.index(func,'mpw1b95'  ).ne.0 &
              .or.index(func,'mpwb1k'  ).ne.0 &
              .or.index(func,'pbe0'  ).ne.0 &
              ) then
            RI=.false.
         elseif(.not.DFT) then 
            RI=.false.
         endif
      endif
      if(RIK) RI=.true.
      if (COSX) RI=.true.
! c how many different heavy atoms
      call atoms(nheavy,nat,ntypes,strange_elem,irare,att)
      if(nat.eq.1) OPT=.false.

      if(io.ne.6)open(unit=io,file=out)
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! c coord 
      write(io,*)'    '
      write(io,*)'    '
      if(COORD)then
         if(RANGST)then
         write(io,'(''aa '',a)')trim(coord_name)
         else
         write(io,'(''a '',a)')trim(coord_name)
         endif
      else
         if(RANGST)then
         write(io,*)'aa coord'
         else
         write(io,*)'a coord'
         endif
      endif
      if(DESY) then
         write(io,'('' desy '',f6.2)')desythr
      else
         write(io,'('' sy '',a)')trim(sym)
      endif
      if(OPT ) write(io,'('' ired '')')
      write(io,*)'*'
      if(.not.OPT ) write(io,'('' no '')')
! c basis
! hok
! own basis library in /$HOME/.definerc
      if(LIB) then
       write(io,*) 'lib'
       write(io,*) libnr
      endif
      write(io,'('' b all '',a20)') bas
! ECPs
      if(ECP) then
      write(*,*) 'WARNING: Check BASIS/ECPs !!'
          write(io,*)'ecp  all ecp-2-sdf'
        do l=1,ntypes    
         write(io,'('' '')') 
         write(io,'('' '')') 
        enddo
          write(io,*)'ecp  all ecp-10-sdf'
        do l=1,ntypes    
         write(io,'('' '')') 
         write(io,'('' '')') 
        enddo
      endif
! c add diffuse   
      if(diffuse)then
         open(unit=142,file='dbas')
         do i=1,ntypes
         read(142,'(a)')dbas
! c        if(att(i).le.2)dbas='1s1p' 
         if(pr)write(*,*) 'adding functions ',dbas
         write(io,'('' bm'')') 
         if(ntypes.gt.1)write(io,'('' #'',i1)') i  
         write(io,'('' flat'')')   
         write(io,'(a)') dbas  
         write(io,'('' '')') 
         write(io,'('' '')') 
         write(io,'('' '')') 
         write(io,'('' '')') 
         enddo
         close(142)
      endif

! RMGF -gf for def2-QZVP
      DATA baselem/'he', &
      'li','be','b ','c ','n ','o ','f ','ne',  &
      'na','mg','al','si','p ','s ','cl','ar',  &
      'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',  &
      'zn','ga','ge','as','se','br','kr',  &
      'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',  &
      'cd','in','sn','sb','te','i ','xe',  &
      'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb','dy',  &
      'ho','er','tm','yb','lu','hf','ta','w ','re','os','ir','pt',  &
      'au','hg','tl','pb','bi','po','at','rn'/

      if(RMGF)then
        write(io,'('' bm'')')          
        write(io,'(''h def2-QZVP'')') 
        write(io,'(''del'')')
        write(io,'(''f'')')
        write(io,'('' '')')
        do i=1,85
          write(io,'('' bm'')')
          write(io,'(a,1x,a)')trim(baselem(i)),'def2-QZVP'
          !write(io,'(''b def2-QZVP'')')
          write(io,'(''del'')')
          write(io,'(''g'')')
          write(io,'('' '')')
        enddo
      endif

! RMF -f for all def2_TZVPD
      DATA baselem2/ &
      'b ','c ','n ','o ','f ','ne',  &
      'al','si','p ','s ','cl','ar',  &
      'sc','ti','v ','cr','mn','fe','co','ni','cu',  &
      'zn','ga','ge','as','se','br','kr',  &
      'y ','zr','nb','mo','tc','ru','rh','pd','ag',  &
      'cd','in','sn','sb','te','i ','xe',  &
      'la','hf','ta','w ','re','os','ir','pt',  &
      'au','hg','tl','pb','bi','po','at','rn'/

      if(RMF)then
        do i=1,60
          write(io,'('' bm'')')
          write(io,'(a,1x,a)')trim(baselem2(i)),'def2-TZVPD'
          write(io,'(''del'')')
          write(io,'(''f'')')
          write(io,'('' '')')
        enddo                                               
      endif

! c CP correction
      if(CP1.or.CP2)then
         call system('splitmol > splitmol.tmp')
         open(unit=43,file='splitmol.tmp')
         read(43,'(a)')atmp
         read(43,'(a)')atmp
         read(43,'(a)')atmp
         read(43,'(a)')atmp
         if(CP2)then
         read(43,'(a)')atmp
         read(43,'(a)')atmp
         endif
         write(io,*)'c'
         write(io,*)trim(atmp)
         write(io,*)'0.0'
         close(43,status='delete')
         close(43)
      endif
      write(io,*)'*'
! c HUECKEL
      if(OLDMO)then
      write(io,*)'use TMP.MOS/control'
      else
      write(io,*)'eht '
      if(MODEHT)then
         write(io,*)'n'
         write(io,*)'y'
         write(io,*)'1.1'
         write(io,*)'n'
         write(io,*)'n'
      endif
! cts
      if(strange_elem) write(io,*)'    '
      write(io,*)'    '
! c     do i=1,1+nheavy
! c        write(io,*)'    '
! c     enddo
      write(io,*)charge 
      if(nat.eq.1)write(io,*)'    '
      if(UHF)then
         write(io,*)'n'
         if(nopen.ne.0)then
            write(io,*)'uf ',nopen
         else
! we want maybe triplett cases
            write(io,*)'s'
         endif
! ?? why list?? the list
!         write(io,*)'l '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
      if(irare.eq.0)write(io,*)'*'
      endif !uhf
      endif ! mos-setup
      write(io,*)'    '
      write(io,*)'    '
      write(io,*)'    '
      write(io,*)'    '
      write(io,*)'    '
      write(io,*)'    '
      write(io,*)'    '
! c DFT
      if(DFT.or.DFPT2)then
         write(io,*)'dft'
         write(io,*)'on '
         write(io,*)'func'
         write(io,*)'b-p'
! c         write(io,'(a20)') func
         write(io,*)'grid'
         write(io,'(a20)') grid
         write(io,*)'q'
      endif
      if(RI)then
         write(io,*)'ri '
         write(io,*)'on '
         if(ALIB) then
          do l=1,ntypes     ! hok
          write(io,*)'newlib '
          write(io,*) trim(newlib)
          write(io,*) 'lib'
          enddo
         endif
         write(io,*)'m  '
         write(io,*)ricore
         write(io,*)'q'
      endif
      if(RIK)then
      write(io,*)'rijk'
      write(io,*)'on'   
      write(io,*)'m'   
      write(io,*)ricore
      write(io,*)'q' 
      ! SAW added COSX support
      elseif(COSX) then
      write(io,*)'senex'
      write(io,*)'on'
      write(io,*)'y'
      write(io,*)'g'
      write(io,'(a20)') grid
      write(io,*)'y'
      write(io,*)'q'
      endif

! c special
      write(io,*)'scf'
      write(io,*)'iter'
      write(io,*)maxiter
      write(io,*)'conv'
      write(io,*)scfconv
      write(io,*)'thi'
      write(io,*)thize
      write(io,*)thime
      write(io,*)'ints'
      write(io,*)'y'
      write(io,*)intmem,' twoint'
      write(io,*)'*'                
      write(io,*)'    '
! c mp2
      if(MP2.or.SCS.or.SOS)then
        write(io,*)'mp2'
         write(io,*)'cbas'
         if(ALIB) then
          do l=1,ntypes     ! hok
          write(io,*)'newlib '
          write(io,*) trim(newlib)
          write(io,*) 'lib'
          enddo
         endif
         write(io,*)'*'
         if(FC)then
            write(io,*)'freeze'
            write(io,*)'fp ',fp 
            write(io,*)'*'
         endif
         write(io,*)'other'
         if(.not.OPT)write(io,*)'emp2'
         write(io,*)'*'
        write(io,*)'*'
      endif
! c CCSD(T)
       if(CC) then
         write(io,*)'cc2'
         write(io,*)'cbas'
         if(ALIB) then
          do l=1,ntypes     ! hok
          write(io,*)'newlib '
          write(io,*) trim(newlib)
          write(io,*) 'lib'
          enddo
         endif
         write(io,*)'*'
         if(FC)then
            write(io,*)'freeze'
            write(io,*)'fp ',fp
            write(io,*)'*'
         endif
         write(io,*) 'ricc2'
         write(io,*) 'ccsd(t)'
         write(io,*) '*'
        write(io,*)'*'
       endif
! c R12
      if(R12)then
        write(io,*)'cc2'
        write(io,*)'mp2-f12'
        write(io,*)'*'
        write(io,*)'cabs'
        write(io,*)'*'
       write(io,*)'*'
      endif

      if(RIK.and.TRUNC)then
         write(io,*)'trunc'
      endif
! c end
      write(io,*)'q'
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if(io.ne.6)close(io)

      run1='HF'
      if(DFT)run1='DFT'
      if(MP2)run1='RI-MP2'
      if(SCS)run1='RI-SCS-MP2'
      if(SOS)run1='RI-SOS-MP2'
      if(DFT.and.RI)run1='RI-DFT'
      if(DFPT2)run1='DHDF'
      if(DFPT2.and.RI)run1='RI-DHDF'
      if(DFT.or.DFPT2)then
      write(run2,'(a,''('',a2,'')'')')trim(run1),trim(grid)
      else
      func=''
      run2=run1
      endif
      if(VDW)write(run2,'(a,''-D'')')trim(run2)

      if(pr)write(*,'('' * '',a,''-'',a,''/'',a,'' * '')')trim(run2),trim(func),trim(bas)

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if(.not.TEST) then
         call system('rm -f control basis auxbasis mos alpha beta')

         deletion_failed=.false.
         INQUIRE(FILE="auxbasis", EXIST=da)
         if(da)deletion_failed=.true.
         INQUIRE(FILE="basis", EXIST=da)
         if(da)deletion_failed=.true.
         INQUIRE(FILE="control", EXIST=da)
         if(da)deletion_failed=.true.
         INQUIRE(FILE="mos", EXIST=da)
         if(da)deletion_failed=.true.
         INQUIRE(FILE="alpha", EXIST=da)
         if(da)deletion_failed=.true.
         INQUIRE(FILE="beta", EXIST=da)
         if(da)deletion_failed=.true.

         if(deletion_failed) stop 'files remaining from previous setup, exiting'

      call flush(6) ! cbannwarth 21.08.13: use unit=6 (stdout) to prevent crash when executing ifort-compiled binary
!         call flush
!         call system('sync')
! FB use old $tmole script to keep standartized define, else error with Pd and charge in TM.7.2.1
         !call system('echo ''$tmole'' > control; echo ''$end'' >> control')
         call system(run3)
      else
         stop 'def.inp written.'
      endif
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! c     call system('sdg symmetry')
      if(pr)write(*,'(''#atoms='',i3)')nat
! c     call system('grep "nbf(AO)" control')
      call system('eiger > tmp.eiger')
      call system('grep "Number of MOs" tmp.eiger')
      
      if(MRCI) then
! c       call freeze(UHF)
        if(pr)then
        write(*,*)'!!! SET DFT/MRCI FROZEN ORBS AFTER SCF MANUALLY !!!'
        write(*,*)'    (e.g. freeze script: freeze -3.0 2.0)'
        write(*,*)'writing mrci example input <dftmrci.inp> '
        endif
        open(unit=33,file='dftmrci.inp')
        write(33,*)'$method dftci'                             
        write(33,*)'$roots 2 0 0 0 0 0 0 0'                    
        write(33,*)'$mul=1'                    
        write(33,*)'$ciref 4 4 2'                    
        write(33,*)'$esel=0.8'                    
        close(33)
      endif

      if(EX)then
        call system("echo '$scfinstab rpas' >>control")
        call system("echo '$soes' >>control")
        call system("echo ' <irrep> <# states>' >>control")
        call freeze(UHF)
        if(pr)then
        write(*,*)'!!! SETTING MP2 FROZEN CORE !!!'
        write(*,*)'!!! PLEASE ADJUST $soes     !!!'
        endif
      endif

! c     if( index(func,'fith').ne.0)then
! c         call system('kdg dft')
! c         call system('kdg end')
! c         call system("echo '$dft' >>control")
! c         call system("echo ' functional fith' >>control")
! c         call system("echo ' gridsize m4'     >>control")
! c         call system("echo '$end' >>control")
! c     elseif(index(func,'fit').ne.0)then
! c         call system('kdg dft')
! c         call system('kdg end')
! c         call system("echo '$dft' >>control")
! c         call system("echo ' functional fit' >>control")
! c         call system("echo ' gridsize m4'    >>control")
! c         call system("echo '$end' >>control")
! c     endif
          
! c specials
      if(POL)call system('kdg dft')
      call system('kdg maxcor')
      call system('kdg rpacor')
      call system('kdg end')
      write(atmp,1001)maxcor     
      call system(atmp)                                 
 1001 format(' echo ''$maxcor ',i5,' ''>>control')
      write(atmp,1002)rpacor     
      call system(atmp)                                 
 1002 format(' echo ''$rpacor ',i5,' ''>>control')

      call system("echo '$pop' >>control")
      if(RI.and.(.not.MP2)) call system("echo '$jbas file=auxbasis'>>control")

      if(VDW) call system("echo '$vdwx' >>control")
! c     if(DFPT2) call system("kdg denconv")
      if(FOLD)  call system("kdg forceinit")
      if(OR)then
         call system("echo '$scfinstab dynpol nm'>>control")
         call system("echo ' 589.3'>>control")
         call system("echo '$rpacor 1000'>>control")
         call system("echo '$rpaconv 4 '>>control")
         call system("echo '$velocity gauge '>>control")
      endif
      if(FON)then
      call system("echo '$fermi tmstrt=5000.0 tmend=5000.0 tmfac=1.000' &
                       ' hlcrt=1.0 stop=0.001' >>control")
      endif
      if(TS) then
         call system("echo '$statpt'   >>control")
         call system("echo '   itrvec 1' >>control")
         call system("echo '   tradius 0.05 ' >>control")
         call system("echo '   radmax  0.05 ' >>control")
         call system("echo '   threchange  5.0d-7' >>control")
         call system("echo '   thrrmsgrad  5.0d-5' >>control")
         call system("echo '   thrmaxdispl 1.0d-1' >>control")
         call system("echo '   thrrmsdispl 1.0d-1' >>control")
      else  
         call system("echo '$statpt'   >>control")
         call system("echo '   itrvec 0' >>control")
         call system("echo '   tradius 0.3 ' >>control")
         call system("echo '   threchange  5.0d-7' >>control")
         call system("echo '   thrrmsgrad  5.0d-5' >>control")
         call system("echo '   thrmaxdispl 1.0d-1' >>control")
         call system("echo '   thrrmsdispl 1.0d-1' >>control")
      endif
! specify where to find hessian
      if(TROLD) then    
         call system('mv hss.tmp hessian')
         open(file='hessian',unit=44,status='old')
         rewind(44)
         hsstmp='                    '
         read(44,'(a20)')hsstmp
         close(44)
         write(atmp,'(a,a,a)')"echo '",hsstmp," file=hessian' >> control"
         call system(atmp)
      endif   
      if(COSMO) then
         call system("echo '$cosmo'   >>control")
         write(atmp,1003)cosmodk    
 1003    format(' echo '' epsilon= ',f6.2,' ''>>control')
         call system(atmp)                                 
      endif
      if(POL)then
         call system("echo '$scfinstab dynpol a.u.'>>control")
         call system("echo ' 0.000001 i'>>control")
         call system("echo ' .05 i'>>control")
         call system("echo ' 0.1 i'>>control")
         call system("echo ' 0.2 i'>>control")
         call system("echo ' 0.3 i'>>control")
         call system("echo ' 0.4 i'>>control")
         call system("echo ' 0.5 i'>>control")
         call system("echo ' 0.6 i'>>control")
         call system("echo ' 0.7 i'>>control")
         call system("echo ' 0.8 i'>>control")
         call system("echo ' 0.9 i'>>control")
         call system("echo ' 1.0 i'>>control")
         call system("echo ' 1.2 i'>>control")
         call system("echo ' 1.4 i'>>control")
         call system("echo ' 1.6 i'>>control")
         call system("echo ' 1.8 i'>>control")
         call system("echo ' 2.0 i'>>control")
         call system("echo ' 2.5 i'>>control")
         call system("echo ' 3.0 i'>>control")
!        call system("echo ' 4.0 i'>>control")
!        call system("echo ' 5.0 i'>>control")
!        call system("echo ' 7.5 i'>>control")
!        call system("echo '10.0 i'>>control")
         call system("echo '$rpacor 1000'>>control")
         call system("echo '$rpaconv 4 '>>control")
         call system("echo '$escfiterlimit 250'>>control")
         call system("echo '$dft'>>control")
         call system("echo '  functional pbe38'>>control")
         call system("echo '  gridsize 4'>>control")
      endif

      if(pr)then
         call system('grep "HOMO/LUMO-SEPARATION" define.out')
         if(pr)then
         if(MP2.and.OPT.or.SCS.and.OPT) then
         write(*,*)'!!! MP2 OPTIMIZATION REQUESTED !!!'
         endif
         endif
      endif

      if(FOLD)then
         call system("echo '$forceinit off'>>control")
         call system("rm gradient")
      endif

! c   handle SCS-MP2 SOS-MP2 LP-SOS-MP2 MP2 settings
      if(MP2.or.SCS.or.SOS)then
         call system("echo '$ricc2'>>control")
         if(SOS)then
            call system("echo ' sos'>>control")
         elseif(SCS) then
            call system("echo ' scs cos=1.2  css=0.333333333'>>control")
         else
            if(.not.dfpt2) call system("echo ' scs cos=1.0  css=1.0'>>control")
         endif
         

         if(.not.OPT)then
            call system("echo ' mp2 energy only'>>control")
         else
            call system("echo ' geoopt model=mp2'>>control")
         endif
         if(LAP) then
         
         call system("echo '$laplace' >>control")
         call system("echo 'conv=4' >>control")
         endif
      endif


! handle double-hybrids
!PT2
!B2PLYP 0.27
!B2GPPLYP
!PWPB95
!PTPSS
      if(func.eq.'b2-plyp') then
        call system("echo ' scs cos=0.27  css=0.27 '>>control")
      elseif (func.eq.'dsd-blyp') then
       call system("echo ' scs cos=0.460  css=0.370 '>>control")
      elseif (func.eq.'b2gp-plyp') then
        call system("echo ' scs cos=0.36  css=0.36 '>>control")
     elseif (func.eq.'ptpss') then
        call system("echo ' scs cos=0.375  css=0.0'>>control")
      elseif (func.eq.'pwpb95') then
        call system("echo ' scs cos=0.269  css=0.0 '>>control")
      endif


      if(marij) then
      call system("echo '$marij' >> control")
      endif
      if(BJ.and.ZERO) then
       write(*,*) '** WARNING: unclear D3 options **'
       write(*,*) '** cefine -zero OR cefine -d3  **'
      endif
      if(BJ.and.D4) then
       write(*,*) '** WARNING: unclear D3 /D4 option! **'
       write(*,*) '   found BJ and D4 '
      endif
      !FB D3 Dispersion edited 
      if (BJ.and.hf3c) then
        call system("echo '$disp3 -bj -func hf3c' >> control")
      elseif(BJ.and.ATM) then
        call system("echo '$disp3 -bj -abc' >> control")
      elseif(BJ.and..not.hf3c) then 
        call system("echo '$disp3 -bj' >> control")
      !FB D4 dispersion
      elseif(D4)then
         call system("echo '$disp4 ' >> control")
         write(*,*) 'D4 selected'
      endif
      if(ZERO) call system("echo '$disp3 ' >> control")
      !FB NL
      if(donl) call system("echo '$donl ' >> control")
! c      if(func.eq.'b2gp-plyp'.or.func.eq.'ptpss') then
! c       call system('kdg dft')
! c       call system("echo '$dft'>>control")
! c       if(func.eq.'ptpss')
! c     . call system("echo '  functional ptpss' >>control")
! c       if(func.eq.'b2gp-plyp')
! c     . call system("echo '  functional b2gp-plyp' >>control")
! c       call system("echo ' gridsize m5' >> control")
! c      endif
      !FB gcp in case of pbe-3c, pbe0-3c, b3-lyp-3c
      if(gcpinfo)then
          call system("echo '$gcp dft/sv(p)' >> control")
      endif
    
      !FB change radsize (radial grid points) this is independent
      !   of gridsize !!!
      if(modrad) then
        write(atmp,"(a,i0,a)") "sed -i '/gridsize/a\   radsize    ",radsize,"' control"
        call system(trim(atmp))
      endif
      

!JGB modify K tolerance
if(extol.gt.0.0d0) then
write (atmp, "(a,a,f6.3,a)") "echo ", "'$extol ",extol," ' >> control"
call system(atmp)
endif

!JGB mulipole accelerated RI for GGA, PBEh-3c, HF-3c if natoms>200
if(nat.gt.200)then 
if( index(func,'b3-lyp').ne.0 &
     .or.index(func,'bh-lyp').ne.0 &
     .or.index(func,'b2-plyp' ).ne.0 &
     .or.index(func,'b2gp-plyp' ).ne.0 &
     .or.index(func,'ptpss' ).ne.0 &
     .or.index(func,'dsd-blyp' ).ne.0 &
     .or.index(func,'tpssh' ).ne.0 &
     .or.index(func,'pbe38' ).ne.0 &
     .or.index(func,'pw6b95' ).ne.0 &
     .or.index(func,'pwpb95' ).ne.0 &
     .or.index(func,'pbe0'  ).ne.0 &
     .or.index(func,'pwb95'  ).ne.0 &
     .or.index(func,'mpw1b95'  ).ne.0 &
     .or.index(func,'mpwb1k'  ).ne.0 &
     .or.index(func,'pbe0'  ).ne.0 &
              ) then
else
   atmp="echo '$marij' >> control"
   call system(trim(atmp))
   atmp="echo '   precision   0.100D-06' >> control"
   call system(trim(atmp))
   atmp="echo '   lmaxmom            12' >> control"
   call system(trim(atmp))
endif
endif

! chok  set functional
       atmp='sed -i s/b-p/'//trim(func)//'/ control'
      if(KEEP) write(*,*) 'sed functional' ,atmp
      call system(trim(atmp))
! c hok set reference gridsize
      if (REF) then
      atmp='sed -i s/gridsize.*$/reference/ control'
      call system("echo '$denconv 1d-4' >> control")
      call system("echo '$scftol 1e-13' >> control")
!      atmp='sed -i s/gridsize/reference/ control'
      if(KEEP) write(*,*) 'sed grid' ,atmp
      call system(trim(atmp))
      endif

! c use old mos file
      if(MOLD)call system('mv mos.tmp mos')

! set difference densities expansion to zero
      if(NDIFF) call system("echo '$scfdenapproxl 0' >> control")

! "close" control file
      call system("echo '$end' >>control")
! c print some control file settings
      if(ECHO)then
        write(*,'(''==============================================='')')
        write(*,*)'IMPORTANT FINAL CONTROL SETTINGS:'
        call system('echo $TURBODIR')
        call system('sdg symmetry')
        call system('sdg atoms')
        call system('sdg rij')
        call system('sdg rik')
        call system('sdg dft')
        call system('sdg vdwx')
        call system('sdg closed')
        call system('sdg alpha shells')
        call system('sdg beta shells')
        call system('sdg ricc2')
        write(*,'(''==============================================='')')
      endif


!JGB modify auxbasis
if(noauxg) then
INQUIRE(FILE="auxbasis", EXIST=da)
if(da) then
do i =1,9
write (atmp, "(a,i1,a)") "sed -i '/",i,"\ \ g/,+1 d' auxbasis"
call system(atmp)
enddo
endif
endif


      if(.not.KEEP) call system('rm -rf define.out def.inp prep.inp tmp.eiger')
      if(OLDMO.and..not.keep) call system("rm -r TMP.MOS")
    end Program comand_line_define


! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! c     integer n
! c     call atoms(n)
! c     write(*,*) n
! c     end
! 
! c returns the number of heavy (n>10) atoms in a coord file, and the number of diff atom types
! c sets cu_pd true for some special elements that need more input
      subroutine atoms(n,nat,nt,cu_pd,irare,att)
      implicit none
      integer n,i,j,nn,nat,irare,att(20)
      logical cu_pd
      character*80 a80
      real*8 xx(10)
      integer na(110),nt,na2(110)

      cu_pd=.false.
      na = 0
      irare = 0
      na2=0
      n=0
      nat=0
      j=0
      nt=0
      open(unit=1,file='coord')
      read(1,'(a)',end=100) a80
 10   read(1,'(a)',end=100) a80
      call readl(a80,xx,nn)
      if(index(a80,'$').ne.0)goto 100
      if(nn.eq.3)then
         nat=nat+1
         j=j+1
         call elem(a80,i)
! select rare gas and
         if(i.eq.2.or.i.eq.10.or.i.eq.18.or. &
          i.eq.36.or.i.eq.54.or.i.eq.86)irare=1
         na2(i)=na2(i)+1
         if(na2(i).eq.1)then
            nt=nt+1
            att(nt)=i
         endif
         if(i.gt.10) na(i)=na(i)+1
! cts check for Cu/Pd problem
         if(i.eq.29.or.i.eq.46) cu_pd=.true.
      endif
      goto 10
100   close(1)

      n=0
      do i=1,110
         if(na(i).gt.0)n=n+1
      enddo

      end

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE ELEM(KEY1, NAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*(*) KEY1
      CHARACTER*2 ELEMNT(107),E

      DATA ELEMNT/'h ','he', &
      'li','be','b ','c ','n ','o ','f ','ne',  &
      'na','mg','al','si','p ','s ','cl','ar',  &
      'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',  &
      'zn','ga','ge','as','se','br','kr',  &
      'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',  &
      'cd','in','sn','sb','te','i ','xe',  &
      'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb','dy',  &
      'ho','er','tm','yb','lu','hf','ta','w ','re','os','ir','pt',  &
      'au','hg','tl','pb','bi','po','at','rn',  &
      'fr','ra','ac','th','pa','u ','np','pu','am','cm','bk','cf','xx',  &
      'fm','md','cb','xx','xx','xx','xx','xx'/
     
      nat=0
      e='  '
      k=1
      DO J=1,len(key1)
         if (k.gt.2)exit
         N=ICHAR(key1(J:J))
         if(n.ge.ichar('A') .and. n.le.ichar('Z') )then
            call lower(key1(j:j))
            N=ICHAR(key1(J:J))
         endif
         if(n.ge.ichar('a') .and. n.le.ichar('z') )then
            e(k:k)=key1(j:j)
            k=k+1
         endif
      enddo

      DO I=1,107
         if(e.eq.elemnt(i))then
            NAT=I
            RETURN
         ENDIF
      ENDDO

      end

! C     *****************************************************************         

      SUBROUTINE lower(AS)
      CHARACTER*1 AS
      AS=CHAR(ICHAR(AS)-ICHAR('A')+ICHAR('a'))
      END

! C     *****************************************************************         

      SUBROUTINE backstring(A1,A2,lena2)
      CHARACTER*(*) A1
      CHARACTER*(*) A2
      integer n,lena2
      n=0
      DO J=1,len(a1)
         if(a1(j:j).ne.' ')then
            n=n+1
            a2(n:n)=a1(j:j)
         endif
      enddo
      DO J=1,lena2   
         a2(j:j)=' '
      enddo
      a1=a2
      a2='                                                            '
      n=0
      DO J=1,len(a1)
         if(a1(j:j).ne.' ')then
            n=n+1
            a2(n:n)=a1(j:j)
         endif
      enddo

      END


! C     *****************************************************************         
                                                                                
      SUBROUTINE READL(A1,X,N)                                               
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      CHARACTER*(*) A1                                                      
      DIMENSION X(*)                                                            
      I=0                                                                       
      IS=1                                                                      
  10  I=I+1                                                                     
      X(I)=READAA(A1,IS,IB,IE)                                               
      IF(IB.GT.0 .AND. IE.GT.0) THEN                                            
                                IS=IE                                           
                                GOTO 10                                         
      ENDIF                                                                     
      N=I-1                                                                     
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
      FUNCTION READAA(A,ISTART,IEND,IEND2)                                   
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      REAL*8 READAA                                                             
      CHARACTER*(*) A                                                      
      NINE=ICHAR('9')                                                           
      IZERO=ICHAR('0')                                                          
      MINUS=ICHAR('-')                                                          
      IDOT=ICHAR('.')                                                           
      ND=ICHAR('D')                                                             
      NE=ICHAR('E')                                                             
      IBL=ICHAR(' ')                                                            
      IEND=0                                                                    
      IEND2=0                                                                   
      IDIG=0                                                                    
      C1=0                                                                      
      C2=0                                                                      
      ONE=1.D0                                                                  
      X = 1.D0                                                                  
      NL=LEN(A) 
      DO 10 J=ISTART,NL-1                                                       
         N=ICHAR(A(J:J))                                                          
         M=ICHAR(A(J+1:J+1)) 
         IF(N.LE.NINE.AND.N.GE.IZERO .OR.N.EQ.IDOT)GOTO 20                      
         IF(N.EQ.MINUS.AND.(M.LE.NINE.AND.M.GE.IZERO &                            
      .OR. M.EQ.IDOT)) GOTO 20                                                 
   10 CONTINUE                                                                  
      READAA=0.D0                                                               
      RETURN                                                                    
   20 CONTINUE                                                                  
      IEND=J                                                                    
      DO 30 I=J,NL                                                              
         N=ICHAR(A(I:I))                                                          
         IF(N.LE.NINE.AND.N.GE.IZERO) THEN                                      
            IDIG=IDIG+1                                                         
            IF (IDIG.GT.10) GOTO 60                                             
            C1=C1*10+N-IZERO                                                    
         ELSEIF(N.EQ.MINUS.AND.I.EQ.J) THEN                                     
            ONE=-1.D0                                                           
         ELSEIF(N.EQ.IDOT) THEN                                                 
            GOTO 40                                                             
         ELSE                                                                   
            GOTO 60                                                             
         ENDIF                                                                  
   30 CONTINUE                                                                  
   40 CONTINUE                                                                  
      IDIG=0                                                                    
      DO 50 II=I+1,NL                                                           
         N=ICHAR(A(II:II))                                                         
         IF(N.LE.NINE.AND.N.GE.IZERO) THEN                                      
            IDIG=IDIG+1                                                         
            IF (IDIG.GT.10) GOTO 60                                             
            C2=C2*10+N-IZERO                                                    
            X = X /10                                                           
         ELSEIF(N.EQ.MINUS.AND.II.EQ.I) THEN                                    
            X=-X                                                                
         ELSE                                                                   
            GOTO 60                                                             
         ENDIF                                                                  
   50 CONTINUE                                                                  
! C                                                                               
! C PUT THE PIECES TOGETHER                                                       
! C                                                                               
   60 CONTINUE                                                                  
      READAA= ONE * ( C1 + C2 * X)                                              
      DO 55 J=IEND,NL                                                           
         N=ICHAR(A(J:J))                                                          
         IEND2=J                                                                
         IF(N.EQ.IBL)RETURN                                                     
   55 IF(N.EQ.ND .OR. N.EQ.NE)GOTO 57                                           
      RETURN                                                                    
                                                                                
   57 C1=0.0D0                                                                  
      ONE=1.0D0                                                                 
      DO 31 I=J+1,NL                                                            
         N=ICHAR(A(I:I))                                                          
         IEND2=I                                                                
         IF(N.EQ.IBL)GOTO 70                                                    
         IF(N.LE.NINE.AND.N.GE.IZERO) C1=C1*10.0D0+N-IZERO                      
         IF(N.EQ.MINUS)ONE=-1.0D0                                               
   31 CONTINUE                                                                  
   61 CONTINUE                                                                  
   70 READAA=READAA*10**(ONE*C1)                                                
      RETURN                                                                    
      END                                                                       

! C     *****************************************************************         

      subroutine freeze(uhf)
      implicit real*8 (a-h,o-z)
      character*60 a,line(2000),dum
      character*5  lab(8)
      dimension xx(10),irf(8,4)
      logical uhf,ex

      irf=0
      inquire(file='~/.freeze',exist=ex)
      if(ex)then
        open(unit=11,file='~/.freeze')
        read(11,*)thr1,thr2
        close(11)
      else
        thr1=-5.0
        thr2= 1.d+9
      endif

      line=' '

      if(uhf)then
         call system('grep eigenvalue alpha > freeze.tmp')
      else
         call system('grep eigenvalue mos > freeze.tmp')
      endif

      open(unit=1,file='freeze.tmp')

      ir=1
      n=0
      imem=0
      i=1
 10   read(1,'(a)',end=100)line(i)
      i=i+1
      goto 10
 100  continue
      nl=i-1
      close(1)

      do i=1,nl
      a=line(i)
      call readl(a,xx,nn)
      n=n+1
      if(xx(nn-1).gt.thr1.and.imem.eq.0)then
         irf(ir,2)=n-1
         imem=1
      endif
      lab(ir)=a(7:11)
      dum    =line(i+1)
      if(lab(ir).ne.dum(7:11))then
         irf(ir,1)=1  
         lab(ir)=a(7:11)
         ir=ir+1
         n=0
         imem=0
      endif
      enddo   

      open(unit=1,file='freeze.tmp')
      ir=1
      n=0
      imem=0
      do i=1,nl
      a=line(i)
      call readl(a,xx,nn)
      n=n+1
      if(xx(nn-1).gt.thr2.and.imem.eq.0)then
         irf(ir,3)=n
         imem=1
      endif
      lab(ir)=a(7:11)
      dum    =line(i+1)
      if(lab(ir).ne.dum(7:11))then
! c        irf(ir,4)=nao
         lab(ir)=a(7:11)
         ir=ir+1
         n=0
         imem=0
      endif
      enddo
 
      ir=ir-1

      rewind 1
      write(1,'(''$freeze'')')
      do i=1,ir
         if(irf(i,2).gt.0.and.irf(i,3).gt.0) &
        write(1,500)lab(i),irf(i,1),irf(i,2),irf(i,3),irf(i,4)
         if(irf(i,2).eq.0.and.irf(i,3).gt.0) &
        write(1,501)lab(i),irf(i,3),irf(i,4)
         if(irf(i,3).eq.0.and.irf(i,2).ne.0) &
        write(1,501)lab(i),irf(i,1),irf(i,2) 
      enddo
      write(1,'(''$end'')')
      close(1)

      call system('kdg freeze')
      call system('kdg end')
      call system('cat freeze.tmp >> control')

! c     write(*,*) 
! c     write(*,*)'the following data group will be inserted into control'
! c     write(*,*) 
! c     call system('cat freeze.tmp')

      call system('rm freeze.tmp')


 500  format(2x,a5,2x,i3,'-',i3,', ',i3,'-',i3)
 501  format(2x,a5,2x,i3,'-',i3)

      end
      


      subroutine susy(sym,DESY)
! c     program    susy
      implicit none
      character*20 a
      character*80 sym
      character*3  g(5)
      integer n,i
      logical DESY

      sym=''
      write(*,*)'======= automatic Abel-subgoup adjustment ======'
      call system('rm -rf control')
      open(unit=1,file='def.dum')
      write(1,*)
      write(1,*)
      write(1,*)'a coord'
      write(1,*)'desy 0.03'
      write(1,*)'*'
      write(1,*)'no'
      close(1)
 
      call system('define_huge < def.dum > def.out')
      call system('sdg symmetry > def.out')
      open(unit=1,file='def.out')
      read(1,'(a)') a
      close(1)

      call system('rm control def.dum def.out tmp.input')

      n=0
      if(index(a,' c3 ').ne.0)then
         write(*,*) 'Group: C3'
         n=1
         g(1)='c1'
      endif
      if(index(a,' oh').ne.0)then
         write(*,*) 'Group: Oh'
         n=2
         g(1)='d4h'
         g(2)='d2h'
      endif
      if(index(a,' d4h').ne.0)then
         write(*,*) 'Group: D4h'
         n=1
         g(1)='d2h'
      endif
      if(index(a,' d6h').ne.0)then
         write(*,*) 'Group: D6h'
         n=1
         g(1)='d2h'
      endif
      if(index(a,' d4d').ne.0)then
         write(*,*) 'Group: D4d'
         n=2
         g(1)='c4v'
         g(2)='c2v'
      endif
      if(index(a,' d5h').ne.0)then
         write(*,*) 'Group: D5h'
         n=1
         g(1)='c2v'
      endif
      if(index(a,' c5v').ne.0)then
         write(*,*) 'Group: C5v'
         n=1
         g(1)='cs'
      endif
      if(index(a,' c6v').ne.0)then
         write(*,*) 'Group: C6v'
         n=1
         g(1)='c2v'
      endif
      if(index(a,' td').ne.0)then
         write(*,*) 'Group: Td'
         n=2
         g(1)='d2d'
         g(2)='c2v'
      endif
      if(index(a,' d2d').ne.0)then
         write(*,*) 'Group: D2d'
         n=1
         g(1)='d2'
      endif
      if(index(a,' d3h').ne.0)then
         write(*,*) 'Group: D3h'
         n=1
         g(1)='c2v'
      endif
      if(index(a,' d3d').ne.0)then
         write(*,*) 'Group: D3d'
         n=1
         g(1)='c2h'
      endif
      if(index(a,' c3v').ne.0)then
         write(*,*) 'Group: C3v'
         n=1
         g(1)='cs'
      endif
      if(index(a,' c3h').ne.0)then
         write(*,*) 'Group: C3h'
         n=1
         g(1)='cs'
      endif
      if(index(a,' c4h').ne.0)then
         write(*,*) 'Group: C4h'
         n=1
         g(1)='c2h'
      endif
      if(index(a,' c6h').ne.0)then
         write(*,*) 'Group: C6h'
         n=1
         g(1)='c2h'
      endif
      if(index(a,' d3 ').ne.0)then
         write(*,*) 'Group: D3'
         n=1
         g(1)='c2'
      endif
      if(index(a,' s6 ').ne.0)then
         write(*,*) 'Group: S6'
         n=1
         g(1)='ci'
      endif

      if(index(a,' ih ').ne.0)then
         write(*,*) 'Group: Ih'
         n=1
         g(1)='d2h'
      endif

      if(n.eq.0)then
         DESY=.true.
         return
      endif

      sym=g(n)
      DESY=.false.
      write(*,*) 'adjusted to:',g(n)

      open(unit=1,file='def.dum')
      write(1,*)
      write(1,*)
      write(1,*)'a coord'
      write(1,*)'desy 0.03'
      write(1,*)'susy'
      do i=1,n
         write(1,'(a)')g(i)
      enddo
      write(1,*)' '
      write(1,*)' '
      write(1,*)'*'
      write(1,*)'no'
      write(1,*)    
      close(1)

      call system('rm -rf control')
      call system('define_huge < def.dum > def.out')

      call system('rm control def.dum def.out tmp.input')
41    continue

      write(*,*)'=======          done                     ======'
      end


! c     read XYZ (xmol) files. the first two lines (#atoms, blank) can be omitted

      subroutine xyzrd(xyz,iat,nat,infile)
      implicit none
      character*2 elemnt(107)
      character*80 infile, outfile,atmp
      real*8 xyz(3,10000),xx(5)
      integer iat(10000),nat,nel,i,nn
      real*8 bohr
      logical da
      DATA ELEMNT/'h ','he',&
      'li','be','b ','c ','n ','o ','f ','ne',   &
      'na','mg','al','si','p ','s ','cl','ar',  &
      'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',  &
      'zn','ga','ge','as','se','br','kr',  &
      'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',  &
      'cd','in','sn','sb','te','i ','xe',  &
      'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb','dy',  &
      'ho','er','tm','yb','lu','hf','ta','w ','re','os','ir','pt',  &
      'au','hg','tl','pb','bi','po','at','rn',  &
      'fr','ra','ac','th','pa','u ','np','pu','am','cm','bk','cf','xx',  &
      'fm','md','cb','xx','xx','xx','xx','xx'/

      bohr=0.52917726
      nat=0
      inquire(file=infile,exist=da)

      if(da)then

      write(*,'(5x,''reading...'',$)')
! read XYZ file
      open(unit=3,file=infile)
       read(3,'(a)',end=100) atmp
! check for first two lines
       call readl(atmp,xx,nn)
        if(nn.gt.1) then   ! more than one argument found, assuming they are coords
           do i=1,10000   ! while loop would be better
            nat=nat+1
            read(3,'(a)',end=123) atmp
           enddo
          else
            nat=idint(xx(1))
           read(3,'(a)',end=100) atmp  !titel line
        endif
 123   if(nn.gt.1) rewind(3)
       do i=1,nat
            read(3,'(a)') atmp
            call readl(atmp,xx,nn)
            call elem(atmp,iat(i))
            xyz(1:3,i)=xx(1:3)
!       write(*,'(a2,5x,3F18.12)') elemnt(iat(i)),xyz(1:3,i)
       enddo
 100  close(3)
      write(*,'(5x,''XYZ file : '',a)')  trim(infile)
      else
      write(*,*) ' no input file found !! '
      endif

      write(*,*) '    number of atoms:  ',nat
      end

      subroutine wtm(xyz,iat,nat,outfile)
      implicit none
      character*2 elemnt(107)
      character*80 outfile,atmp
      real*8 xyz(3,10000),xx(5)
      integer iat(10000),nat,nel,i,nn,io
      real*8 bohr
      logical da
      DATA ELEMNT/'h ','he',   &
      'li','be','b ','c ','n ','o ','f ','ne',  &
      'na','mg','al','si','p ','s ','cl','ar',  &
      'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',  &
      'zn','ga','ge','as','se','br','kr',  &
      'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',  &
      'cd','in','sn','sb','te','i ','xe',  &
      'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb','dy',  &
      'ho','er','tm','yb','lu','hf','ta','w ','re','os','ir','pt',  &
      'au','hg','tl','pb','bi','po','at','rn',  &
      'fr','ra','ac','th','pa','u ','np','pu','am','cm','bk','cf','xx',  &
      'fm','md','cb','xx','xx','xx','xx','xx'/

        bohr=0.52917726
       open(unit=4,file=outfile)
!       write(*,*) 'writing coords'
       io=4
       write(io,'(a)')'$coord'
       do i=1,nat
       write(io,'(3F18.12,2x,a2)') xyz(1:3,i)/bohr , elemnt(iat(i))
       enddo
       write(io,'(a)')'$end'
       close(4)
       end

       subroutine string_to_integer(strg)
         use dyn_array
         implicit none
         character(len=*) :: strg
         
         character(len=:),allocatable :: value1,value2
         integer :: t, i,comma,skip,before,after,error,last,single
         integer, allocatable:: test(:)
         logical :: repetition

         do 
            comma = index(strg,",")
            if (comma .ne. 0) then
                 value1=strg(:comma-1)
                 skip = index(value1,"-")
                 if (skip .ne. 0) then
                        read(value1(:skip-1),*,IOSTAT=error) before
                        call check(error)
                        
                        read(value1(skip+1:),*,IOSTAT=error) after 
                        call check(error)

                        if (after<=before) then 
                           error = 1
                           call check(error)
                        else
                        
                           do i=before,after
                              if (allocated(idx)) call check_for_repeat(i,repetition)
                              if (.not.repetition) then
                                 call resize
                                 last=size(idx)
                                 idx(last)=i
                              endif
                           enddo

                        endif

                        strg=strg(comma+1:)
                  else
                     read(value1(:comma-1),*,IOSTAT=error) single
                     call check(error)
                     
                     if (allocated(idx)) call check_for_repeat(single,repetition)
                     if (.not.repetition) then

                        call resize
                        last=size(idx)
                        idx(last)=single
                    
                     
                     endif
                     
                     strg=strg(comma+1:)
                     
                  endif
            else
                 skip = index(strg,"-")
                 if (skip .ne. 0) then
                        read(strg(:skip-1),*,IOSTAT=error) before
                        call check(error)
                        
                        read(strg(skip+1:),*,IOSTAT=error) after 
                        call check(error)

                        if (after<=before) then 
                           error=1
                           call check(error)
                        else
                        
                           do i=before,after
                              if (allocated(idx)) call check_for_repeat(i,repetition)
                              if (.not.repetition) then

                                 call resize
                                 last=size(idx)
                                 idx(last)=i
                              end if
                           enddo

                        endif

                  else

                     read(strg,*,IOSTAT=error) single
                     call check(error)

                     if (allocated(idx)) call check_for_repeat(single,repetition)
                     if (.not.repetition) then
                        call resize
                        last=size(idx)
                        idx(last)=single
                     endif      
                  endif
                  strg=''
                  print *,idx
                  exit
            endif

        
      enddo
       end subroutine string_to_integer

      subroutine check(err)
      integer, intent(in) :: err
      if (err .ne. 0) then
         write(*,*) 'List specified not correctly, termination of cefine'
         STOP
      endif
      end subroutine check
      
       subroutine resize
         use dyn_array
         integer :: k
         integer, allocatable:: buffer(:)
         if (.not.(allocated(idx))) then
            allocate(idx(1),source=0)
         else
            allocate(buffer(size(idx)+1), source=0)
            buffer(:)=idx(:)
            deallocate(idx)
            call move_alloc(buffer, idx)
         endif

       end subroutine resize
      
      subroutine check_for_repeat(num,rep)
      use dyn_array
      implicit none
      integer, intent(in) :: num
      logical, intent(out) :: rep
      integer :: i,j
      do i=1, size(idx)
         if (num.eq.idx(i)) then
            rep=.true.
            exit
         endif
         rep=.false.
      enddo
      end subroutine
      
!       subroutine setL(L,arg)
!       logical L,arg
!       L=arg
!       print *,L
!       end subroutine 

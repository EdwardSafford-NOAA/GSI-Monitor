#/bin/bash
########################################################################
# 
# exnam_conmon.sh
#
# Run data extract/validation for regional conventional diag data
#
########################################################################

   echo start exnam_conmon.sh

   err=0
   export NCP=${NCP:-/bin/cp -f}
   export Z=gz

   #-------------------------
   # confirm $cnvstat exists
   if [[ ! -s ${cnvstat} ]]; then
      echo "unable to locate cnvstat: ${cnvstat}" 
      err=1
   fi

   #-----------------------
   #  Locate convinfo file
   export convinfo=${convinfo:-${HOMEnam_conmon}/fix/nam_regional_convinfo.txt}	
   if [[ ! -s ${convinfo} ]]; then
      echo "unable to locate convinfo: ${convinfo}" 
      err=2
   fi


   #####################################################################

   if [[ ${err} -eq 0 ]]; then
      export PDATE=${PDY}${CYC}
      echo PDATE = $PDATE

      if [[ ! -d ${TANKDIR_conmon} ]]; then 
         mkdir -p ${TANKDIR_conmon}
      fi


      #-------------------------------------------------------------
      #  Ensure necessary work and TANKDIR directories are in place
      #-------------------------------------------------------------
      mkdir -p ${TANKDIR_conmon}/horz_hist/anl
      mkdir -p ${TANKDIR_conmon}/horz_hist/ges
      mkdir -p ${TANKDIR_conmon}/time_vert

      #------------------------------------------------------------------
      #  Copy data files file to local data directory.  
      #  Untar cnvstat file.  
      #------------------------------------------------------------------
      ${NCP} ${cnvstat} ./cnvstat.${PDATE}

      tar -xvf ./cnvstat.${PDATE}
      rm cnvstat.${PDATE}
   
      netcdf=0
      count=`ls diag* | grep ".nc4" | wc -l`
      if [ $count -gt 0 ] ; then
         netcdf=1
         for filenc4 in `ls diag*nc4.gz`; do
            file=`echo $filenc4 | cut -d'.' -f1-2`.gz
            mv $filenc4 $file
         done
      fi

      echo "netcdf: ${netcdf}"
      export CONMON_NETCDF=${netcdf}
      ${UNCOMPRESS} *.gz

      #---------------------------------------
      #  run the time-vert extraction script
      #
      ${USHconmon}/time_vert.sh 
      rc_time_vert=$?
      echo "rc_time_vert = $rc_time_vert"

      #---------------------------------------
      #  run the horz-hist extraction script
      #
      ${USHconmon}/horz_hist.sh
      rc_horz_hist=$?
      echo "rc_horz_hist = $rc_horz_hist"

      #--------------------------------------
      #  optionally run clean_tankdir script
      #   
      if [[ ${CLEAN_TANKDIR} -eq 1 ]]; then
         ${USHconmon}/clean_tankdir_rgn.sh
         rc_clean_tankdir=$?
         echo "rc_clean_tankdir = $rc_clean_tankdir"
      fi
   fi

   if [[ $rc_horz_hist -ne 0 ]]; then
      echo "ERROR repored from horz_hist.sh:  $rc_horz_hist"
      err=$rc_horz_hist
   elif [[ $rc_time_vert -ne 0 ]]; then
      echo "ERROR repored from time_vert.sh:  $rc_time_vert"
      err=$rc_time_vert
   fi

   if [[ ${KEEPDATA} = "NO" ]]; then
      cd ${C_DATA}/..
      rm -rf ${C_DATA}
   fi

   echo "end exgdas_conmon.sh, exit value = ${err}"

exit ${err}


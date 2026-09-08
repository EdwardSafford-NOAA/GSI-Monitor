#!/bin/bash
set -xa

#------------------------------------------------------------
#
#  diag2grad_gps_case.sh
#
#------------------------------------------------------------

echo "--> diag2grad_gps_case.sh"

   echo "CONMON_SUFFIX   = $CONMON_SUFFIX"
   echo "TANKDIR_conmon  = $TANKDIR_conmon"
   echo "type          = $type"
   echo "PDATE         = $PDATE"
   echo "EXECconmon      = $EXECconmon"
   echo "cycle         = $cycle"
   echo "run           = $run  "
   echo "nreal         = $nreal"
   echo "mtype         = $mtype (type = $type)"
   echo "subtype       = $subtype"
   echo "hint          = $hint"
   echo "workdir       = $workdir"
   echo "INPUT_FILE    = ${INPUT_FILE}"

   nreal_gps=$nreal                 ### one less than the data items of diagnostic files
   nreal2_gps=`expr $nreal - 2`     ### the data items in the grads files 

   echo "CONMON_NETCDF = ${CONMON_NETCDF}"
   netcdf=".false."

   if [ $CONMON_NETCDF -eq 1 ]; then
      netcdf=".true."
   fi
   echo "netcdf = $netcdf"


   run_exe=1
   ctype=${mtype#gps}

   if [[ $mtype == gps* ]]; then
      rm -f diag2grads
      cp ${EXECconmon}/conmon_grads_lev.x ./diag2grads

      cat <<EOF >input
         &input
         input_file=${INPUT_FILE},
         intype='gps',stype='${mtype}',itype=$ctype,nreal=$nreal_gps,
         iscater=1,igrads=1,levcard='alllev',
         intv=$hint,subtype='${subtype}',isubtype=${subtype},
         netcdf=${netcdf},
         run=${run},
/
EOF
   else
      run_exe=0
   fi


   if [ $run_exe -eq 1 ]; then

      ./diag2grads <input>stdout 2>&1 


      rm -f *tmp
      mv stdout stdout_diag2grads_${mtype}_${subtype}.${run}

      dest_dir="${TANKDIR_conmon}/horz_hist/${run}"

      grads_list=`ls gps*grads.${run}`
      for file in $grads_list; do
         ${COMPRESS} ${file}
         cp -f ${file}.${Z} ${dest_dir}/${file}.${PDATE}.${Z}
      done

      scater_list=`ls gps*scater.${run}`
      for file in $scater_list; do
         ${COMPRESS} ${file}
         cp -f ${file}.${Z} ${dest_dir}/${file}.${PDATE}.${Z}
      done

   else
      echo "aborting run, unmatched mtype ${mtype}"
   fi

echo "<-- diag2grad_gps_case.sh"

exit

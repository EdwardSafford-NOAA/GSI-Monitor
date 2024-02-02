#!/bin/bash

#--------------------------------------------------------------------
#  RadMon_CP_glb.sh
#
#    This script searches for new radmon output from the global GDAS
#    and copies those filess to the user's $TANKDIR directory under 
#    the specified suffix argument. 
#
#    The bad_penalty, low count, and missing diag reports are 
#    reevaluated using local copies of the base file and satype
#    files in the $TANKdir/$suffix/info directory. 
#    
#    Note that processing occurs within TANKdir, not in stmp space.
#
#    The unified error report is journaled to warning.${PDY}${CYC}.
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  RadMon_CP_glb.sh suffix [-r|--run gdas|gfs -p|--pdate yyyymmddhh"
  echo ""
  echo "            Suffix (NET) is the indentifier for this data source."
  echo ""
  echo "            -r|--run is the run value, typically gdas or gfs.  Default value is gdas." 
  echo ""
  echo "            -p|--pdate is 10 digit yyyymmddhh string of cycle to be copied."
  echo "                       If not specified the pdate will be calculated by finding the latest"
  echo "                       cycle time in $TANKverf and incrementing it by 6 hours."
  echo ""
  echo "            -f|--radf parent directory to radstat file location.  This will be extended by "
  echo "                       $RUN.$PDY/$CYC/atmos/radmon and the files there copied to TANKverf."
  echo ""
  echo "            -d|--dataf parent directory to extracted radstat data file location.  This will be extended by "
  echo "                       $RUN.$PDY/$CYC and the files there copied to TANKverf."

}


echo start RadMon_CP_glb.sh
exit_value=0

nargs=$#
if [[ $nargs -le 0 || $nargs -gt 9 ]]; then
   usage
   exit 1
fi


export RAD_AREA=glb

#-----------------------------------------------------------
#  Set default values and process command line arguments.
#
run=gdas
pdate=""
data_file_loc=""

while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -p|--pdate)
         pdate="$2"
         shift # past argument
      ;;
      -r|--run)
         run="$2"
         shift # past argument
      ;;
      -f|--radf)
         radstat_loc="$2"
         shift # past argument
      ;;
      -d|--dataf)
         data_file_loc="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is RADMON_SUFFIX
         export RADMON_SUFFIX=$key
      ;;
   esac

   shift
done

echo "RADMON_SUFFIX    = $RADMON_SUFFIX"
echo "run              = $run"
echo "pdate            = $pdate"
echo "radstat_loc      = ${radstat_loc}"
echo "data_file_loc    = ${data_file_loc}"

export RUN=${RUN:-${run}}

set -ax

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
this_dir=`dirname $0`


top_parm=${this_dir}/../../parm

radmon_config=${radmon_config:-${top_parm}/RadMon_config}
if [[ ! -e ${radmon_config} ]]; then
   echo "Unable to source ${radmon_config} file"
   exit 2
fi

. ${radmon_config}
if [[ $? -ne 0 ]]; then
   echo "Error detected while sourcing ${radmon_config} file"
   exit $?
fi


radmon_user_settings=${radmon_user_settings:-${top_parm}/RadMon_user_settings}
if [[ ! -e ${radmon_user_settings} ]]; then
   echo "Unable to source ${radmon_user_settings} file"
   exit 3
fi

. ${radmon_user_settings}
if [[ $? -ne 0 ]]; then
   echo "Unable to source ${radmon_user_settings} file"
   exit $?
fi


#---------------------------------------------------------------
# Create any missing directories.
#---------------------------------------------------------------
if [[ ! -d ${TANKverf} ]]; then
   mkdir -p $TANKverf
fi
if [[ ! -d ${R_LOGDIR} ]]; then
   mkdir -p $R_LOGDIR
fi

#---------------------------------------------------------------
# If the pdate (processing date) was not specified at the 
# command line then set it by finding the latest cycle in
# $TANKverf and increment 6 hours.
#---------------------------------------------------------------
if [[ $pdate = "" ]]; then
   ldate=`${MON_USH}/find_last_cycle.sh --net ${RADMON_SUFFIX} \
	                               --run ${RUN} --mon radmon --tank ${TANKDIR}`
   pdate=`${NDATE} +06 ${ldate}`
fi
echo "pdate = $pdate"
export PDATE=${pdate}

export PDY=`echo $PDATE|cut -c1-8`
export CYC=`echo $PDATE|cut -c9-10`

#---------------------------------------------------------------
#  Set data and radstat locations     
#---------------------------------------------------------------
if [[ -n ${radstat_loc} ]]; then 
   RADSTAT_LOCATION=${radstat_loc}
fi
export RADSTAT_LOCATION=${RADSTAT_LOCATION}/${RUN}.${PDY}/${CYC}/atmos


if [[ -n ${data_file_loc} ]]; then
   export DATA_LOCATION=${data_file_loc}/${RUN}.${PDY}
else  
   export DATA_LOCATION=${RADSTAT_LOCATION}/radmon
fi


if [[  -d ${DATA_LOCATION} ]]; then
   job=${DE_SCRIPTS}/radmon_copy.sh
   jobname=RadMon_CP_${RADMON_SUFFIX}
   logfile=${R_LOGDIR}/CP.${PDY}.${CYC}.log
   if [[ -e ${logfile} ]]; then
     rm -f ${logfile}
   fi


   if [[ $MY_MACHINE = "hera" ]]; then
      $SUB --account=${ACCOUNT} --time=10 -J ${jobname} -D . \
        -o ${logfile} --ntasks=1 --mem=5g ${job}

   elif [[ $MY_MACHINE = "wcoss2" ]]; then
      #---------------------------------------------------------------------
      # The qsub manual says the -v option (used to export variables to the
      # submitted job) can be done in two ways:
      #
      #    Method 1:   -v VAR1=value1,VAR2=value2,... 
      #    Method 2:   -v VAR1=value1 -v VAR2=value2 ....
      #
      # Method 1 is a comma separated list with NO spaces.
      # Method 2 does _NOT_ work on wcoss2 for whatever reason.  
      #---------------------------------------------------------------------
      $SUB -q $JOB_QUEUE -A $ACCOUNT -o ${logfile} -e ${R_LOGDIR}/CP.${PDY}.${CYC}.err \
	   -v RADMON_SUFFIX=${RADMON_SUFFIX},RUN=${RUN},PDY=${PDY},CYC=${CYC},HOMEradmon=${HOMEradmon},RADSTAT_LOCATION=${RADSTAT_LOCATION},DATA_LOCATION=${DATA_LOCATION},TANKverf=${TANKverf},FIXgdas=${FIXgdas},DO_DATA_RPT=${DO_DATA_RPT},DE_EXEC=${DE_EXEC},DE_SCRIPTS=${DE_SCRIPTS},NCP="${NCP}",CLEAN_TANKVERF=${CLEAN_TANKVERF},RAD_AREA=${RAD_AREA} \
	   -l place=shared,select=1:ncpus=1:mem=5000M -l walltime=00:20:00 -N ${jobname} ${job}
   fi
else
   echo "Unable to locate DATA_LOCATION: ${DATA_LOCATION}"
   exit_value=4
fi


echo end RadMon_CP_glb.sh
exit ${exit_value}


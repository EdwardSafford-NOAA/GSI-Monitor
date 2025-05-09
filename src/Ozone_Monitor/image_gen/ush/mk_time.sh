#!/bin/bash

#------------------------------------------------------------------
#  mk_time.sh
#
#   - set up working directory
#   - build the cmdfile script
#   - submit plot job
#------------------------------------------------------------------

echo "begin mk_time.sh"


data_source="ges anl"

#------------------------------------------------------------------
# Define working directory for time plots
#
tmpdir=${WORKDIR}/time
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export WORKDIR=$tmpdir

#------------------------------------------------------------------
#  Expand $OZN_IMGN_TANKDIR for time
#
export OZN_IMGS_TIME=${OZN_TANKDIR_IMGS}/time
if [[ ! -d ${OZN_IMGS_TIME} ]]; then
   mkdir -p ${OZN_IMGS_TIME}
fi

#------------------------------------------------------------------
# Loop over sat types & create entry in cmdfile for each.
#

for dsrc in ${data_source}; do
   suffix=a
   if [[ $dsrc = "ges" ]]; then
      list="cnt omg cpen"
   else
      list="cnt oma cpen"
   fi

   cmdfile=cmdfile_${dsrc}_ptime
   rm -f $cmdfile
   ctr=0

>$cmdfile
   for type in ${SATYPE}; do
      if [[ ${MY_MACHINE} = "hera" || ${MY_MACHINE} = "jet" || 
            ${MY_MACHINE} = "s4"   || ${MY_MACHINE} = "orion" ||
            ${MY_MACHINE} = "hercules" ]]; then
         echo "${ctr} ${OZN_IG_SCRIPTS}/plot_time.sh $type $suffix '$list' $dsrc" >> $cmdfile
         ((ctr=ctr+1))
      else
         echo "${OZN_IG_SCRIPTS}/plot_time.sh $type $suffix '$list' $dsrc" >> $cmdfile
      fi
   done
   chmod a+x $cmdfile

   job=${OZNMON_SUFFIX}_ozn_${dsrc}_ptime

   logf=${OZN_LOGDIR}/IG.${PDY}.${CYC}.${dsrc}.time.log
   if [[ -e $logf ]]; then
      rm -f $logf
   fi

   errf=${OZN_LOGDIR}/IG.${PDY}.${CYC}.${dsrc}.time.err
   if [[ -e $errf ]]; then
      rm -f $errf
   fi

   if [[ ${MY_MACHINE} = "hera" || ${MY_MACHINE} = "s4" ||
         ${MY_MACHINE} = "orion" || ${MY_MACHINE} = "hercules" ]]; then

      $SUB --account ${ACCOUNT} -n $ctr  -o ${logf} -D . -J ${job} --time=10 \
           --wrap "srun -l --multi-prog ${cmdfile}"

   elif [[ ${MY_MACHINE} = "jet" ]]; then

      $SUB --account ${ACCOUNT} -n $ctr  -o ${logf} -D . -J ${job} --time=10 \
           -p ${PARTITION_OZNMON} --wrap "srun -l --multi-prog ${cmdfile}"

   elif [[ $MY_MACHINE = "wcoss2" ]]; then

      $SUB -q $JOB_QUEUE -A $ACCOUNT -o ${logf} -e ${errf} \
           -v "WORKDIR=${WORKDIR}, PDATE=${PDATE}, MON_USH=${MON_USH}, OZNMON_SUFFIX=${OZNMON_SUFFIX}, \
	      NDATE=${NDATE}, UNCOMPRESS=${UNCOMPRESS}, COMPRESS=${COMPRESS}, NUM_CYCLES=${NUM_CYCLES}, \
	      OZN_IG_SCRIPTS=${OZN_IG_SCRIPTS}, GRADS=${GRADS}, NCP=${NCP}, KEEPDATA=${KEEPDATA}, RUN=${RUN}, \
	      OZN_TANKDIR=${OZN_TANKDIR}, CYCLE_INTERVAL=${CYCLE_INTERVAL}, OZN_IG_GSCRPTS=${OZN_IG_GSCRPTS}, \
	      PATH=${PATH}, GADDIR=${GADDIR}, OZN_IMGS_TIME=${OZN_IMGS_TIME}" \
	   -l select=1:mem=500M -l walltime=10:00 -N ${job} ${cmdfile}
   fi


done

echo "end mk_time.sh"
exit

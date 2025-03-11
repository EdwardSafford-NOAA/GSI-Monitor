#!/bin/bash

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#  Install_html.sh
#
#  Given a suffix and a global/regional flag as inputs, build the
#  html necessary for a radiance monitor web site and tranfer it to
#  the server.
#--------------------------------------------------------------------
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  Install_html.sh suffix -t|--tank [-r|--run -a|--area]"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            -t | --tank  parent directory to the radmon data file location.  This"
  echo "              will be extended by /$RADMON_SUFFIX, /$RUN, and /$PDATE to locate the"
  echo "              extracted radmon data.  Note if using internal RadMon format for data"
  echo "              then stop at '/nbns' and do not include '/stats' in tank."
  echo "            -r | --run  Run value for data source.  Default is 'gdas'."
  echo "            -a | --area  Area value for data source.  Valid values are 'glb' or 'rgn'"
  echo "              indicating global or regional data source.  The default is 'glb'."
  echo ""
}

echo "BEGIN Install_html.sh"
echo ""

nargs=$#

if [[ $nargs -lt 1 || $nargs -gt 7 ]]; then
   usage
   exit 2
fi

#-----------------------------------------------------------
#  Set default values and process command line arguments.
#
run=gdas
tank=""
area=glb

while [[ $# -ge 1 ]]; do
   key="$1"

   case $key in
      -t|--tank)
         tank="$2"
	 shift # past argument
         ;;
      -r|--run)
         run="$2"
	 shift # past argument
         ;;
      -a|--area)
         area="$2"
	 shift # past argument
         ;;
      *)
         #any unspecified key is RADMON_SUFFIX
	 export RADMON_SUFFIX=$key
	 ;;
   esac
   shift
done

this_file=`basename $0`
this_dir=`dirname $0`

export RAD_AREA=${area}
top_parm=${this_dir}/../../parm

radmon_config=${radmon_config:-${top_parm}/RadMon_config}
if [[ ! -e ${radmon_config} ]]; then
   echo "Unable to source ${radmon_config}"
   exit 2
fi

. ${radmon_config}
if [[ $? -ne 0 ]]; then
   echo "Error detected while sourcing ${radmon_config} file"
   exit $?
fi


radmon_user_settings=${radmon_user_settings:-${top_parm}/RadMon_user_settings}
if [[ ! -e ${radmon_user_settings} ]]; then
   echo "Unable to locate ${radmon_user_settings} file"
   exit 4
fi

. ${radmon_user_settings}
if [[ $? -ne 0 ]]; then
   echo "Error detected while sourcing ${radmon_user_settings} file"
   exit $?
fi


if [[ ${#tank} -le 0 ]]; then
   tank=${TANKDIR}
fi

export R_TANKDIR=${tank}
export RUN=${run}

if [[ ${RAD_AREA} == "glb" ]]; then
   ${RADMON_IMAGE_GEN}/html/install_glb.sh 

elif [[ ${RAD_AREA} == "rgn" ]]; then
   ${RADMON_IMAGE_GEN}/html/install_rgn.sh 

else
   echo "area value ${RAD_AREA} is not recognized.  Only valid values are 'glb' and 'rgn'."
fi

echo
echo "END Install_html.sh"

exit

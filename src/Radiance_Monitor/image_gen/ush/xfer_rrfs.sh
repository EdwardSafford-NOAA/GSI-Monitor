#!/bin/bash

from=~/nbns/imgn/regional/rrfs/radmon/pngs
to=/home/people/emc/www/htdocs/gmb/gdas/radiance/esafford/regional/rrfs/pngs

echo rsync $from to $to

/usr/bin/rsync -ave ssh --exclude *.ctl* \
    --exclude 'horiz' --delete-during  $from \
    esafford@emcrzdm.ncep.noaa.gov:$to

dset 
options big_endian sequential template
undef -999.
title  conventional stats  jiter
*XDEF is data type
*YDEF is region
*ZDEF is vertical level
* gps    x=    1 dtype=      4 subtype=      0 iuse=  1
* gps    x=    2 dtype=     41 subtype=      0 iuse= -1
* gps    x=    3 dtype=    722 subtype=      0 iuse=  1
* gps    x=    4 dtype=    723 subtype=      0 iuse=  1
* gps    x=    5 dtype=    740 subtype=      0 iuse=  1
* gps    x=    6 dtype=    741 subtype=      0 iuse=  1
* gps    x=    7 dtype=    742 subtype=      0 iuse=  1
* gps    x=    8 dtype=    743 subtype=      0 iuse=  1
* gps    x=    9 dtype=    744 subtype=      0 iuse=  1
* gps    x=   10 dtype=    745 subtype=      0 iuse=  1
* gps    x=   11 dtype=    820 subtype=      0 iuse=  1
* gps    x=   12 dtype=     42 subtype=      0 iuse=  1
* gps    x=   13 dtype=     43 subtype=      0 iuse=  1
* gps    x=   14 dtype=    786 subtype=      0 iuse=  1
* gps    x=   15 dtype=    421 subtype=      0 iuse= -1
* gps    x=   16 dtype=      3 subtype=      0 iuse=  1
* gps    x=   17 dtype=    821 subtype=      0 iuse= -1
* gps    x=   18 dtype=    825 subtype=      0 iuse=  1
* gps    x=   19 dtype=    440 subtype=      0 iuse= -1
* gps    x=   20 dtype=    750 subtype=      0 iuse=  1
* gps    x=   21 dtype=    751 subtype=      0 iuse=  1
* gps    x=   22 dtype=    752 subtype=      0 iuse=  1
* gps    x=   23 dtype=    753 subtype=      0 iuse=  1
* gps    x=   24 dtype=    754 subtype=      0 iuse=  1
* gps    x=   25 dtype=    755 subtype=      0 iuse=  1
* gps    x=   26 dtype=    724 subtype=      0 iuse= -1
* gps    x=   27 dtype=    725 subtype=      0 iuse= -1
* gps    x=   28 dtype=    726 subtype=      0 iuse= -1
* gps    x=   29 dtype=    727 subtype=      0 iuse= -1
* gps    x=   30 dtype=    728 subtype=      0 iuse= -1
* gps    x=   31 dtype=    729 subtype=      0 iuse= -1
* gps    x=   32 dtype=     44 subtype=      0 iuse=  1
* gps    x=   33 dtype=      5 subtype=      0 iuse=  1
* gps    x=   34 dtype=     66 subtype=      0 iuse=  1
* gps    x=   35 dtype=    265 subtype=      0 iuse=  1
* gps    x=   36 dtype=    266 subtype=      0 iuse= -1
* gps    x=   37 dtype=    267 subtype=      0 iuse=  1
* gps    x=   38 dtype=    268 subtype=      0 iuse= -1
* gps    x=   39 dtype=    269 subtype=      0 iuse=  1
* gps    x=   40 dtype=    803 subtype=      0 iuse=  1
* gps    x=   41 dtype=    804 subtype=      0 iuse=  1
* gps    x=   42 dtype=    768 subtype=      0 iuse=  1
* gps    x=   43 dtype=    all    subtype=   0   iuse=   1 
xdef  43 linear 1.0 1.0
ydef  10 linear 1.0 1.0
*  region=  1 GL (180W-180E, 90S-90N)                                           
*  region=  2 NH (180W-180E, 20N-90N)                                           
*  region=  3 CENT (115W- 83W, 25N-50N)                                         
*  region=  4 TR (180W-180E, 20S-20N)                                           
*  region=  5 USA (125W- 65W, 25N-50N)                                          
*  region=  6 EAST ( 93W- 65W, 32N-50N)                                         
*  region=  7 N&CA (165W- 60W,  0S-90N)                                         
*  region=  8 S&CA (165W- 30W, 90S- 0S)                                         
*  region=  9 EU ( 10W- 25E, 35N-70N)                                           
*  region= 10 AS ( 65E-145E,  5N-45N)                                           
zdef  13 linear 1.0 1.0
tdef 1 linear 00z14dec2001 1hr 
*  z=    1, level=    0-2000
*  z=    2, level=    1000-2000
*  z=    3, level=    950-1000
*  z=    4, level=    900-950
*  z=    5, level=    850-900
*  z=    6, level=    800-850
*  z=    7, level=    750-800
*  z=    8, level=    700-750
*  z=    9, level=    600-700
*  z=   10, level=    500-600
*  z=   11, level=    400-500
*  z=   12, level=    300-400
*  z=   13, level=    0-300
vars      18
count1      13  0 assimilated obs no. ,0: all
count_vqc1      13  0  obs no. rejected by vqc for assimilated data, 0: all
bias1       13  0  bias (obs-ges) for assimilated data
rms1        13  0  rms  for assimilated data
rat1        13  0  penalty for assimilated data
qcrat1      13  0  qc penalty for assimilated data
count2      13  0  rejected obs no,0: all
count_vqc2      13  0  obs no. rejected by vqc for rejected data,0: all
bias2       13  0  bias(obs-ges) for rejected data
rms2        13  0  rms  for rejected data
rat2        13  0  penalty for rejected data
qcrat2      13  0  qc penalty for rejected data
count3      13  0   obs no. for monitored data,0: all
count_vqc3      13  0 obs no. rejected by vqc for monitored data,0: all
bias3       13  0  bias(obs-ges) for monitored data
rms3        13  0  rms for monitored data
rat3        13  0  penalty for monitored data
qcrat3      13  0  qc penalty for monitored data
endvars

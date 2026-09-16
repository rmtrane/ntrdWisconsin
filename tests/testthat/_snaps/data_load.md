# pull_redcap_data works for UDS-4

    Code
      scramble_uds4
    Output
          VISITYR VISITMO VISITDAY   SEX   NACCID  EDUC  RACE IQCODEINFORM IQCODESELF
            <num>   <num>    <int> <num>   <char> <num> <num>        <num>      <num>
       1:    2025       9       11     2 sim00001    14    NA       2.4375     3.0625
       2:    2025       6       30     2 sim00002    16     1       3.0000     3.0000
       3:    2025       8       18     1 sim00003    13     1       3.1250     3.2500
       4:    2025       7        9     2 sim00004    18     2       3.1875     3.0625
       5:    2025      12        2     1 sim00005    16     2       3.4375         NA
       6:    2025       7        9     2 sim00006    14     1       2.5625     3.0625
       7:    2025       8       21    NA sim00007    14     1       3.0000     3.2500
       8:    2025      12       11     2 sim00008    20     1       3.0625     3.8750
       9:    2025       9       26     2 sim00009    18     1       3.0000     3.0000
      10:    2025       8       22     2 sim00010    16     2       3.0000         NA
      11:    2025       9        4    NA sim00011    18    NA       3.0000     2.4375
      12:    2025      10       31     2 sim00012    NA     1       3.0000         NA
      13:    2025      12       30     1 sim00013    NA     1       3.0000         NA
      14:    2025       7       21     2 sim00014    20     1       3.0000     3.2500
      15:    2025       9       18     1 sim00015    26    NA       3.0000     3.2500
      16:    2025       6       30     1 sim00016    16     1       3.0000     3.2500
      17:    2025      10       24    NA sim00017    13     1       3.0000     3.0000
      18:    2025       9       12     1 sim00018    16     1       3.0000     2.9375
      19:    2025      12       17     1 sim00019    15     1       2.9375         NA
      20:    2025      10       21     2 sim00020    18     1           NA         NA
          VISITYR VISITMO VISITDAY   SEX   NACCID  EDUC  RACE IQCODEINFORM IQCODESELF
            <num>   <num>    <int> <num>   <char> <num> <num>        <num>      <num>
          HANDED CDRGLOB MOCATOTS MOCBTOTS TRAILA TRAILARR TRAILALI OTRAILA OTRLARR
           <num>   <num>    <num>    <num>  <num>    <num>    <num>   <num>   <num>
       1:      2     0.5       26       NA     20       NA       24      NA      NA
       2:      2     1.0       20       NA     19        0       24      NA      NA
       3:      2     0.5       NA       NA     21        0       24      NA      NA
       4:      1      NA       NA       NA     21        0       NA      NA      NA
       5:      2     0.0       30       NA     17       NA       24      NA      NA
       6:      2     2.0       19       NA     NA        0       NA      NA      NA
       7:      2     0.0       29       NA     25        0       24      NA      NA
       8:      2     0.0       29       NA     22        0       24     997      NA
       9:      2     0.5       24       NA     15       NA       24      NA      NA
      10:      2     0.5       28       NA     NA       NA       NA      NA      NA
      11:      1     0.0       22       NA     NA        0       24     997      NA
      12:     NA     0.0       16       NA     21        0       24     888      NA
      13:      1     3.0       29       NA     44       NA       24      NA      NA
      14:      2      NA       29       NA     30       NA       NA      NA      NA
      15:      2     0.0       29       NA     19       NA       24      NA      NA
      16:      2     0.5       29       NA     26        0       24      NA      NA
      17:      2     0.0       16       NA     NA        0       NA      NA      NA
      18:      2      NA       26       NA     20        0       NA      NA      NA
      19:      1     0.0       NA       NA     33        0       NA      NA      NA
      20:      2     0.5       NA       NA     NA        0       NA      NA      NA
          HANDED CDRGLOB MOCATOTS MOCBTOTS TRAILA TRAILARR TRAILALI OTRAILA OTRLARR
           <num>   <num>    <num>    <num>  <num>    <num>    <num>   <num>   <num>
          DIGFORCT DIGFORSL DIGBACCT DIGBACLS  WAIS MINTTOTS ANIMALS   VEG UDSVERTN
             <num>    <num>    <num>    <num> <num>    <num>   <num> <num>    <num>
       1:        7        7       97        4    NA       32      18    14       NA
       2:       97       NA        6       NA    45       20      14    16       38
       3:        5       NA       NA       NA    NA       32      25    14       35
       4:        7        9       NA       NA    62       32      23    14       29
       5:       97        7        9        7    48       32      97    12       NA
       6:        5        6        6       NA    22       29      20    11       35
       7:        8        9       98        6    NA       32      NA    15       33
       8:       NA        7       NA        5    55       32      14    11       50
       9:       97        7       10        4    32       32      28    17       37
      10:        6       NA       97        5    56       32      24    21       32
      11:       13        7        6        6    NA       32      30     9       28
      12:       10        7        2        6    52       32      96    97       19
      13:       97       NA        7        2    55       32      97    15       NA
      14:        8        7       NA        6    NA       32      27    14       30
      15:       NA        5        7        5    30       NA      19     6       19
      16:       10        8       97       NA    NA       32      23     9       27
      17:        6        8        6       NA    63       28      NA    97       NA
      18:       97        6        9        6    52       29      16     7       27
      19:        7       NA        6        4    NA       28      19     7       41
      20:        9        7        8       NA    62       32       6    15       24
          DIGFORCT DIGFORSL DIGBACCT DIGBACLS  WAIS MINTTOTS ANIMALS   VEG UDSVERTN
             <num>    <num>    <num>    <num> <num>    <num>   <num> <num>    <num>
          UDSVERFC UDSVERLC UDSBENTC UDSBENTD CRAFTVRS CRAFTURS CRAFTDVR CRAFTDRE
             <num>    <num>    <num>    <num>    <num>    <num>    <num>    <num>
       1:       24       16       16       12       30        6       NA       14
       2:       NA       17       NA       14       36        6       20       17
       3:       97       97       NA       11       NA       NA       16       14
       4:       14       10       16       NA       27       NA       24       24
       5:       25       20       14        8        9       18       96       15
       6:       14       25       NA       NA       34       19       27       12
       7:       NA       14       15       NA       21       15       19       16
       8:       21       12       17       14       11       20       21       18
       9:       14       15       16       12       12       15        9       NA
      10:       10       97       16       NA       20       16       20       NA
      11:       20       27       16        3       18       17       21       13
      12:       15       16       16       NA       34       22       NA        8
      13:       13       17       NA       14       21       21       27       23
      14:       25       20       NA       14        7       17       NA       18
      15:       96       13       16       NA       17       16       19        9
      16:       23       14       16        0       33       NA       15        7
      17:       96       97       16       NA       21       16       13       18
      18:       14       20       16       15       29       19        1       NA
      19:       NA       NA       16       13       18       NA       22       18
      20:       23       NA       NA       NA       NA       22       35       NA
          UDSVERFC UDSVERLC UDSBENTC UDSBENTD CRAFTVRS CRAFTURS CRAFTDVR CRAFTDRE
             <num>    <num>    <num>    <num>    <num>    <num>    <num>    <num>
          REY1REC REY2REC REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC REYTCOR
            <num>   <num>   <num>   <num>   <num>    <num>   <num>   <num>   <num>
       1:      97       8      NA      NA      13       98      15       5      15
       2:      NA       8      12      14      15       NA      11      14      NA
       3:       4       9      NA      14      15        5       8      15      NA
       4:      97       7      15      NA       6        7      14       7      NA
       5:       5      NA      NA       9      NA       96      12      10      NA
       6:      NA      10       8      NA      14       NA      12      14      14
       7:      11       9      15      NA      NA       96      NA       5      15
       8:       4       6      15       0      13       NA      12      11      12
       9:       4      NA      13      13      NA        6      NA      14      NA
      10:      98       9       9       6       6        5       6      10      NA
      11:       5       8      NA      14      NA        4       0      NA      12
      12:       9      NA      14      12       9        5      NA       5      NA
      13:       5       8      13      98      15       96      NA      15      10
      14:       7       8       9      13      14        4      NA      12      15
      15:      97       5      10      11      14        3      13      10      14
      16:       6      10      NA       6      15       96      14       7      13
      17:       9      NA       4      NA       6        3      10      NA      14
      18:       7       7      NA      NA      15        4       8      NA      13
      19:       9      NA      10      11      10       NA      NA       6      97
      20:       6      14      10       7       7        6       5      13      10
          REY1REC REY2REC REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC REYTCOR
            <num>   <num>   <num>   <num>   <num>    <num>   <num>   <num>   <num>
          REYFPOS TRAILB TRAILBRR TRAILBLI MOCACLOC MOCACLOH MOCACLON OTRAILB OTRLBRR
            <num>  <num>    <num>    <num>    <num>    <num>    <num>   <num>   <num>
       1:      NA     NA        2       24        1        1        1     997      NA
       2:       2     NA        1       24        1       NA       NA     997      NA
       3:       0     70        2       24       NA        1        1     888      NA
       4:       0     NA       NA       24        1       NA        1      NA      NA
       5:       0    115        6       24       NA       NA        1      NA      NA
       6:      NA     79        0       24       NA        0        1      NA      NA
       7:       0     NA        0       24       NA       NA       NA      NA      NA
       8:       1    300       NA       24       NA        1       NA      NA      NA
       9:       0    204       NA       24        1        1       NA      NA      NA
      10:      NA     48        0       24        1        1        0      NA      NA
      11:      NA     60       NA       NA        1        1        1      NA      NA
      12:       0     NA       NA       23        1        1        1     996      NA
      13:      NA     NA       NA       24        1        1       NA      NA      NA
      14:       1     43        0       24        1       NA        1      NA      NA
      15:      NA    997        0       24       NA        1       NA      NA      NA
      16:       2     73        0       NA       NA        1        1     888      NA
      17:       2    121        0       24       NA        1       NA      NA      NA
      18:       2     61        0       24        1       NA        1     888      NA
      19:       0     33        0       NA        1       NA        1      NA      NA
      20:       1     70       NA       24       NA        1       NA     997      NA
          REYFPOS TRAILB TRAILBRR TRAILBLI MOCACLOC MOCACLOH MOCACLON OTRAILB OTRLBRR
            <num>  <num>    <num>    <num>    <num>    <num>    <num>   <num>   <num>
          OTRLBLI NACCGDS CDRSUM UDSBENRS NACCUDSD BILLS TAXES SHOPPING GAMES STOVE
            <num>   <num>  <num>    <num>    <num> <num> <num>    <num> <num> <num>
       1:      NA       4    0.0        1        1    NA     0        0     3     2
       2:      NA       3    0.0        1        1     0     0        3     0     0
       3:      NA      NA     NA        1        1     0     0        0     1     1
       4:      NA       1    1.5        1        2     0     0        0     0     3
       5:      NA       0    0.0        1        1     3     0        1     1     2
       6:      NA      NA    5.5       NA        1     8     0        0     3     0
       7:      NA       2    1.0        1        1     0     0        3     0     2
       8:      NA       0   12.0        1        1     8     0        1     0     0
       9:      NA       0    0.0        1        3     0     0        0     0     2
      10:      NA       2    1.5       NA       NA     0     8        0     0     1
      11:      NA       0    0.5       NA        1     0     8        0     0     1
      12:      NA       0    1.0        1        4     8     0        0    NA     0
      13:      NA       3   18.0        0        1     1     0        0     0    NA
      14:      NA      NA    0.0        1        1     0     3        1     0     0
      15:      NA       3    1.0       NA        1     0     0        0     0    NA
      16:      NA      NA   18.0       NA        4     0    NA        0     0     0
      17:      NA       2    0.0        1        1     1     0        0     0    NA
      18:      NA       0    0.0       NA        1     0     0        0     0    NA
      19:      NA      NA    2.0       NA        1    NA     3        0     0     0
      20:      NA       0    0.0       NA        1     0     1        0     0     0
          OTRLBLI NACCGDS CDRSUM UDSBENRS NACCUDSD BILLS TAXES SHOPPING GAMES STOVE
            <num>   <num>  <num>    <num>    <num> <num> <num>    <num> <num> <num>
          MEALPREP EVENTS PAYATTN REMDATES TRAVEL BIRTHYR BIRTHMO ALCDEM ANXIET
             <num>  <num>   <num>    <num>  <num>   <num>   <num>  <num>  <num>
       1:        0      0       0        0      0    1951       9     NA     NA
       2:        0      0       0        0     NA    1951       8     NA      1
       3:        0      0       0        0      0      NA       9     NA     NA
       4:        0     NA       0        0      0    1947      NA     NA     NA
       5:        0      0       0        0      0    1956      12     NA     NA
       6:        0      0       0        3      0    1946       5     NA     NA
       7:        3     NA       1        0      0    1947      NA     NA      1
       8:        0      0       0        1      0    1942      10     NA     NA
       9:        0      0       0        1      0    1937       5     NA     NA
      10:        1      0       0        0      3      NA       2     NA      1
      11:       NA      3       1       NA      0    1965       3     NA     NA
      12:        0      3      NA        0      0    1955      10     NA     NA
      13:        0      3      NA        0      1    1953       9     NA      1
      14:        0      0      NA        0      0    1952       1     NA     NA
      15:        0      0       0       NA      0    1962      10     NA     NA
      16:        0      0       0        0      0    1950      11     NA     NA
      17:       NA      0       0       NA      0    1965       2     NA     NA
      18:        0      0       9        1      0    1958       5     NA     NA
      19:        8      1       0       NA      0    1938       4     NA     NA
      20:        0      0       0        2      3    1967       8     NA     NA
          MEALPREP EVENTS PAYATTN REMDATES TRAVEL BIRTHYR BIRTHMO ALCDEM ANXIET
             <num>  <num>   <num>    <num>  <num>   <num>   <num>  <num>  <num>
          BIPOLDX BRNINJ COGOTH COGOTH2 COGOTH3  CORT   CVD DELIR DOWNS EPILEP FTLDMO
            <num>  <num>  <num>   <num>   <num> <num> <num> <num> <num>  <num>  <num>
       1:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       2:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       3:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       4:      NA      1     NA      NA      NA    NA    NA    NA    NA     NA     NA
       5:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       6:      NA     NA     NA      NA      NA    NA     1    NA    NA     NA     NA
       7:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       8:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       9:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      10:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      11:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      12:      NA     NA     NA      NA      NA    NA     1    NA    NA     NA     NA
      13:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      14:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      15:      NA     NA     NA      NA      NA    NA     1    NA    NA     NA     NA
      16:      NA     NA      1      NA      NA    NA    NA    NA    NA     NA     NA
      17:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      18:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      19:      NA      1     NA      NA      NA    NA    NA    NA    NA     NA     NA
      20:      NA      1     NA      NA      NA    NA    NA    NA    NA     NA     NA
          BIPOLDX BRNINJ COGOTH COGOTH2 COGOTH3  CORT   CVD DELIR DOWNS EPILEP FTLDMO
            <num>  <num>  <num>   <num>   <num> <num> <num> <num> <num>  <num>  <num>
          FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS   MSA NACCALZD NACCLBDE  NEOP
            <num> <num> <num>  <num>  <num> <num> <num>    <num>    <num> <num>
       1:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       2:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       3:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       4:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       5:      NA    NA    NA     NA     NA    NA    NA        1       NA    NA
       6:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       7:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       8:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       9:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      10:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      11:      NA    NA    NA     NA     NA    NA    NA        1       NA    NA
      12:      NA    NA    NA     NA     NA    NA    NA        1       NA    NA
      13:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      14:      NA    NA    NA     NA     NA    NA    NA        1       NA    NA
      15:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      16:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      17:      NA    NA    NA     NA     NA    NA    NA        1       NA    NA
      18:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      19:      NA    NA    NA     NA     NA    NA    NA        1       NA    NA
      20:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
          FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS   MSA NACCALZD NACCLBDE  NEOP
            <num> <num> <num>  <num>  <num> <num> <num>    <num>    <num> <num>
          OTHCOG OTHPSY PPAPH PRION   PSP PTSDDX SCHIZOP STROKE ALCDEMIF ANXIETIF
           <num>  <num> <num> <num> <num>  <num>   <num>  <num>    <num>    <num>
       1:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
       2:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
       3:     NA     NA    NA    NA    NA     NA      NA      0       NA        2
       4:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
       5:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
       6:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
       7:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
       8:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
       9:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
      10:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
      11:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
      12:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
      13:     NA     NA    NA    NA    NA     NA      NA      0       NA        3
      14:     NA     NA    NA    NA    NA     NA      NA      2       NA       NA
      15:     NA     NA    NA    NA    NA     NA      NA      2       NA       NA
      16:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
      17:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
      18:     NA     NA    NA    NA    NA     NA      NA     NA       NA        3
      19:     NA     NA    NA    NA    NA      1      NA      0       NA       NA
      20:     NA     NA    NA    NA    NA     NA      NA      2       NA       NA
          OTHCOG OTHPSY PPAPH PRION   PSP PTSDDX SCHIZOP STROKE ALCDEMIF ANXIETIF
           <num>  <num> <num> <num> <num>  <num>   <num>  <num>    <num>    <num>
          BIPOLDIF COGOTHIF COGOTH2F COGOTH3F CORTIF CVDIF DELIRIF DOWNSIF EPILEPIF
             <num>    <num>    <num>    <num>  <num> <num>   <num>   <num>    <num>
       1:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       2:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       3:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       4:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       5:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       6:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       7:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       8:       NA        1       NA       NA     NA    NA      NA      NA       NA
       9:       NA       NA       NA       NA     NA     3      NA      NA       NA
      10:       NA       NA       NA       NA     NA     2      NA      NA       NA
      11:       NA        1       NA       NA     NA    NA      NA      NA       NA
      12:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      13:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      14:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      15:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      16:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      17:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      18:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      19:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      20:       NA       NA       NA       NA     NA    NA      NA      NA       NA
          BIPOLDIF COGOTHIF COGOTH2F COGOTH3F CORTIF CVDIF DELIRIF DOWNSIF EPILEPIF
             <num>    <num>    <num>    <num>  <num> <num>   <num>   <num>    <num>
          FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF IMPSUBIF MEDSIF MSAIF NACCALZP
             <num>    <num> <num>  <num>    <num>    <num>  <num> <num>    <num>
       1:       NA       NA    NA     NA       NA       NA     NA    NA        1
       2:       NA       NA    NA     NA       NA       NA     NA    NA        1
       3:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       4:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       5:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       6:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       7:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       8:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       9:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      10:       NA       NA    NA     NA       NA       NA     NA    NA        1
      11:       NA       NA    NA     NA       NA       NA     NA    NA        3
      12:       NA       NA    NA     NA       NA       NA     NA    NA        2
      13:       NA       NA    NA     NA       NA       NA     NA    NA        3
      14:       NA       NA    NA     NA       NA       NA     NA    NA        3
      15:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      16:       NA       NA    NA     NA       NA       NA     NA    NA        1
      17:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      18:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      19:       NA       NA    NA     NA       NA       NA     NA    NA        2
      20:       NA       NA    NA     NA       NA       NA     NA    NA        1
          FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF IMPSUBIF MEDSIF MSAIF NACCALZP
             <num>    <num> <num>  <num>    <num>    <num>  <num> <num>    <num>
          NACCLBDP NEOPIF OTHCOGIF OTHPSYIF PRIONIF PSPIF PTSDDXIF SCHIZOIF
             <num>  <num>    <num>    <num>   <num> <num>    <num>    <num>
       1:       NA     NA       NA       NA      NA    NA       NA       NA
       2:       NA     NA       NA       NA      NA    NA       NA       NA
       3:       NA     NA       NA       NA      NA    NA       NA       NA
       4:       NA     NA       NA       NA      NA    NA        2       NA
       5:       NA     NA       NA       NA      NA    NA       NA       NA
       6:       NA     NA       NA       NA      NA    NA       NA       NA
       7:       NA     NA       NA       NA      NA    NA       NA       NA
       8:       NA     NA       NA       NA      NA    NA       NA       NA
       9:       NA     NA       NA       NA      NA    NA       NA       NA
      10:       NA     NA       NA       NA      NA    NA       NA       NA
      11:       NA     NA       NA       NA      NA    NA       NA       NA
      12:       NA     NA       NA       NA      NA    NA       NA       NA
      13:       NA     NA       NA       NA      NA    NA       NA       NA
      14:       NA     NA       NA       NA      NA    NA       NA       NA
      15:       NA     NA       NA       NA      NA    NA       NA       NA
      16:       NA     NA       NA       NA      NA    NA       NA       NA
      17:       NA     NA       NA       NA      NA    NA        2       NA
      18:       NA     NA        1       NA      NA    NA       NA       NA
      19:       NA      3       NA       NA      NA    NA       NA       NA
      20:       NA     NA       NA       NA      NA    NA       NA       NA
          NACCLBDP NEOPIF OTHCOGIF OTHPSYIF PRIONIF PSPIF PTSDDXIF SCHIZOIF
             <num>  <num>    <num>    <num>   <num> <num>    <num>    <num>
                     COGOTHX COGOTH2X COGOTH3X              OTHCOGX OTHPSYX CESDTOTAL
                      <char>   <char>   <char>               <char>  <char>     <num>
       1:           Insomnia     <NA>     <NA>                 <NA>    <NA>         5
       2:               <NA>     <NA>     <NA>                 <NA>    <NA>        NA
       3:               <NA>     <NA>     <NA>                 <NA>    <NA>         3
       4:               <NA>     <NA>     <NA>                 <NA>    <NA>         4
       5:               <NA>     <NA>     <NA>                 <NA>    <NA>        20
       6:               <NA>     <NA>     <NA>                 <NA>    <NA>        NA
       7:               <NA>     <NA>     <NA>                 <NA>    <NA>         3
       8:               <NA>     <NA>     <NA>                 <NA>    <NA>         1
       9:               <NA>     <NA>     <NA>                 <NA>    <NA>        NA
      10:               <NA>     <NA>     <NA>                 <NA>    <NA>        NA
      11:               <NA>     <NA>     <NA> meningioma resection    <NA>         0
      12:               <NA>     <NA>     <NA>                 <NA>    <NA>         7
      13:               <NA>     <NA>     <NA>                 <NA>    <NA>        NA
      14:               <NA>     <NA>     <NA>                 <NA>    <NA>        NA
      15: chronic migraines      <NA>     <NA>                 <NA>    <NA>         7
      16:               <NA>     <NA>     <NA>                 <NA>    <NA>         0
      17:               <NA>     <NA>     <NA>                 <NA>    <NA>         1
      18:               <NA>     <NA>     <NA>                 <NA>    <NA>         5
      19:               <NA>     <NA>     <NA>                 <NA>    <NA>        13
      20:               <NA>     <NA>     <NA>                 <NA>    <NA>         0
                     COGOTHX COGOTH2X COGOTH3X              OTHCOGX OTHPSYX CESDTOTAL
                      <char>   <char>   <char>               <char>  <char>     <num>
          wadrc_c2_behavioral_observations_checklist_complete wadrc_c2_boc_mood___1
                                                        <num>                 <num>
       1:                                                   0                     0
       2:                                                   0                     0
       3:                                                   0                     0
       4:                                                   0                     0
       5:                                                   0                     0
       6:                                                   0                     0
       7:                                                   0                     0
       8:                                                   0                     0
       9:                                                   0                     0
      10:                                                   0                     0
      11:                                                   0                     0
      12:                                                   0                     0
      13:                                                   0                     0
      14:                                                   0                     0
      15:                                                   0                     0
      16:                                                   0                     0
      17:                                                   0                     0
      18:                                                   0                     0
      19:                                                   0                     0
      20:                                                   0                     0
          wadrc_c2_behavioral_observations_checklist_complete wadrc_c2_boc_mood___1
                                                        <num>                 <num>
          wadrc_c2_boc_mood___2 wadrc_c2_boc_mood___3 wadrc_c2_boc_mood___4
                          <num>                 <num>                 <num>
       1:                     0                     0                     0
       2:                     0                     0                     0
       3:                     0                     0                     0
       4:                     0                     0                     0
       5:                     0                     0                     0
       6:                     0                     0                     0
       7:                     0                     0                     0
       8:                     0                     0                     0
       9:                     0                     0                     0
      10:                     0                     0                     0
      11:                     0                     0                     0
      12:                     0                     0                     0
      13:                     0                     0                     0
      14:                     0                     0                     0
      15:                     0                     0                     0
      16:                     0                     0                     0
      17:                     0                     0                     0
      18:                     0                     0                     0
      19:                     0                     0                     0
      20:                     0                     0                     0
          wadrc_c2_boc_mood___2 wadrc_c2_boc_mood___3 wadrc_c2_boc_mood___4
                          <num>                 <num>                 <num>
          wadrc_c2_boc_mood___5 wadrc_c2_boc_affect___1 wadrc_c2_boc_affect___2
                          <num>                   <num>                   <num>
       1:                     0                       0                       0
       2:                     0                       0                       0
       3:                     0                       0                       0
       4:                     0                       0                       0
       5:                     0                       0                       0
       6:                     0                       0                       0
       7:                     0                       0                       0
       8:                     0                       0                       0
       9:                     0                       0                       0
      10:                     0                       0                       0
      11:                     0                       0                       0
      12:                     0                       0                       0
      13:                     0                       0                       0
      14:                     0                       0                       0
      15:                     0                       0                       0
      16:                     0                       0                       0
      17:                     0                       0                       0
      18:                     0                       0                       0
      19:                     1                       0                       0
      20:                     0                       0                       0
          wadrc_c2_boc_mood___5 wadrc_c2_boc_affect___1 wadrc_c2_boc_affect___2
                          <num>                   <num>                   <num>
          wadrc_c2_boc_affect___3 wadrc_c2_boc_affect___4 wadrc_c2_boc_affect___5
                            <num>                   <num>                   <num>
       1:                       0                       0                       0
       2:                       0                       1                       0
       3:                       0                       0                       0
       4:                       0                       0                       0
       5:                       0                       0                       0
       6:                       0                       0                       0
       7:                       0                       0                       0
       8:                       0                       0                       0
       9:                       0                       0                       0
      10:                       0                       0                       0
      11:                       0                       0                       0
      12:                       0                       1                       0
      13:                       0                       0                       0
      14:                       0                       0                       0
      15:                       0                       0                       0
      16:                       0                       0                       0
      17:                       0                       0                       0
      18:                       0                       0                       0
      19:                       0                       0                       0
      20:                       0                       0                       0
          wadrc_c2_boc_affect___3 wadrc_c2_boc_affect___4 wadrc_c2_boc_affect___5
                            <num>                   <num>                   <num>
          wadrc_c2_boc_attitude___1 wadrc_c2_boc_attitude___2
                              <num>                     <num>
       1:                         0                         0
       2:                         0                         0
       3:                         0                         0
       4:                         0                         0
       5:                         0                         0
       6:                         0                         0
       7:                         0                         0
       8:                         0                         0
       9:                         0                         0
      10:                         0                         0
      11:                         0                         0
      12:                         0                         0
      13:                         0                         0
      14:                         0                         0
      15:                         0                         0
      16:                         0                         0
      17:                         0                         0
      18:                         0                         0
      19:                         0                         0
      20:                         0                         0
          wadrc_c2_boc_attitude___1 wadrc_c2_boc_attitude___2
                              <num>                     <num>
          wadrc_c2_boc_attitude___3 wadrc_c2_boc_attitude___4
                              <num>                     <num>
       1:                         0                         0
       2:                         0                         0
       3:                         0                         0
       4:                         0                         0
       5:                         0                         0
       6:                         0                         0
       7:                         0                         0
       8:                         0                         0
       9:                         0                         0
      10:                         0                         0
      11:                         0                         0
      12:                         0                         0
      13:                         0                         0
      14:                         0                         0
      15:                         0                         0
      16:                         0                         0
      17:                         0                         0
      18:                         0                         0
      19:                         0                         0
      20:                         0                         0
          wadrc_c2_boc_attitude___3 wadrc_c2_boc_attitude___4
                              <num>                     <num>
          wadrc_c2_boc_attitude___5 wadrc_c2_boc_language___1
                              <num>                     <num>
       1:                         0                         0
       2:                         0                         0
       3:                         0                         0
       4:                         0                         0
       5:                         0                         0
       6:                         0                         0
       7:                         0                         0
       8:                         0                         0
       9:                         0                         0
      10:                         0                         0
      11:                         0                         0
      12:                         0                         0
      13:                         0                         0
      14:                         0                         0
      15:                         0                         0
      16:                         0                         0
      17:                         0                         0
      18:                         0                         0
      19:                         0                         0
      20:                         0                         0
          wadrc_c2_boc_attitude___5 wadrc_c2_boc_language___1
                              <num>                     <num>
          wadrc_c2_boc_language___2 wadrc_c2_boc_language___3
                              <num>                     <num>
       1:                         0                         0
       2:                         0                         0
       3:                         0                         0
       4:                         0                         0
       5:                         0                         0
       6:                         0                         0
       7:                         0                         0
       8:                         0                         0
       9:                         0                         0
      10:                         0                         0
      11:                         0                         0
      12:                         0                         0
      13:                         0                         0
      14:                         0                         0
      15:                         0                         0
      16:                         0                         0
      17:                         0                         0
      18:                         0                         0
      19:                         0                         0
      20:                         0                         0
          wadrc_c2_boc_language___2 wadrc_c2_boc_language___3
                              <num>                     <num>
          wadrc_c2_boc_language___4 wadrc_c2_boc_language___5
                              <num>                     <num>
       1:                         0                         0
       2:                         0                         0
       3:                         0                         0
       4:                         0                         0
       5:                         0                         0
       6:                         0                         0
       7:                         0                         0
       8:                         0                         0
       9:                         0                         0
      10:                         0                         0
      11:                         0                         0
      12:                         0                         0
      13:                         0                         0
      14:                         0                         0
      15:                         0                         0
      16:                         0                         0
      17:                         0                         0
      18:                         0                         0
      19:                         0                         0
      20:                         0                         0
          wadrc_c2_boc_language___4 wadrc_c2_boc_language___5
                              <num>                     <num>
          wadrc_c2_boc_snsry_fncn___0 wadrc_c2_boc_snsry_fncn___1
                                <num>                       <num>
       1:                           0                           0
       2:                           0                           0
       3:                           0                           0
       4:                           0                           0
       5:                           0                           0
       6:                           0                           0
       7:                           0                           0
       8:                           0                           0
       9:                           0                           0
      10:                           0                           0
      11:                           0                           0
      12:                           0                           0
      13:                           0                           0
      14:                           0                           0
      15:                           0                           0
      16:                           0                           0
      17:                           0                           0
      18:                           0                           0
      19:                           0                           0
      20:                           0                           0
          wadrc_c2_boc_snsry_fncn___0 wadrc_c2_boc_snsry_fncn___1
                                <num>                       <num>
          wadrc_c2_boc_snsry_fncn___2 wadrc_c2_boc_snsry_fncn___3
                                <num>                       <num>
       1:                           0                           0
       2:                           0                           0
       3:                           0                           0
       4:                           0                           0
       5:                           0                           0
       6:                           0                           0
       7:                           0                           0
       8:                           0                           0
       9:                           0                           0
      10:                           0                           0
      11:                           0                           0
      12:                           0                           0
      13:                           0                           0
      14:                           0                           0
      15:                           0                           0
      16:                           0                           0
      17:                           0                           0
      18:                           0                           0
      19:                           0                           0
      20:                           0                           0
          wadrc_c2_boc_snsry_fncn___2 wadrc_c2_boc_snsry_fncn___3
                                <num>                       <num>
          wadrc_c2_boc_snsry_fncn___4 wadrc_c2_boc_snsry_fncn___5
                                <num>                       <num>
       1:                           0                           0
       2:                           0                           0
       3:                           0                           0
       4:                           0                           0
       5:                           0                           0
       6:                           0                           0
       7:                           0                           0
       8:                           0                           0
       9:                           0                           0
      10:                           0                           0
      11:                           0                           0
      12:                           0                           0
      13:                           0                           0
      14:                           0                           0
      15:                           0                           0
      16:                           0                           0
      17:                           0                           0
      18:                           0                           0
      19:                           0                           0
      20:                           0                           0
          wadrc_c2_boc_snsry_fncn___4 wadrc_c2_boc_snsry_fncn___5
                                <num>                       <num>
          wadrc_c2_boc_comprhnsn___1 wadrc_c2_boc_comprhnsn___2
                               <num>                      <num>
       1:                          0                          0
       2:                          0                          0
       3:                          0                          0
       4:                          0                          0
       5:                          0                          0
       6:                          0                          0
       7:                          0                          0
       8:                          0                          0
       9:                          0                          0
      10:                          0                          0
      11:                          0                          0
      12:                          0                          0
      13:                          0                          0
      14:                          0                          0
      15:                          0                          0
      16:                          0                          0
      17:                          0                          0
      18:                          0                          0
      19:                          0                          0
      20:                          0                          0
          wadrc_c2_boc_comprhnsn___1 wadrc_c2_boc_comprhnsn___2
                               <num>                      <num>
          wadrc_c2_boc_comprhnsn___3 wadrc_c2_boc_battery wadrc_c2_boc_notes respval
                               <num>                <num>             <char>   <num>
       1:                          0                   NA               <NA>       1
       2:                          0                   NA               <NA>       1
       3:                          0                   NA               <NA>       1
       4:                          0                   NA               <NA>       3
       5:                          0                   NA               <NA>       1
       6:                          0                   NA               <NA>       1
       7:                          0                   NA               <NA>       3
       8:                          0                   NA               <NA>       1
       9:                          0                   NA               <NA>       3
      10:                          0                   NA               <NA>       1
      11:                          0                   NA               <NA>      NA
      12:                          0                   NA               <NA>       1
      13:                          0                   NA               <NA>       1
      14:                          0                   NA               <NA>       1
      15:                          0                   NA               <NA>       1
      16:                          0                   NA               <NA>       1
      17:                          0                   NA               <NA>       1
      18:                          0                   NA               <NA>      NA
      19:                          0                   NA               <NA>      NA
      20:                          0                   NA               <NA>       1
          wadrc_c2_boc_comprhnsn___3 wadrc_c2_boc_battery wadrc_c2_boc_notes respval
                               <num>                <num>             <char>   <num>
          loc_res___1 loc_res___2 loc_res___3 loc_res___4 loc_res___5 loc_res___6
                <num>       <num>       <num>       <num>       <num>       <num>
       1:           0           0           0           0           0           0
       2:           0           0           0           0           0           0
       3:           0           0           0           0           0           0
       4:           0           0           0           0           0           0
       5:           0           0           0           0           0           0
       6:           0           0           0           0           0           0
       7:           0           0           0           0           0           0
       8:           0           0           0           0           0           0
       9:           0           0           0           0           0           0
      10:           0           0           0           0           0           0
      11:           0           0           0           0           0           0
      12:           0           0           0           0           0           0
      13:           0           0           0           0           0           0
      14:           0           0           0           0           0           0
      15:           0           0           0           0           0           0
      16:           0           0           0           0           0           0
      17:           0           0           0           0           0           0
      18:           0           0           0           0           0           0
      19:           0           0           0           0           0           0
      20:           0           0           0           0           0           0
          loc_res___1 loc_res___2 loc_res___3 loc_res___4 loc_res___5 loc_res___6
                <num>       <num>       <num>       <num>       <num>       <num>
          loc_res___7 loc_res___8                  respothx
                <num>       <num>                    <char>
       1:           0           0                      <NA>
       2:           0           0                      <NA>
       3:           0           0                      <NA>
       4:           0           0                      <NA>
       5:           0           0                      <NA>
       6:           0           0                      <NA>
       7:           0           1 Test was not administered
       8:           0           0                      <NA>
       9:           0           0                      <NA>
      10:           0           0 Test was not administered
      11:           0           0                      <NA>
      12:           0           0 Test was not administered
      13:           0           0 Test was not administered
      14:           0           1 Test was not administered
      15:           0           0                      <NA>
      16:           0           0                      <NA>
      17:           0           0                      <NA>
      18:           0           1                      <NA>
      19:           0           0                      <NA>
      20:           0           0                      <NA>
          loc_res___7 loc_res___8                  respothx
                <num>       <num>                    <char>


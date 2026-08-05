# the NACC column contract is stable (review snapshot on first run)

    Code
      print(contract, row.names = FALSE)
    Output
          column           class
           BILLS         numeric
         BIRTHMO         numeric
         BIRTHYR         numeric
            EDUC         numeric
          EVENTS         numeric
             FAS         numeric
           GAMES         numeric
          HANDED         numeric
        MEALPREP         numeric
        MOCACLOC         numeric
       MOCACLOCK ntrs::MOCACLOCK
        MOCACLOH         numeric
        MOCACLON         numeric
          NACCID       character
         PAYATTN         numeric
            RACE         numeric
        REMDATES         numeric
         REY1REC   ntrs::REY1REC
         REY2REC   ntrs::REY2REC
         REY3REC   ntrs::REY3REC
         REY4REC   ntrs::REY4REC
         REY5REC   ntrs::REY5REC
         REYAREC   ntrs::REYAREC
         REYFPOS   ntrs::REYFPOS
         REYTCOR   ntrs::REYTCOR
        REYTOTAL  ntrs::REYTOTAL
             SEX         numeric
        SHOPPING         numeric
           STOVE         numeric
           TAXES         numeric
          TRAVEL         numeric
        VISITDAY         numeric
         VISITMO         numeric
         VISITYR         numeric

# pull_redcap_data works for UDS-2

    Code
      scramble_uds2
    Output
              NACCID VISITYR VISITMO VISITDAY   SEX  EDUC  RACE IQCODEINFORM
              <char>   <num>   <num>    <num> <num> <num> <num>        <num>
         1: sim00001    2011       1       14     2    18     1         3.00
         2: sim00001    2011      12        9     2    18     1         3.00
         3: sim00001    2013       5        3     2    18     1         3.00
         4: sim00001    2013       5       31     2    18     1         3.68
         5: sim00001    2013      10        9     2    18     1         3.06
        ---                                                                 
      1634: sim00613    2011       9       12     2    12     1         2.75
      1635: sim00614    2012      10       11     2    12     1         3.00
      1636: sim00615    2014       8       29     2    20     1         3.00
      1637: sim00616    2011      12       15     1    16     1         3.00
      1638: sim00617    2012       8        3     2    14     1           NA
            IQCODESELF HANDED CDRGLOB TRAILA TRAILARR TRAILALI  WAIS ANIMALS   VEG
                 <num>  <num>   <num>  <num>    <num>    <num> <num>   <num> <num>
         1:       2.88      2     0.5     12        1       -4    46      24    22
         2:       3.94      2     0.5     29        0       24    59      27     4
         3:       3.38      2     0.5     14        0       NA    95      32    19
         4:       4.00      2     0.5     28        0       24    NA      19     4
         5:       3.13      2     0.0    150        0       NA    97      24    13
        ---                                                                       
      1634:       2.88      2     0.5     35        0       24    60      23    97
      1635:       3.13      2     0.0     45        0       24    74      33     2
      1636:       3.00      2     0.0     16        0       24    65      15    15
      1637:       3.00      2     0.0     35        0       24    NA      18     8
      1638:       3.63      2     0.0     20        0       24    97      18    11
            REY1REC REY2REC REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC TRAILB
              <num>   <num>   <num>   <num>   <num>    <num>   <num>   <num>  <num>
         1:      NA       3      13      12      13        2       2      10    300
         2:       5       6       7      12      11       NA      13      11     NA
         3:       7       1      10      10       8       NA       7       0    996
         4:       9       3       9      10       8       10       0      10     46
         5:       8      11      NA       8      14        6       8      NA     38
        ---                                                                        
      1634:       5       5       6      11      14       NA       0       6     45
      1635:       6       7       9      10       7        3       2      14     91
      1636:      NA       4       3       7      14        3       8       0    107
      1637:       6      12       8      13      15        5      15      NA    128
      1638:      NA       7       0      14      15        3      11       6     99
            TRAILBRR TRAILBLI NACCGDS CDRSUM NACCMMSE BOSTON LOGIMEM MEMUNITS DIGIF
               <num>    <num>   <num>  <num>    <num>  <num>   <num>    <num> <num>
         1:        0       24       0    0.0       30     29      19        3    97
         2:        1       24       0    0.0       27     29       9       12     7
         3:        3       24       1    0.0       18     30      11        6     8
         4:       -4       24       7    1.5       21     97      17        5    12
         5:       NA       24       0    0.0       29     27      12        2     3
        ---                                                                        
      1634:        1       24       1    0.0       27     28       2       97     6
      1635:        2       24       2    9.0       30     NA       9       15    10
      1636:       NA       24       0    0.0       19     28      13       11     7
      1637:        0       24       0    0.0       97     97       8       16    12
      1638:        0       24       0    0.0       97     28      17        0    97
            DIGIFLEN DIGIB DIGIBLEN MEMTIME BILLS TAXES SHOPPING GAMES STOVE MEALPREP
               <num> <num>    <num>   <num> <num> <num>    <num> <num> <num>    <num>
         1:        5    NA        4      19     0     8        0     0     3        2
         2:        8    10        3      15     0     0        0     0     8        0
         3:        8     7        3      18     0     0        0     3     0        0
         4:        5     8        3      16     0     0        8     1     0        0
         5:        4     6        4      19     0     0        0     0     0        0
        ---                                                                          
      1634:        7     9        4      19     0     8        0     0     0        0
      1635:        6     9        7      19     2     8        0     0     0        0
      1636:        6     5        5      23     1     0        0     0     0        0
      1637:       NA    12        4      14     0     0        0     0     0        0
      1638:        6     8        4      15     0     0        0     0     1        0
            EVENTS PAYATTN REMDATES TRAVEL BIRTHYR BIRTHMO ALCDEM ANXIET BIPOLDX
             <num>   <num>    <num>  <num>   <num>   <num>  <num>  <num>   <num>
         1:      0       0        2      8    1945       5      0     NA      NA
         2:      0       0        0      0    1945       5      0     NA      NA
         3:      0       0        2      3    1945       5     NA     NA      NA
         4:      0       0        0      0    1945       5     NA     NA      NA
         5:      0       0        8      0    1945       5      0     NA      NA
        ---                                                                     
      1634:      0       8        0      0    1934       6      0     NA      NA
      1635:      0       0        0      0    1955       2      0     NA      NA
      1636:      0       0        0      0    1959       1     NA     NA      NA
      1637:      0       0        0      0    1933       6     NA     NA      NA
      1638:      2       0        0      0    1934       8     NA     NA      NA
            BRNINJ COGOTH COGOTH2 COGOTH3  CORT   CVD DELIR DEMUN   DEP DOWNS DYSILL
             <num>  <num>   <num>   <num> <num> <num> <num> <num> <num> <num>  <num>
         1:      0      0       0       0     0    NA    NA    NA     0     0      0
         2:      0      0       0       0     0    NA    NA    NA     0     0      1
         3:      0      0       0       0     0    NA    NA     0     1     0      0
         4:      0      0       0       0     0    NA    NA    NA     0     0      0
         5:      0      0       0       0     0    NA    NA    NA     0     0      0
        ---                                                                         
      1634:      0      0       0       0     0    NA    NA    NA     0     0      0
      1635:      0      0       0       0     0    NA    NA     0     0     0      0
      1636:      0      0       0       0     0    NA    NA     0     0     0      0
      1637:      0      0       0       0     0    NA    NA    NA     0     0      0
      1638:      0      1       0       0     0    NA    NA    NA     0     0      0
            EPILEP ESSTREM FTLDMO FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS   MSA
             <num>   <num>  <num>   <num> <num> <num>  <num>  <num> <num> <num>
         1:     NA      NA     NA      NA    NA     0      0     NA     0    NA
         2:     NA      NA     NA      NA    NA     0      0     NA     0    NA
         3:     NA      NA     NA      NA    NA     0      0     NA     0    NA
         4:     NA      NA     NA      NA    NA     0      0     NA     0    NA
         5:     NA      NA     NA      NA    NA     0      0     NA     0    NA
        ---                                                                    
      1634:     NA      NA     NA      NA    NA     0      0     NA     0    NA
      1635:     NA      NA     NA      NA    NA     0      0     NA     0    NA
      1636:     NA      NA     NA      NA    NA     0      0     NA     0    NA
      1637:     NA      NA     NA      NA    NA     0      0     NA     0    NA
      1638:     NA      NA     NA      NA    NA     0      0     NA     0    NA
            NACCALZD NACCLBDE  NEOP OTHCOG OTHPSY POSSAD PPAPH PRION PROBAD   PSP
               <num>    <num> <num>  <num>  <num>  <num> <num> <num>  <num> <num>
         1:       NA       NA     0     NA      0     NA    NA     0     NA     0
         2:       NA       NA     0     NA      0     NA     0     0     NA     0
         3:       NA       NA     0     NA      0     NA    NA     0     NA     0
         4:       NA       NA     0     NA      0     NA     0     0     NA     0
         5:       NA       NA     0     NA      0     NA    NA     0     NA     0
        ---                                                                      
      1634:       NA       NA     0     NA      0     NA     0     0     NA     0
      1635:       NA       NA     0     NA      0      1    NA     0      1     0
      1636:       NA       NA     0     NA      0     NA     0     0     NA     0
      1637:       NA       NA     0     NA      1     NA    NA     0     NA     0
      1638:       NA       NA     0     NA      0     NA     0     0     NA     0
            PTSDDX SCHIZOP STROKE  VASC VASCPS ALCDEMIF ANXIETIF BIPOLDIF BRNINJIF
             <num>   <num>  <num> <num>  <num>    <num>    <num>    <num>    <num>
         1:     NA      NA      0    NA     NA       NA       NA       NA       NA
         2:     NA      NA      0     0      0       NA       NA       NA       NA
         3:     NA      NA      0    NA      1       NA       NA       NA       NA
         4:     NA      NA      0    NA     NA       NA       NA       NA       NA
         5:     NA      NA      0    NA      0       NA       NA       NA       NA
        ---                                                                       
      1634:     NA      NA      0    NA      0       NA       NA       NA       NA
      1635:     NA      NA      0    NA     NA       NA       NA       NA       NA
      1636:     NA      NA      0    NA     NA       NA       NA       NA       NA
      1637:     NA      NA      0    NA     NA       NA       NA       NA       NA
      1638:     NA      NA      0     0      0       NA       NA       NA       NA
            COGOTHIF COGOTH2F COGOTH3F CORTIF CVDIF DELIRIF DEMUNIF DEPIF DOWNSIF
               <num>    <num>    <num>  <num> <num>   <num>   <num> <num>   <num>
         1:       NA       NA       NA     NA    NA      NA      NA    NA      NA
         2:       NA       NA       NA     NA    NA      NA      NA    NA      NA
         3:       NA       NA       NA     NA    NA      NA      NA    NA      NA
         4:       NA       NA       NA     NA    NA      NA      NA    NA      NA
         5:       NA       NA       NA     NA    NA      NA      NA    NA      NA
        ---                                                                      
      1634:       NA       NA       NA     NA    NA      NA      NA    NA      NA
      1635:       NA       NA       NA     NA    NA      NA      NA    NA      NA
      1636:       NA       NA       NA     NA    NA      NA      NA    NA      NA
      1637:       NA       NA       NA     NA    NA      NA      NA    NA      NA
      1638:       NA       NA       NA     NA    NA      NA      NA    NA      NA
            DYSILLIF EPILEPIF ESSTREIF FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF
               <num>    <num>    <num>    <num>    <num> <num>  <num>    <num>
         1:       NA       NA       NA       NA       NA    NA     NA       NA
         2:       NA       NA       NA       NA       NA    NA     NA       NA
         3:       NA       NA       NA       NA       NA    NA     NA       NA
         4:       NA       NA       NA       NA       NA    NA     NA       NA
         5:       NA       NA       NA       NA       NA    NA     NA       NA
        ---                                                                   
      1634:       NA       NA       NA       NA       NA    NA     NA       NA
      1635:       NA       NA       NA       NA       NA    NA     NA       NA
      1636:       NA       NA       NA       NA       NA    NA     NA       NA
      1637:       NA       NA       NA       NA       NA    NA     NA       NA
      1638:       NA       NA       NA       NA       NA    NA     NA       NA
            IMPSUBIF MEDSIF MSAIF NACCALZP NACCLBDP NEOPIF OTHCOGIF OTHPSYIF POSSADIF
               <num>  <num> <num>    <num>    <num>  <num>    <num>    <num>    <num>
         1:       NA     NA    NA       NA       NA     NA       NA       NA       NA
         2:       NA     NA    NA       NA       NA     NA       NA       NA       NA
         3:       NA     NA    NA       NA       NA     NA       NA       NA       NA
         4:       NA     NA    NA       NA       NA     NA       NA       NA       NA
         5:       NA     NA    NA       NA       NA     NA       NA       NA       NA
        ---                                                                          
      1634:       NA     NA    NA       NA       NA     NA       NA       NA       NA
      1635:       NA     NA    NA       NA       NA     NA       NA       NA       NA
      1636:       NA     NA    NA       NA       NA     NA       NA       NA       NA
      1637:       NA     NA    NA       NA       NA     NA       NA       NA       NA
      1638:       NA     NA    NA       NA       NA     NA       NA       NA       NA
            PPAPHIF PRIONIF PROBADIF PSPIF SCHIZOIF STROKIF VASCIF VASCPSIF COGOTHX
              <num>   <num>    <num> <num>    <num>   <num>  <num>    <num>  <char>
         1:      NA      NA        1    NA       NA      NA     NA       NA    <NA>
         2:      NA      NA       NA    NA       NA      NA     NA       NA    <NA>
         3:      NA      NA       NA    NA       NA      NA     NA       NA    <NA>
         4:      NA      NA        1    NA       NA      NA     NA       NA    <NA>
         5:      NA      NA       NA    NA       NA      NA     NA       NA    <NA>
        ---                                                                        
      1634:      NA      NA       NA    NA       NA      NA     NA       NA    <NA>
      1635:      NA      NA       NA    NA       NA      NA     NA       NA    <NA>
      1636:      NA      NA        1    NA       NA      NA     NA       NA    <NA>
      1637:      NA      NA        1    NA       NA      NA     NA       NA    <NA>
      1638:      NA      NA        1    NA       NA      NA     NA       NA    <NA>
               COGOTH2X COGOTH3X OTHCOGX OTHPSYX CESDTOTAL
                 <char>   <char>   <num>   <num>     <num>
         1:        <NA>     <NA>      NA      NA        20
         2:        <NA>     <NA>      NA      NA        NA
         3:        <NA>     <NA>      NA      NA        17
         4:        <NA>     <NA>      NA      NA        NA
         5:        <NA>     <NA>      NA      NA        11
        ---                                               
      1634:        <NA>     <NA>      NA      NA         0
      1635: Sleep Apnea     <NA>      NA      NA        NA
      1636:        <NA>     <NA>      NA      NA         3
      1637:        <NA>     <NA>      NA      NA         5
      1638:        <NA>     <NA>      NA      NA        38

# pull_redcap_data works for UDS-3

    Code
      scramble_uds3
    Output
              NACCID   SEX  EDUC  RACE IQCODEINFORM IQCODESELF HANDED CDRGLOB
              <char> <num> <num> <num>        <num>      <num>  <num>   <num>
         1: sim00001     2    NA    NA           NA       3.13     NA      NA
         2: sim00001     2    NA    NA         3.00       3.19     NA      NA
         3: sim00001     2    NA    NA         3.00       3.38     NA     0.0
         4: sim00001     2    NA    NA           NA       3.00     NA     0.0
         5: sim00001     2    NA    NA           NA       3.06     NA     0.0
        ---                                                                  
      6539: sim01117     2    20    NA           NA         NA     NA     0.5
      6540: sim01118     1    NA    NA         4.44         NA      2      NA
      6541: sim01119     2    NA    NA           NA         NA     NA     0.0
      6542: sim01120     2    NA    NA         3.06         NA     NA      NA
      6543: sim01121     2    NA    NA         2.44       3.00      2     0.5
            MOCATOTS MOCBTOTS TRAILA TRAILARR TRAILALI OTRAILA OTRLARR DIGFORCT
               <num>    <num>  <num>    <num>    <num>   <num>   <num>    <num>
         1:       NA       NA     NA        0       24     888      NA        8
         2:       NA       NA     17       NA       NA      NA      NA       11
         3:       NA       NA     38        0       24      NA      NA        7
         4:       29       NA     20       NA       NA      NA      NA       NA
         5:       28       19     NA        0       NA      NA      NA       NA
        ---                                                                    
      6539:       30       NA     32       NA       24     888      NA       NA
      6540:       NA       NA     27        0       24      NA      NA        6
      6541:       30       NA     23        0       24      NA      NA        4
      6542:       NA       NA     45        0       24      NA      NA       NA
      6543:       24       88     NA       NA       24      NA      NA       11
            DIGFORSL DIGBACCT DIGBACLS   WAIS MINTTOTS ANIMALS   VEG UDSVERTN
               <num>    <num>    <num> <char>    <num>   <num> <num>    <num>
         1:        7        8        8   <NA>       31      25     5       32
         2:        4        9        5     39       32      30    NA       29
         3:        5        8        4   <NA>       29      25     7       16
         4:        8       NA        3     62       31      NA    17       25
         5:        8        5       NA   <NA>       NA      14     9       23
        ---                                                                  
      6539:        7       NA       NA     57       27      NA    16       NA
      6540:        8       98        7   <NA>       32      NA    NA       20
      6541:        8        6        4   <NA>       NA      25    11       37
      6542:       NA       12        7   <NA>       32      NA    16       26
      6543:       NA       NA        7   <NA>       NA      25    13        7
            UDSVERFC UDSVERLC UDSBENTC UDSBENTD CRAFTVRS CRAFTURS CRAFTDVR CRAFTDRE
               <num>    <num>    <num>    <num>    <num>    <num>    <num>    <num>
         1:        8       NA       10       12        2       19       NA       20
         2:       NA       11       17       12       16       11       24       NA
         3:       19       NA       15       NA       10        7       11       12
         4:       17       13       NA        9       25       18       26       NA
         5:       NA        9       15       10       20       NA       42        0
        ---                                                                        
      6539:       19       NA       16        0       26       NA       NA        9
      6540:       NA       NA       15       14       23       NA       26       NA
      6541:        7       NA       16        8       NA       17       16       20
      6542:        9       NA       NA       NA       31       NA       17        0
      6543:        9       NA       NA       NA       NA       20       30        7
            REY1REC REY2REC REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC REYTCOR
              <num>   <num>   <num>   <num>   <num>    <num>   <num>   <num>   <num>
         1:      NA      11      11      NA      13       NA      12      12      NA
         2:      NA       4       7      15      13       NA       9      NA      NA
         3:       6      14       8      13       9        6       5       7      15
         4:      10       9      NA      11      10       NA       3      12      14
         5:       8       3      NA      NA       9        7       6      12      15
        ---                                                                         
      6539:      NA      NA      NA       9      NA        5      NA       5      14
      6540:       5      NA      NA      NA      14        3      NA      NA      NA
      6541:       5      11      10      NA      NA       10      NA       6       9
      6542:       6      10      NA      13       8        8      14      NA      15
      6543:      NA      12       5      10      NA        4      NA       7      NA
            REYTNEG TRAILB TRAILBRR TRAILBLI MOCACLOC MOCACLOH MOCACLON OTRAILB
              <num>  <num>    <num>    <num>    <num>    <num>    <num>   <num>
         1:      NA     62        0       NA       NA        1       NA      NA
         2:      NA     51        0       NA       NA       NA        0      NA
         3:      15    181       NA       24        1       NA        1      NA
         4:      15     51        1       24       NA       NA        1      NA
         5:      12     NA        0       24       NA        1       NA      NA
        ---                                                                    
      6539:      NA    998        1       NA        1       NA        1      NA
      6540:      13     NA        0       24        1        1        1      NA
      6541:      13     NA        0       24       NA        1       NA      NA
      6542:      14     45       NA       NA        1        0        1      NA
      6543:      15     67       NA       24       NA        1       NA      NA
            OTRLBRR OTRLBLI NACCGDS CDRSUM UDSBENRS NACCUDSD NACCMMSE BOSTON LOGIMEM
              <num>   <num>   <num>  <num>    <num>    <num>    <num>  <num>   <num>
         1:      NA      NA       1    1.0       NA        1       NA     NA      NA
         2:      NA      NA       1     NA        1        4       NA     NA      NA
         3:      NA      NA       0    0.0        1       NA       NA     NA      NA
         4:      NA      NA      NA    2.5        1        1       NA     NA      NA
         5:      NA      NA       0    0.0        1        4       NA     NA      NA
        ---                                                                         
      6539:      NA      NA      NA    0.0        1        1       NA     NA      NA
      6540:      NA      NA      NA    0.0       NA        1       NA     NA      NA
      6541:      NA      NA       0    0.5        1       NA       NA     NA      NA
      6542:      NA      NA      NA    0.0        1        4       NA     NA      NA
      6543:      NA      NA       7   10.0       NA        4       NA     NA      NA
            MEMUNITS DIGIF DIGIFLEN DIGIB DIGIBLEN MEMTIME BILLS TAXES SHOPPING GAMES
               <num> <num>    <num> <num>    <num>   <num> <num> <num>    <num> <num>
         1:       NA    NA       NA    NA       NA      NA     0    NA        1     0
         2:       NA    NA       NA    NA       NA      NA     0     0        0     3
         3:       NA    NA       NA    NA       NA      NA    NA    NA        0     0
         4:       NA    NA       NA    NA       NA      NA     0     0        0     0
         5:       NA    NA       NA    NA       NA      NA     0    NA        1     0
        ---                                                                          
      6539:       NA    NA       NA    NA       NA      NA     0     8        0     2
      6540:       NA    NA       NA    NA       NA      NA     0     0        0     0
      6541:       NA    NA       NA    NA       NA      NA     0    NA        0     3
      6542:       NA    NA       NA    NA       NA      NA    NA     0        0     0
      6543:       NA    NA       NA    NA       NA      NA     0     0        0     3
            STOVE MEALPREP EVENTS PAYATTN REMDATES TRAVEL BIRTHYR BIRTHMO VISITYR
            <num>    <num>  <num>   <num>    <num>  <num>   <num>   <num>   <num>
         1:     0        0     NA       0       NA      2    1961       9    2014
         2:     0        0      0       0        0      0    1961       9    2015
         3:    NA        0     NA      NA       NA      0    1961       9    2016
         4:     0        0     NA       0        0      0    1961       9    2016
         5:     0       NA     NA       0       NA      0    1961       9    2017
        ---                                                                      
      6539:    NA        0      0       0        3     NA    1956      10    2018
      6540:     1       NA      0       2        0      0    1942      11    2023
      6541:     0        9     NA      NA        0      0    1952      12    2018
      6542:     1        0     NA      NA        0      1    1950      NA    2012
      6543:    NA        0      0      NA        0      0    1953      12    2022
            VISITMO VISITDAY ALCDEM ANXIET BIPOLDX BRNINJ COGOTH COGOTH2 COGOTH3
              <num>    <num>  <num>  <num>   <num>  <num>  <num>   <num>   <num>
         1:       1       16     NA     NA      NA     NA     NA      NA      NA
         2:       8       25     NA     NA      NA     NA     NA      NA      NA
         3:       2        8     NA      1      NA     NA     NA      NA      NA
         4:       4       22     NA     NA      NA     NA     NA      NA      NA
         5:       3       28     NA     NA      NA     NA     NA      NA      NA
        ---                                                                     
      6539:       7        5     NA     NA      NA     NA     NA      NA      NA
      6540:      12       12     NA     NA      NA     NA     NA      NA      NA
      6541:       4        3     NA     NA      NA     NA     NA      NA      NA
      6542:       1       30     NA     NA      NA     NA     NA      NA      NA
      6543:       7        8     NA     NA      NA     NA     NA      NA      NA
             CORT   CVD DELIR   DEP DOWNS DYSILL EPILEP ESSTREM FTLDMO FTLDNOS   HIV
            <num> <num> <num> <num> <num>  <num>  <num>   <num>  <num>   <num> <num>
         1:    NA    NA    NA    NA    NA     NA     NA      NA     NA      NA    NA
         2:    NA    NA    NA     1    NA     NA     NA      NA     NA      NA    NA
         3:    NA    NA    NA     1    NA     NA     NA      NA     NA      NA    NA
         4:    NA    NA    NA    NA    NA     NA     NA      NA     NA      NA    NA
         5:    NA    NA    NA    NA    NA     NA     NA      NA     NA      NA    NA
        ---                                                                         
      6539:    NA    NA    NA    NA    NA     NA     NA      NA     NA      NA    NA
      6540:    NA    NA    NA    NA    NA     NA     NA      NA     NA      NA    NA
      6541:    NA    NA    NA    NA    NA     NA     NA      NA     NA      NA    NA
      6542:    NA    NA    NA    NA    NA     NA     NA      NA     NA      NA    NA
      6543:    NA    NA    NA    NA    NA     NA     NA      NA     NA      NA    NA
             HUNT HYCEPH IMPSUB  MEDS   MSA NACCALZD NACCLBDE  NEOP OTHCOG OTHPSY
            <num>  <num>  <num> <num> <num>    <num>    <num> <num>  <num>  <num>
         1:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
         2:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
         3:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
         4:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
         5:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
        ---                                                                      
      6539:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
      6540:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
      6541:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
      6542:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
      6543:    NA     NA     NA    NA    NA       NA       NA    NA     NA     NA
            PPAPH PRION   PSP PTSDDX SCHIZOP STROKE ALCDEMIF ANXIETIF BIPOLDIF
            <num> <num> <num>  <num>   <num>  <num>    <num>    <num>    <num>
         1:    NA    NA    NA     NA      NA      0       NA       NA       NA
         2:    NA    NA    NA     NA      NA     NA       NA       NA       NA
         3:    NA    NA    NA     NA      NA     NA       NA       NA       NA
         4:    NA    NA    NA     NA      NA     NA       NA       NA       NA
         5:    NA    NA    NA     NA      NA     NA       NA       NA       NA
        ---                                                                   
      6539:    NA    NA    NA     NA      NA     NA       NA       NA       NA
      6540:    NA    NA    NA     NA      NA     NA       NA       NA       NA
      6541:    NA    NA    NA     NA      NA     NA       NA       NA       NA
      6542:    NA    NA    NA      1      NA     NA       NA       NA       NA
      6543:    NA    NA    NA     NA      NA     NA       NA       NA       NA
            BRNINJIF COGOTHIF COGOTH2F COGOTH3F CORTIF CVDIF DELIRIF DEPIF DOWNSIF
               <num>    <num>    <num>    <num>  <num> <num>   <num> <num>   <num>
         1:       NA        1       NA       NA     NA    NA      NA    NA      NA
         2:       NA       NA       NA       NA     NA    NA      NA    NA      NA
         3:       NA       NA       NA       NA     NA    NA      NA    NA      NA
         4:       NA       NA       NA       NA     NA    NA      NA    NA      NA
         5:       NA       NA       NA       NA     NA    NA      NA    NA      NA
        ---                                                                       
      6539:       NA       NA       NA       NA     NA    NA      NA    NA      NA
      6540:       NA       NA       NA       NA     NA    NA      NA    NA      NA
      6541:       NA       NA       NA       NA     NA    NA      NA    NA      NA
      6542:       NA       NA       NA       NA     NA    NA      NA    NA      NA
      6543:       NA       NA       NA       NA     NA    NA      NA    NA      NA
            DYSILLIF EPILEPIF ESSTREIF FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF
               <num>    <num>    <num>    <num>    <num> <num>  <num>    <num>
         1:       NA       NA       NA       NA       NA    NA     NA       NA
         2:       NA       NA       NA       NA       NA    NA     NA       NA
         3:       NA       NA       NA       NA       NA    NA     NA       NA
         4:       NA       NA       NA       NA       NA    NA     NA       NA
         5:       NA       NA       NA       NA       NA    NA     NA       NA
        ---                                                                   
      6539:       NA       NA       NA       NA       NA    NA     NA       NA
      6540:       NA       NA       NA       NA       NA    NA     NA       NA
      6541:       NA       NA       NA       NA       NA    NA     NA       NA
      6542:       NA       NA       NA       NA       NA    NA     NA       NA
      6543:       NA       NA       NA       NA       NA    NA     NA       NA
            IMPSUBIF MEDSIF MSAIF NACCALZP NACCLBDP NEOPIF OTHCOGIF OTHPSYIF PRIONIF
               <num>  <num> <num>    <num>    <num>  <num>    <num>    <num>   <num>
         1:       NA     NA    NA       NA       NA     NA       NA       NA      NA
         2:       NA     NA    NA       NA       NA     NA       NA       NA      NA
         3:       NA     NA    NA       NA       NA     NA       NA       NA      NA
         4:       NA     NA    NA       NA       NA     NA       NA       NA      NA
         5:       NA     NA    NA       NA       NA     NA       NA       NA      NA
        ---                                                                         
      6539:       NA     NA    NA       NA       NA     NA       NA       NA      NA
      6540:       NA     NA    NA       NA       NA     NA       NA       NA      NA
      6541:       NA     NA    NA       NA       NA     NA       NA       NA      NA
      6542:       NA     NA    NA       NA       NA     NA       NA       NA      NA
      6543:       NA     NA    NA       NA       NA     NA       NA       NA      NA
            PSPIF PTSDDXIF SCHIZOIF COGOTHX COGOTH2X COGOTH3X OTHCOGX
            <num>    <num>    <num>  <char>   <char>   <char>  <char>
         1:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
         2:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
         3:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
         4:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
         5:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
        ---                                                          
      6539:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
      6540:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
      6541:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
      6542:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
      6543:    NA       NA       NA    <NA>     <NA>     <NA>    <NA>
                                   OTHPSYX CESDTOTAL respval respothx
                                    <char>     <num>   <num>   <char>
         1:                           <NA>        NA      NA     <NA>
         2:                           <NA>        NA      NA     <NA>
         3:                           <NA>        NA      NA     <NA>
         4:                           <NA>        NA      NA     <NA>
         5:                           <NA>         5      NA     <NA>
        ---                                                          
      6539:                           <NA>        NA      NA     <NA>
      6540:                           <NA>        13      NA     <NA>
      6541: Impulse control disorder, ADHD         1      NA     <NA>
      6542:                           <NA>        15      NA     <NA>
      6543:                           <NA>         4      NA     <NA>

# pull_redcap_data works for UDS-4

    Code
      scramble_uds4
    Output
           VISITYR VISITMO VISITDAY   SEX   NACCID  EDUC  RACE IQCODEINFORM
             <num>   <num>    <int> <num>   <char> <num> <num>        <num>
        1:    2025      11        4     2 sim00001    12     1       3.0625
        2:    2025       7       25     2 sim00002    18     3       3.8125
        3:    2025       9       15     2 sim00003    16     3       3.0625
        4:    2025       9       18    NA sim00004    18     1       3.1250
        5:    2025      11       24     2 sim00005    13     1       4.0000
       ---                                                                 
      279:    2025       9        5     2 sim00279    18     2       3.0000
      280:    2025      12       10     1 sim00280    12    NA       3.1875
      281:    2025      11        5     2 sim00281    25     1           NA
      282:    2025       9       30     2 sim00282    14     1       3.9375
      283:    2025       9       26     2 sim00283    17     1           NA
           IQCODESELF HANDED CDRGLOB MOCATOTS MOCBTOTS TRAILA TRAILARR TRAILALI
                <num>  <num>   <num>    <num>    <num>  <num>    <num>    <num>
        1:         NA      2     3.0       26       NA     NA        0       24
        2:     3.0000      2     0.0       25       NA     91       NA       NA
        3:     4.0000      2     0.5       NA       NA     60        0       NA
        4:     3.8125      2     0.5       NA       NA     31       NA       NA
        5:         NA      9     1.0       30       NA     NA        0       24
       ---                                                                     
      279:         NA      2     0.5       NA       NA     NA       NA       NA
      280:     3.0625      2     0.0       25       NA     28        1       24
      281:     3.0000      2     0.5       88       NA     NA        0       24
      282:     3.0000     NA     1.0       29       NA     NA        0       24
      283:         NA      2     1.0       29       NA     NA        0       24
           OTRAILA OTRLARR DIGFORCT DIGFORSL DIGBACCT DIGBACLS  WAIS MINTTOTS ANIMALS
             <num>   <num>    <num>    <num>    <num>    <num> <num>    <num>   <num>
        1:     997      NA        8        7        4        5    NA       NA      25
        2:     888      NA        7       NA        7        5    74       NA      25
        3:      NA      NA        9        4       97       NA    NA       32      25
        4:      NA      NA       NA        8        4        3    48       NA      24
        5:      NA      NA        9        7        8        6    63       NA      34
       ---                                                                           
      279:      NA      NA       14        7        6        5    58       24      14
      280:      NA      NA        6       NA       98        6    66       28      30
      281:      NA      NA        5        5        8       NA    55       NA      25
      282:     996      NA       96        8        8        6    57       30      15
      283:     888      NA       10        7       97        7    NA       32      NA
             VEG UDSVERTN UDSVERFC UDSVERLC UDSBENTC UDSBENTD CRAFTVRS CRAFTURS
           <num>    <num>    <num>    <num>    <num>    <num>    <num>    <num>
        1:    97       NA       23       15       15       13       28       15
        2:    11       48        7       97       16       NA       97       17
        3:     4       14        5       NA       16       12       18       19
        4:    15       NA       97       96       16       NA       20       17
        5:    17       NA       17       11       NA        0       40       17
       ---                                                                     
      279:    10       NA       18       10       NA       NA       97       NA
      280:    13       25       16       12       NA       12       29       18
      281:     9       23       97       16       NA       12       NA       11
      282:    NA       25       15       NA       16       14       97       18
      283:     4       34       11       NA       NA       10       24       16
           CRAFTDVR CRAFTDRE REY1REC REY2REC REY3REC REY4REC REY5REC REYDLIST REY6REC
              <num>    <num>   <num>   <num>   <num>   <num>   <num>    <num>   <num>
        1:       96       NA       9       5      NA       0      11       NA      13
        2:       96       18       8       6       6      12      14        7      14
        3:        8       20      12      NA      NA      98      13       98      NA
        4:       19        9       8       9      13      12      15        1      13
        5:       30       21      12      13       8      15      15        4      11
       ---                                                                           
      279:       24       NA      12      NA      11       6      13       NA       7
      280:       96       15      12       7      15       9      NA       NA      13
      281:       96       NA      97      15      NA      15       8        5      11
      282:       20       17       7       7       6      NA       6       NA       9
      283:       26       NA      NA       9      13       0       9        5      15
           REYDREC REYTCOR REYFPOS TRAILB TRAILBRR TRAILBLI MOCACLOC MOCACLOH
             <num>   <num>   <num>  <num>    <num>    <num>    <num>    <num>
        1:       6      15       4     78        0       24       NA        0
        2:      11      10       0     74       NA       24        0        1
        3:       4      15       1     NA        0       NA        1        0
        4:      10      15       1     NA        0       24        1        1
        5:      NA      NA      NA     66        0       NA       NA        1
       ---                                                                   
      279:       9      NA       1     NA       NA       NA        1        1
      280:       8      15       1    204        1       24       NA       NA
      281:      11      15      NA     78        0       24        1       NA
      282:      NA      15      NA    125       NA       24        1       NA
      283:      13      NA       1     52        0       24        1        0
           MOCACLON OTRAILB OTRLBRR OTRLBLI NACCGDS CDRSUM UDSBENRS NACCUDSD BILLS
              <num>   <num>   <num>   <num>   <num>  <num>    <num>    <num> <num>
        1:       NA      NA      NA      NA      NA    0.0       NA        1     0
        2:        1      NA      NA      NA       0    0.0        1        1     8
        3:        1      NA      NA      NA       0    0.0       NA        1     0
        4:        1     888      NA      NA       3     NA        1        1     8
        5:        1     997      NA      NA       1    0.5        1        1    NA
       ---                                                                        
      279:        1     996      NA      NA      NA    3.0       NA        4     0
      280:       NA     888      NA      NA       2    0.0        1        1     0
      281:        1      NA      NA      NA       1    0.0       NA        3     0
      282:       NA      NA      NA      NA      NA     NA        1        4     0
      283:        1     888      NA      NA      NA    0.0        1        1     0
           TAXES SHOPPING GAMES STOVE MEALPREP EVENTS PAYATTN REMDATES TRAVEL BIRTHYR
           <num>    <num> <num> <num>    <num>  <num>   <num>    <num>  <num>   <num>
        1:     0        0     0     0        0      0       0        0      3    1952
        2:     3        0     0     0        0      0       0        0      0    1944
        3:     8        0     0     0        0      0       3        0      0    1953
        4:     0        0     0     0        0      0       2        2      2    1960
        5:     0        2     0     0        3      0       0        0      3    1953
       ---                                                                           
      279:     0        0     0     0        0      0       0        0      0      NA
      280:     8        0     0     0        0      0       0        0      0    1963
      281:    NA        3     0    NA        0     NA       0        0      0    1948
      282:     0        0     0     3        0      0       0       NA      0    1956
      283:    NA        0     0     3       NA      3       0        0      3    1945
           BIRTHMO ALCDEM ANXIET BIPOLDX BRNINJ COGOTH COGOTH2 COGOTH3  CORT   CVD
             <num>  <num>  <num>   <num>  <num>  <num>   <num>   <num> <num> <num>
        1:       9     NA     NA      NA     NA     NA      NA      NA    NA    NA
        2:       9     NA     NA      NA     NA     NA      NA      NA    NA    NA
        3:       5     NA     NA      NA     NA     NA      NA      NA    NA    NA
        4:       2     NA     NA      NA     NA     NA      NA      NA    NA    NA
        5:       3     NA     NA      NA     NA     NA      NA      NA    NA    NA
       ---                                                                        
      279:       2     NA      1      NA     NA     NA      NA      NA    NA    NA
      280:      12     NA     NA      NA     NA     NA      NA      NA    NA     1
      281:       8     NA     NA      NA     NA     NA      NA      NA    NA    NA
      282:      12     NA     NA      NA     NA     NA      NA      NA    NA    NA
      283:       4     NA     NA      NA     NA     NA      NA      NA    NA     1
           DELIR DOWNS EPILEP FTLDMO FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS   MSA
           <num> <num>  <num>  <num>   <num> <num> <num>  <num>  <num> <num> <num>
        1:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
        2:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
        3:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
        4:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
        5:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
       ---                                                                        
      279:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
      280:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
      281:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
      282:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
      283:    NA    NA     NA     NA      NA    NA    NA     NA     NA    NA    NA
           NACCALZD NACCLBDE  NEOP OTHCOG OTHPSY PPAPH PRION   PSP PTSDDX SCHIZOP
              <num>    <num> <num>  <num>  <num> <num> <num> <num>  <num>   <num>
        1:        1       NA    NA     NA     NA    NA    NA    NA     NA      NA
        2:       NA       NA    NA     NA      1    NA    NA    NA     NA      NA
        3:        1        1    NA     NA     NA    NA    NA    NA     NA      NA
        4:       NA       NA    NA     NA     NA    NA    NA    NA     NA      NA
        5:        1       NA    NA     NA     NA    NA    NA    NA     NA      NA
       ---                                                                       
      279:       NA       NA    NA     NA     NA    NA    NA    NA     NA      NA
      280:        1       NA    NA     NA     NA    NA    NA    NA     NA      NA
      281:       NA        1    NA     NA     NA    NA    NA    NA     NA      NA
      282:       NA       NA    NA     NA     NA    NA    NA    NA     NA      NA
      283:       NA       NA    NA     NA     NA    NA    NA    NA     NA      NA
           STROKE ALCDEMIF ANXIETIF BIPOLDIF COGOTHIF COGOTH2F COGOTH3F CORTIF CVDIF
            <num>    <num>    <num>    <num>    <num>    <num>    <num>  <num> <num>
        1:     NA       NA       NA       NA       NA       NA       NA     NA    NA
        2:      1       NA        3       NA       NA       NA       NA     NA    NA
        3:      0       NA       NA       NA       NA       NA       NA     NA    NA
        4:      0       NA       NA       NA        2       NA       NA     NA    NA
        5:      0       NA       NA       NA       NA       NA       NA     NA    NA
       ---                                                                          
      279:      0       NA       NA       NA        2       NA       NA     NA    NA
      280:      0       NA       NA       NA       NA       NA       NA     NA    NA
      281:      0       NA        3       NA       NA       NA       NA     NA    NA
      282:      0       NA       NA       NA       NA       NA       NA     NA    NA
      283:      0       NA       NA       NA       NA       NA       NA     NA    NA
           DELIRIF DOWNSIF EPILEPIF FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF IMPSUBIF
             <num>   <num>    <num>    <num>    <num> <num>  <num>    <num>    <num>
        1:      NA      NA       NA       NA       NA    NA     NA       NA       NA
        2:      NA      NA       NA       NA       NA    NA     NA       NA       NA
        3:      NA      NA       NA       NA       NA    NA     NA       NA       NA
        4:      NA      NA       NA       NA       NA    NA     NA       NA       NA
        5:      NA      NA       NA       NA       NA    NA     NA       NA       NA
       ---                                                                          
      279:      NA      NA       NA       NA       NA    NA     NA       NA       NA
      280:      NA      NA       NA       NA       NA    NA     NA       NA       NA
      281:      NA      NA       NA       NA       NA    NA     NA       NA       NA
      282:      NA      NA       NA       NA       NA    NA     NA       NA       NA
      283:      NA      NA       NA       NA       NA    NA     NA       NA       NA
           MEDSIF MSAIF NACCALZP NACCLBDP NEOPIF OTHCOGIF OTHPSYIF PRIONIF PSPIF
            <num> <num>    <num>    <num>  <num>    <num>    <num>   <num> <num>
        1:     NA    NA       NA       NA     NA       NA       NA      NA    NA
        2:     NA    NA       NA       NA     NA       NA       NA      NA    NA
        3:     NA    NA       NA       NA     NA       NA       NA      NA    NA
        4:     NA    NA       NA       NA     NA       NA       NA      NA    NA
        5:     NA    NA       NA       NA     NA       NA       NA      NA    NA
       ---                                                                      
      279:     NA    NA        2       NA     NA       NA       NA      NA    NA
      280:     NA    NA       NA       NA     NA       NA       NA      NA    NA
      281:     NA    NA       NA       NA     NA       NA       NA      NA    NA
      282:     NA    NA       NA       NA     NA       NA       NA      NA    NA
      283:     NA    NA       NA       NA     NA       NA       NA      NA    NA
           PTSDDXIF SCHIZOIF COGOTHX COGOTH2X COGOTH3X OTHCOGX OTHPSYX CESDTOTAL
              <num>    <num>  <char>   <char>   <char>  <char>  <char>     <num>
        1:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>         3
        2:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>        NA
        3:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>        NA
        4:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>         2
        5:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>        16
       ---                                                                      
      279:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>         7
      280:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>        NA
      281:        3       NA    <NA>     <NA>     <NA>    <NA>    <NA>        10
      282:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>        NA
      283:       NA       NA    <NA>     <NA>     <NA>    <NA>    <NA>         6
           wadrc_c2_behavioral_observations_checklist_complete wadrc_c2_boc_mood___1
                                                         <num>                 <num>
        1:                                                   0                     0
        2:                                                   0                     0
        3:                                                   0                     0
        4:                                                   0                     0
        5:                                                   0                     0
       ---                                                                          
      279:                                                   0                     0
      280:                                                   0                     0
      281:                                                   0                     0
      282:                                                   0                     0
      283:                                                   0                     0
           wadrc_c2_boc_mood___2 wadrc_c2_boc_mood___3 wadrc_c2_boc_mood___4
                           <num>                 <num>                 <num>
        1:                     0                     0                     0
        2:                     0                     0                     0
        3:                     0                     0                     0
        4:                     0                     0                     0
        5:                     0                     0                     0
       ---                                                                  
      279:                     0                     0                     0
      280:                     0                     0                     0
      281:                     0                     0                     0
      282:                     0                     0                     0
      283:                     0                     0                     0
           wadrc_c2_boc_mood___5 wadrc_c2_boc_affect___1 wadrc_c2_boc_affect___2
                           <num>                   <num>                   <num>
        1:                     0                       0                       0
        2:                     0                       0                       0
        3:                     0                       0                       0
        4:                     0                       0                       0
        5:                     0                       0                       0
       ---                                                                      
      279:                     0                       0                       0
      280:                     0                       0                       0
      281:                     0                       0                       0
      282:                     0                       0                       0
      283:                     0                       0                       0
           wadrc_c2_boc_affect___3 wadrc_c2_boc_affect___4 wadrc_c2_boc_affect___5
                             <num>                   <num>                   <num>
        1:                       0                       0                       0
        2:                       0                       0                       0
        3:                       0                       0                       0
        4:                       0                       0                       0
        5:                       0                       0                       0
       ---                                                                        
      279:                       0                       0                       0
      280:                       0                       0                       0
      281:                       0                       0                       0
      282:                       0                       0                       0
      283:                       0                       0                       0
           wadrc_c2_boc_attitude___1 wadrc_c2_boc_attitude___2
                               <num>                     <num>
        1:                         0                         0
        2:                         0                         0
        3:                         0                         0
        4:                         0                         0
        5:                         0                         0
       ---                                                    
      279:                         0                         0
      280:                         0                         0
      281:                         0                         0
      282:                         0                         0
      283:                         0                         0
           wadrc_c2_boc_attitude___3 wadrc_c2_boc_attitude___4
                               <num>                     <num>
        1:                         0                         0
        2:                         0                         0
        3:                         0                         0
        4:                         0                         0
        5:                         0                         0
       ---                                                    
      279:                         0                         0
      280:                         0                         0
      281:                         0                         0
      282:                         0                         0
      283:                         0                         0
           wadrc_c2_boc_attitude___5 wadrc_c2_boc_language___1
                               <num>                     <num>
        1:                         0                         0
        2:                         0                         0
        3:                         0                         0
        4:                         0                         0
        5:                         0                         0
       ---                                                    
      279:                         0                         0
      280:                         0                         0
      281:                         0                         0
      282:                         0                         0
      283:                         0                         0
           wadrc_c2_boc_language___2 wadrc_c2_boc_language___3
                               <num>                     <num>
        1:                         0                         0
        2:                         0                         0
        3:                         0                         0
        4:                         0                         0
        5:                         0                         0
       ---                                                    
      279:                         0                         0
      280:                         0                         0
      281:                         0                         0
      282:                         0                         0
      283:                         0                         0
           wadrc_c2_boc_language___4 wadrc_c2_boc_language___5
                               <num>                     <num>
        1:                         0                         0
        2:                         0                         0
        3:                         0                         0
        4:                         0                         0
        5:                         0                         0
       ---                                                    
      279:                         0                         0
      280:                         0                         0
      281:                         0                         0
      282:                         0                         0
      283:                         0                         0
           wadrc_c2_boc_snsry_fncn___0 wadrc_c2_boc_snsry_fncn___1
                                 <num>                       <num>
        1:                           0                           0
        2:                           0                           0
        3:                           0                           0
        4:                           0                           0
        5:                           0                           0
       ---                                                        
      279:                           0                           0
      280:                           0                           0
      281:                           0                           0
      282:                           0                           0
      283:                           0                           0
           wadrc_c2_boc_snsry_fncn___2 wadrc_c2_boc_snsry_fncn___3
                                 <num>                       <num>
        1:                           0                           0
        2:                           0                           0
        3:                           0                           0
        4:                           0                           0
        5:                           0                           0
       ---                                                        
      279:                           0                           0
      280:                           0                           0
      281:                           0                           0
      282:                           0                           0
      283:                           0                           0
           wadrc_c2_boc_snsry_fncn___4 wadrc_c2_boc_snsry_fncn___5
                                 <num>                       <num>
        1:                           0                           0
        2:                           0                           0
        3:                           0                           0
        4:                           0                           0
        5:                           0                           0
       ---                                                        
      279:                           0                           0
      280:                           0                           0
      281:                           0                           0
      282:                           0                           0
      283:                           0                           0
           wadrc_c2_boc_comprhnsn___1 wadrc_c2_boc_comprhnsn___2
                                <num>                      <num>
        1:                          0                          0
        2:                          0                          0
        3:                          0                          0
        4:                          0                          0
        5:                          0                          0
       ---                                                      
      279:                          0                          0
      280:                          0                          0
      281:                          0                          0
      282:                          0                          0
      283:                          0                          0
           wadrc_c2_boc_comprhnsn___3 wadrc_c2_boc_battery wadrc_c2_boc_notes respval
                                <num>                <num>             <char>   <num>
        1:                          0                   NA               <NA>       3
        2:                          0                   NA               <NA>       3
        3:                          0                   NA               <NA>       1
        4:                          0                   NA               <NA>       1
        5:                          0                   NA               <NA>      NA
       ---                                                                           
      279:                          0                   NA               <NA>      NA
      280:                          0                   NA               <NA>       3
      281:                          0                   NA               <NA>       1
      282:                          0                   NA               <NA>       1
      283:                          0                   NA               <NA>       1
           loc_res___1 loc_res___2 loc_res___3 loc_res___4 loc_res___5 loc_res___6
                 <num>       <num>       <num>       <num>       <num>       <num>
        1:           0           0           0           0           0           0
        2:           0           0           0           0           0           0
        3:           0           0           0           0           0           0
        4:           0           0           0           0           0           0
        5:           0           0           0           0           0           0
       ---                                                                        
      279:           0           0           0           0           0           0
      280:           0           0           0           0           0           0
      281:           0           0           0           0           0           0
      282:           0           0           0           0           0           0
      283:           0           0           0           0           0           0
           loc_res___7 loc_res___8                  respothx
                 <num>       <num>                    <char>
        1:           0           0 Test was not administered
        2:           0           0                      <NA>
        3:           0           0 Test was not administered
        4:           0           0                      <NA>
        5:           0           0                      <NA>
       ---                                                  
      279:           0           1                      <NA>
      280:           0           0                      <NA>
      281:           0           0                      <NA>
      282:           0           0                      <NA>
      283:           0           0                      <NA>


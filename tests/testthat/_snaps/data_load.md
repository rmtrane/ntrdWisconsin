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


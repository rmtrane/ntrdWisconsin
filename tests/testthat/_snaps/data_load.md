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

# pull_redcap_data() prepares the frozen UDS-2 fixture

    Code
      out
    Output
            NACCID VISITYR VISITMO VISITDAY   SEX  EDUC  RACE IQCODEINFORM IQCODESELF
            <char>   <num>   <num>    <num> <num> <num> <num>        <num>      <num>
       1: sim00001    2014      10       21     1    NA    NA         3.12       2.88
       2: sim00001    2015       2       16     1    NA    NA         4.69       4.44
       3: sim00002    2012       8       23     1    NA    NA         3.00       2.88
       4: sim00003    2012       8        7     2    NA    NA         3.13       2.63
       5: sim00003    2014       5        5     2    NA    NA         2.94       4.13
       6: sim00004    2013       5       10     2    NA     1         3.00       3.00
       7: sim00005    2011       3       22     1    NA    NA         3.00       3.13
       8: sim00005    2012       4       24     1    NA    NA         3.13       3.38
       9: sim00005    2012       9       24     1    NA    NA         4.07         NA
      10: sim00005    2013       3        1     1    NA    NA         3.06       3.00
      11: sim00006    2012       1       27     1    16    NA         3.50       2.88
      12: sim00006    2012       6        5     1    16    NA         3.00       3.44
      13: sim00006    2013      12       23     1    16    NA         4.63       3.13
      14: sim00006    2015       1        7     1    16    NA         2.75       3.06
      15: sim00007    2011       7       11     1    NA     1         3.50       3.06
      16: sim00008    2014      10       27     2    NA     1         3.06       3.00
      17: sim00009    2012       6        5     1    13    NA         3.36       3.19
      18: sim00009    2014       3       31     1    13    NA         3.00       3.44
      19: sim00009    2014      12       23     1    13    NA         4.56       3.50
      20: sim00010    2012       7       13     2    NA    NA         3.25       3.19
      21: sim00010    2012       9       20     2    NA    NA           NA       3.13
      22: sim00011    2013       1        8     2    16     1         3.06       3.44
      23: sim00012    2011       6       27     1    NA     2           NA       3.00
      24: sim00013    2012       1        4     1    16    NA         3.69       3.38
      25: sim00013    2013       6        7     1    16    NA         3.93       3.00
      26: sim00013    2013       8        8     1    16    NA         3.25       3.00
      27: sim00013    2013       9       20     1    16    NA         3.44       2.94
      28: sim00013    2014       7        1     1    16    NA         3.06       3.19
      29: sim00014    2010       4        8     1    NA    NA         3.13       3.00
      30: sim00014    2013       9       18     1    NA    NA         3.00       2.56
      31: sim00014    2014       6        5     1    NA    NA         3.00       3.19
      32: sim00014    2014       8       22     1    NA    NA         2.94       3.19
      33: sim00014    2014      11       17     1    NA    NA         3.00       3.56
      34: sim00015    2012       7       12     2    NA    NA         3.00       3.56
      35: sim00015    2014       1        9     2    NA    NA         3.10       2.88
      36: sim00016    2014       4       29     1    NA    NA         3.10       3.31
      37: sim00017    2012      11       16     2    14    NA         3.13       3.25
      38: sim00018    2011       4        5     1    16    NA         3.00         NA
      39: sim00018    2011       8       22     1    16    NA         3.19       4.25
      40: sim00018    2012      12       17     1    16    NA         3.00       3.38
      41: sim00019    2011       8       16     2    10    NA         2.81       3.00
      42: sim00020    2011       9       12     2    NA    NA         3.00       3.06
            NACCID VISITYR VISITMO VISITDAY   SEX  EDUC  RACE IQCODEINFORM IQCODESELF
            <char>   <num>   <num>    <num> <num> <num> <num>        <num>      <num>
          HANDED CDRGLOB TRAILA TRAILARR TRAILALI  WAIS ANIMALS   VEG REY1REC REY2REC
           <num>   <num>  <num>    <num>    <num> <num>   <num> <num>   <num>   <num>
       1:     NA     0.0     NA        0       24    64      22    97      NA      NA
       2:     NA     0.0     23        2       24    71       6    17       5       6
       3:      2     0.0     41       NA       24    52      20    16      13       2
       4:      2     1.0     16        2       24    54      18    NA       8      NA
       5:      2     0.0     28        1       24    65      31    12       5       7
       6:     NA     0.0    997       -4       24    43      NA    13       5       7
       7:     NA     0.0     NA        0       24    16      17    17       2      11
       8:     NA     1.0     19        1       24    54      22    14       2       3
       9:     NA     0.5     19       NA       24    37      39    15       3      11
      10:     NA     0.0     38        0       24    50      97    21       7       7
      11:      1     0.5     19        2       24    NA      13    15       6       5
      12:      1     0.0     28        0       24    71      97    21      10      NA
      13:      1     0.5     29        0       24    43      14    16       5       3
      14:      1     1.0     15        0       24    96      24    14       8       6
      15:     NA     0.0     37        0       24    27      17    18       3       8
      16:     NA     1.0    997        0       24    13      28     9       4       7
      17:     NA     0.5    997        0       -4    63      18    23       4       3
      18:     NA     0.0     54        0       24    80      24    16      11       6
      19:     NA     0.5     29        0       -4    76      25    12       4       1
      20:     NA     0.0     23        0       24    51      23     7       7      NA
      21:     NA     0.0     34        0       24    36      23     5       9       7
      22:     NA     0.0     21        0       24    26      13    97       6      11
      23:      2     0.0     23        1       24    57      16     5      NA      12
      24:     NA     0.0     30        0       -4    48      97     8       6      11
      25:     NA     0.5     27        0       24    54       9    28       8       4
      26:     NA     0.0     23        0       18    54      20    20      NA      11
      27:     NA     0.0     22        0       NA    57      18     2       7       9
      28:     NA     0.0     21        0       24    19      NA    97       5       9
      29:      2     0.0     26        0       24    55      30    14       6       3
      30:      2     0.0     67        0       -4    38      18    19       0       7
      31:      2     0.0     20        0       24    29      19    97       8       2
      32:      2     0.0     NA        0       24    75      28    18       4      13
      33:      2     0.0     32       NA       24    49      21    13       6      12
      34:     NA     0.5     16        0       24    68      36    16       8      11
      35:     NA     0.0     17        0       24    98      40     4      NA       2
      36:      2     0.5     56        0       24    29      15     5       4      11
      37:     NA     0.0     87        0       24    49      24    10       7       4
      38:     NA     0.0     NA        1       24    31      28     9       5      10
      39:     NA     0.0     13        0       24    35      33     7      10       9
      40:     NA     0.0     14        1       24    74      21     8      NA       6
      41:     NA     0.0     25        1       24    97      21    16       2      11
      42:      2     1.0     NA        0       24    97      24    18       4      12
          HANDED CDRGLOB TRAILA TRAILARR TRAILALI  WAIS ANIMALS   VEG REY1REC REY2REC
           <num>   <num>  <num>    <num>    <num> <num>   <num> <num>   <num>   <num>
          REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC TRAILB TRAILBRR TRAILBLI
            <num>   <num>   <num>    <num>   <num>   <num>  <num>    <num>    <num>
       1:      12       7      13        1      15       1     72       -4       -4
       2:       5      13       9        5       8      14    254       -4       24
       3:      13       2      13       NA      10       9    997        0       24
       4:      11      14      13        8       0       9     88        0       24
       5:       9       6      NA       NA      NA      15     65        0       24
       6:      15       8       9        8       7       0     40        3       24
       7:      10      13      13        5      11      15     49       -4       -4
       8:       9      15      13        5      12      11     48       NA       24
       9:       7       9      11        4       4       8    130       -4       24
      10:      NA      11      NA       NA      15      11     60        1       24
      11:      15      11       1        6       2       0     98        0       24
      12:      14       1       5        6      NA       8     46        0       24
      13:      14      15      11        7       3      NA     88        3       24
      14:      15      14      14        2       5      11     36        0       24
      15:       9       4      14        6       6      15     38        2       24
      16:      12      10      13       NA       8      NA    998        8       24
      17:      NA      15       9       12      13      NA     57       -4       24
      18:      13      12       8        3       2      10    997        0       17
      19:       4       4      13        5      15      NA    996        0       -4
      20:       4      11       8        4      12       0    118        0       NA
      21:      15      10      14        5       2       9     38        0       NA
      22:      15      11       6        0      13       9     60        5       24
      23:      NA      12       8        7       0       3    108        3       24
      24:       6       4      12        3       8      15     57        0       24
      25:       6      NA       3        4       9      14     67       NA       NA
      26:      NA      10      13        5       9      13     NA        1       -4
      27:      13      13       9        2       8       8     45        0       -4
      28:      14       6      10        4       5      NA     51        0       24
      29:       7       9       9        7      15       9     NA        0       24
      30:       5      11      14        3       1      12    126        2       24
      31:      14      NA      12        4       6      12    126        0       24
      32:      11      13      15        6      12       7     50        1       24
      33:      11       1      NA        0       7       3    997        2       24
      34:      10      11      NA       NA      12       0     49        0       24
      35:       3      13      12        9      10       8     41        0       24
      36:      13      12      14        3       5       9     74        0       24
      37:       9       7      14        6      10      13    117        0       24
      38:      10       9       3        4      14       9     57        0       -4
      39:      11      10      11        6      13       2     81        2       24
      40:       9       9       9        4      10       1    997        1       17
      41:       7      11       9        4       8       4     40        3       24
      42:       6      13      15        4      15      14     76       NA       24
          REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC TRAILB TRAILBRR TRAILBLI
            <num>   <num>   <num>    <num>   <num>   <num>  <num>    <num>    <num>
          NACCGDS CDRSUM NACCMMSE BOSTON LOGIMEM MEMUNITS DIGIF DIGIFLEN DIGIB
            <num>  <num>    <num>  <num>   <num>    <num> <num>    <num> <num>
       1:       1    0.0       26     28      10       NA     6        8    11
       2:       1    0.0       NA     28      16        1     5        5     6
       3:       0    0.0       22     29      17       18     7        8    NA
       4:       1    0.0       29     30      NA       16     7        6    97
       5:       0    0.0       NA     24      19       16    97        7    NA
       6:       3    4.0       30     29       0       16     5        8     5
       7:       2    0.0       23     30       6       12     7       NA     3
       8:       0    8.0       29     20       5       15    12       97     7
       9:       3    0.0       NA     27       0       NA     8        5     4
      10:       0    4.5       30     NA       0       19     9        5     8
      11:       1    0.0       30     29      20        9    10        8     4
      12:       0    1.5       29     NA       8       16     7        6     7
      13:       1    0.0       30     30      NA       NA     7        8     4
      14:       1    0.0       29     97      12       13    NA        8     5
      15:       1    0.0       27     97      97       10    11        4    12
      16:       0    9.0       NA     29       2       17     5        7     5
      17:       0    0.0       30     28      13       10     7        7     4
      18:       0    0.0       29     22       5       97     9        6     8
      19:       0   10.0       30     NA       0       14     4        8     9
      20:       0    0.0       19      6      14       12     6        7     7
      21:       3   13.0       NA     97      12        3     4        6     6
      22:       0    0.0       23     29      16        8     5        5     6
      23:       0    7.0       30     16      14        7     7        8     4
      24:       0    0.5       28     29      15        9    NA        7    NA
      25:       3    0.0       30     30       4        8     8        5    NA
      26:       0    0.0       NA     30       8        6     8        8     4
      27:       4    0.0       30     NA       0       15     8        3     5
      28:       0    0.0       29     29       5       16    NA        8     5
      29:       1    3.5       28     30       0        4    NA        4     6
      30:       1    0.5       27     97      17       13    11        6    NA
      31:       1    1.0       30     28       6        0     8        6     7
      32:       0    0.0       NA     27      16       15    11        5     8
      33:       0    0.0       30     97       7       15    11        6     6
      34:       0    0.0       24     30       9        1    10        6     5
      35:       1    0.0       30     30      12       NA     7        7     9
      36:       0    0.0       30     25      NA       11     7        6    NA
      37:      NA    5.5       27     29       5       12     6        8     2
      38:       2    5.0       29     28      13        7     3        7     6
      39:      NA    0.0       30     30      13       15     7        7     8
      40:      NA   10.0       13     28      15       11     7        8    97
      41:       0   11.5       30     30      12        1     7        5     6
      42:       2    5.0       29     29      13       14     9        5    10
          NACCGDS CDRSUM NACCMMSE BOSTON LOGIMEM MEMUNITS DIGIF DIGIFLEN DIGIB
            <num>  <num>    <num>  <num>   <num>    <num> <num>    <num> <num>
          DIGIBLEN MEMTIME BILLS TAXES SHOPPING GAMES STOVE MEALPREP EVENTS PAYATTN
             <num>   <num> <num> <num>    <num> <num> <num>    <num>  <num>   <num>
       1:        4      20     0     0        0     0     0        3      0       0
       2:        4      18     3     0        3     0     0        0      1       0
       3:        5      20     0     3        0     0     0        0      3       2
       4:        4      18     0     3        0     0     0        0      0       0
       5:        5      20     0     3        0     0     0        8      0       1
       6:        7      22     0     8        0     0     0        3      1       1
       7:        6      20     3     8        0     0     2        0      0       0
       8:        4      20     0     0        0     0     2        8      0       0
       9:        5      19     0     0        0     0     0        3      3       0
      10:        4      18     0     0        0     0     0        2      0       0
      11:        6      21     2     0        0     0     0        8      3       0
      12:       NA      20     0     2        0     0     0        0      0       0
      13:        5      22     0     0        0     0     0        8      0       0
      14:        7      20     1     2        0     0     0        0      0       0
      15:        3      17     8     0        0     0     3        0      2       1
      16:        6      21     0     0        0     0     3        0      2       0
      17:       97      15     3     2        3     0     0        0      0       0
      18:        5      30     0     3        0     0     0        8      0       2
      19:        6      17     0     0        0     8     0        0      0       0
      20:        5      19     0     0        0     0     0        0      3       0
      21:        7      18     3     1        0     0     0        0      1       0
      22:        5      20     3     0        0     0     0        1      0       3
      23:       NA      16     8     2        0     0     2        0      0       0
      24:        5      20     0     3        8     0     0        0      0       0
      25:        4      21     3     8        0     0     0        0      0       0
      26:        4      16     3     0        0     0     0        2      3       0
      27:        3      19     8     3        1     8     1        0      0       1
      28:        4      18     0     0        0     2     0        0      0       0
      29:        6      -4     0     1        0     0     2        0      0       0
      30:        2      16     0     0        0     0     0        1      0       0
      31:        5      20     0     0        2     0     0        8      0       1
      32:        4      20     0     0        0     0     0        0      0       0
      33:        2      19     1     2        0     0     0        0      0       1
      34:        3      20     0     0        0     3     0        0      0       0
      35:        3      13     0     3        0     0     0        0      0       0
      36:        3      20     0     0        0     0     0        0      0       0
      37:        4      23     0     3        0     0     0        0      0       0
      38:        6      18     0     0        0     0     0        0      1       1
      39:        6      -4     0     8        0     8     0        0      0       0
      40:       97      17     3     3        0     0     0        0      0       0
      41:        6      15     0     0        0     0     0        0      1       0
      42:        6      20     0     0        0     0     0        0      0       0
          DIGIBLEN MEMTIME BILLS TAXES SHOPPING GAMES STOVE MEALPREP EVENTS PAYATTN
             <num>   <num> <num> <num>    <num> <num> <num>    <num>  <num>   <num>
          REMDATES TRAVEL BIRTHYR BIRTHMO ALCDEM ANXIET BIPOLDX BRNINJ COGOTH COGOTH2
             <num>  <num>   <num>   <num>  <num>  <num>   <num>  <num>  <num>   <num>
       1:        2      0    1924       9     NA     NA      NA      0      0       0
       2:        0      3    1924       9      0     NA      NA      0      0       0
       3:        0      1    1926      11     NA     NA      NA      0      0       0
       4:        0      0    1938       6      0     NA      NA      0      0       0
       5:        0      0    1938       6     NA     NA      NA      0      0       0
       6:        2      0    1940       1     NA     NA      NA      0      0       0
       7:        1      0    1967      11     NA     NA      NA      0      0       0
       8:        3      0    1967      11      0     NA      NA      0      0       0
       9:        0      0    1967      11      0     NA      NA      1      0       0
      10:        0      0    1967      11     NA     NA      NA      0      0       0
      11:        1      0    1943       8     NA     NA      NA      0      0       0
      12:        0      0    1943       8     NA     NA      NA      0      0       0
      13:        0      2    1943       8     NA     NA      NA      0      1       0
      14:        0      3    1943       8     NA     NA      NA      0      0       0
      15:        8      0    1957       9     NA     NA      NA      0      0       0
      16:        1      0    1932       1     NA     NA      NA      0      0       0
      17:        0      1    1948       1     NA     NA      NA      0      0       0
      18:        0      1    1948       1      0     NA      NA      0      0       0
      19:        0      0    1948       1      0     NA      NA      0      0       0
      20:        0      0    1926      12      0     NA      NA      0      0       0
      21:        2      0    1926      12     NA     NA      NA      0      0       0
      22:        1      3    1952      11      0     NA      NA      0      0       0
      23:        0      3    1951      11     NA     NA      NA      0      0       0
      24:        0      0    1948       5     NA     NA      NA      0      0       0
      25:        0      0    1948       5     NA     NA      NA      0      0       0
      26:        0      0    1948       5     NA     NA      NA      0      0       0
      27:        0      1    1948       5     NA     NA      NA      0      0       0
      28:        0      0    1948       5     NA     NA      NA      0      0       0
      29:        0      0    1936      10     NA     NA      NA      0      1       0
      30:        3      1    1936      10     NA     NA      NA      0      0       0
      31:        0      0    1936      10     NA     NA      NA      0      0       0
      32:        0      0    1936      10     NA     NA      NA      0      0       0
      33:        3      0    1936      10     NA     NA      NA      0      0       0
      34:        0      1    1963       3     NA     NA      NA      0      0       0
      35:        2      0    1963       3      0     NA      NA      0      0       0
      36:        0      0    1938       5     NA     NA      NA      0      0       0
      37:        0      0    1930      11      0     NA      NA      0      0       0
      38:        0      0    1947       7      0     NA      NA      0      0       0
      39:        1      0    1947       7     NA     NA      NA      0      0       0
      40:        0      1    1947       7     NA     NA      NA      1      0       0
      41:        0      0    1968       6     NA     NA      NA      0      0       1
      42:        2      8    1958      10     NA     NA      NA      1      1       0
          REMDATES TRAVEL BIRTHYR BIRTHMO ALCDEM ANXIET BIPOLDX BRNINJ COGOTH COGOTH2
             <num>  <num>   <num>   <num>  <num>  <num>   <num>  <num>  <num>   <num>
          COGOTH3  CORT   CVD DELIR DEMUN   DEP DOWNS DYSILL EPILEP ESSTREM FTLDMO
            <num> <num> <num> <num> <num> <num> <num>  <num>  <num>   <num>  <num>
       1:       0     0    NA    NA     0     0     0      0     NA      NA     NA
       2:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
       3:       0     0    NA    NA     0     0     0      0     NA      NA     NA
       4:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
       5:       0     0    NA    NA     0     0     0      0     NA      NA     NA
       6:       0     0    NA    NA     0     0     0      0     NA      NA     NA
       7:       0     0    NA    NA     0     0     0      0     NA      NA     NA
       8:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
       9:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      10:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      11:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      12:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      13:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      14:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      15:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      16:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      17:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      18:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      19:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      20:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      21:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      22:       0     0    NA    NA    NA     1     0      0     NA      NA     NA
      23:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      24:       0     0    NA    NA     0     1     0      0     NA      NA     NA
      25:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      26:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      27:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      28:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      29:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      30:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      31:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      32:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      33:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      34:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      35:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      36:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      37:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      38:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      39:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      40:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
      41:       0     0    NA    NA     0     0     0      0     NA      NA     NA
      42:       0     0    NA    NA    NA     0     0      0     NA      NA     NA
          COGOTH3  CORT   CVD DELIR DEMUN   DEP DOWNS DYSILL EPILEP ESSTREM FTLDMO
            <num> <num> <num> <num> <num> <num> <num>  <num>  <num>   <num>  <num>
          FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS   MSA NACCALZD NACCLBDE  NEOP
            <num> <num> <num>  <num>  <num> <num> <num>    <num>    <num> <num>
       1:      NA    NA     0      0     NA     0    NA       NA       NA     0
       2:      NA    NA     0      0     NA     0    NA       NA       NA     0
       3:      NA    NA     0      0     NA     0    NA       NA       NA     0
       4:      NA    NA     0      0     NA     0    NA       NA       NA     0
       5:      NA    NA     0      0     NA     0    NA       NA       NA     0
       6:      NA    NA     0      0     NA     0    NA       NA       NA     0
       7:      NA    NA     0      0     NA     0    NA       NA       NA     0
       8:      NA    NA     0      0     NA     0    NA       NA       NA     0
       9:      NA    NA     0      0     NA     0    NA       NA       NA     0
      10:      NA    NA     0      0     NA     0    NA       NA       NA     0
      11:      NA    NA     0      0     NA     0    NA       NA       NA     0
      12:      NA    NA     0      0     NA     0    NA       NA       NA     0
      13:      NA    NA     0      0     NA     0    NA       NA       NA     0
      14:      NA    NA     0      0     NA     0    NA       NA       NA     0
      15:      NA    NA     0      0     NA     0    NA       NA       NA     0
      16:      NA    NA     0      0     NA     0    NA       NA       NA     0
      17:      NA    NA     0      0     NA     0    NA       NA       NA     0
      18:      NA    NA     0      0     NA     0    NA       NA       NA     0
      19:      NA    NA     0      0     NA     0    NA       NA       NA     0
      20:      NA    NA     0      0     NA     0    NA       NA       NA     0
      21:      NA    NA     0      0     NA     0    NA       NA       NA     0
      22:      NA    NA     0      0     NA     0    NA       NA       NA     0
      23:      NA    NA     0      0     NA     0    NA       NA       NA     0
      24:      NA    NA     0      0     NA     0    NA       NA       NA     0
      25:      NA    NA     0      0     NA     0    NA       NA       NA     0
      26:      NA    NA     0      0     NA     0    NA       NA       NA     0
      27:      NA    NA     0      0     NA     0    NA       NA       NA     0
      28:      NA    NA     0      0     NA     0    NA       NA       NA     0
      29:      NA    NA     0      0     NA     0    NA       NA       NA     0
      30:      NA    NA     0      0     NA     0    NA       NA       NA     0
      31:      NA    NA     0      0     NA     0    NA       NA       NA     0
      32:      NA    NA     0      0     NA     0    NA       NA       NA     0
      33:      NA    NA     0      0     NA     0    NA       NA       NA     0
      34:      NA    NA     0      0     NA     0    NA       NA       NA     0
      35:      NA    NA     0      0     NA     0    NA       NA       NA     0
      36:      NA    NA     0      0     NA     0    NA       NA       NA     0
      37:      NA    NA     0      0     NA     0    NA       NA       NA     0
      38:      NA    NA     0      0     NA     0    NA       NA       NA     0
      39:      NA    NA     0      0     NA     0    NA       NA       NA     0
      40:      NA    NA     0      0     NA     0    NA       NA       NA     0
      41:      NA    NA     0      0     NA     0    NA       NA       NA     0
      42:      NA    NA     0      0     NA     0    NA       NA       NA     0
          FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS   MSA NACCALZD NACCLBDE  NEOP
            <num> <num> <num>  <num>  <num> <num> <num>    <num>    <num> <num>
          OTHCOG OTHPSY POSSAD PPAPH PRION PROBAD   PSP PTSDDX SCHIZOP STROKE  VASC
           <num>  <num>  <num> <num> <num>  <num> <num>  <num>   <num>  <num> <num>
       1:     NA      0     NA     0     0     NA     0     NA      NA      0    NA
       2:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
       3:     NA      0     NA    NA     0      1     0     NA      NA      0    NA
       4:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
       5:     NA      0     NA    NA     0      1     0     NA      NA      0    NA
       6:     NA      0     NA     0     0      1     0     NA      NA      0    NA
       7:     NA      0     NA     0     0      1     0     NA      NA      0    NA
       8:     NA      0     NA    NA     0     NA     0     NA      NA      0     0
       9:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      10:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      11:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      12:     NA      0     NA     0     0     NA     0     NA      NA      0    NA
      13:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      14:     NA      0     NA    NA     0      1     0     NA      NA      0    NA
      15:     NA      0     NA     0     0     NA     0     NA      NA      0    NA
      16:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      17:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      18:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      19:     NA      0     NA    NA     0     NA     0     NA      NA      0     0
      20:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      21:     NA      0     NA    NA     0      0     0     NA      NA      0    NA
      22:     NA      0     NA     0     0     NA     0     NA      NA      0     0
      23:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      24:     NA      0     NA    NA     0      1     0     NA      NA      0    NA
      25:     NA      0     NA     0     0     NA     0     NA      NA      0    NA
      26:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      27:     NA      0     NA    NA     0     NA     0     NA      NA      0     0
      28:     NA      0     NA     0     0     NA     0     NA      NA      0    NA
      29:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      30:     NA      0     NA    NA     0      1     0     NA      NA      0     0
      31:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      32:     NA      0     NA     0     0     NA     0     NA      NA      0    NA
      33:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      34:     NA      0     NA     0     0     NA     0     NA      NA      0     0
      35:     NA      0     NA     0     0      1     0     NA      NA      0    NA
      36:     NA      0     NA     0     0     NA     0     NA      NA      0     0
      37:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      38:     NA      0     NA     0     0     NA     0     NA      NA      0    NA
      39:     NA      0     NA    NA     0     NA     0     NA      NA      0    NA
      40:     NA      0     NA     0     0     NA     0     NA      NA      0    NA
      41:     NA      0     NA    NA     0     NA     0     NA      NA      0     0
      42:     NA      0     NA    NA     0      1     0     NA      NA      0    NA
          OTHCOG OTHPSY POSSAD PPAPH PRION PROBAD   PSP PTSDDX SCHIZOP STROKE  VASC
           <num>  <num>  <num> <num> <num>  <num> <num>  <num>   <num>  <num> <num>
          VASCPS ALCDEMIF ANXIETIF BIPOLDIF BRNINJIF COGOTHIF COGOTH2F COGOTH3F
           <num>    <num>    <num>    <num>    <num>    <num>    <num>    <num>
       1:     NA       NA       NA       NA       NA       NA       NA       NA
       2:     NA       NA       NA       NA       NA       NA       NA       NA
       3:     NA       NA       NA       NA       NA       NA       NA       NA
       4:     NA       NA       NA       NA       NA       NA       NA       NA
       5:      0       NA       NA       NA       NA       NA       NA       NA
       6:     NA       NA       NA       NA       NA       NA       NA       NA
       7:     NA       NA       NA       NA       NA       NA       NA       NA
       8:     NA       NA       NA       NA       NA       NA       NA       NA
       9:      0       NA       NA       NA       NA       NA       NA       NA
      10:     NA       NA       NA       NA       NA       NA       NA       NA
      11:     NA       NA       NA       NA       NA       NA       NA       NA
      12:     NA       NA       NA       NA       NA       NA       NA       NA
      13:     NA       NA       NA       NA       NA       NA       NA       NA
      14:     NA       NA       NA       NA       NA       NA       NA       NA
      15:     NA       NA       NA       NA       NA       NA       NA       NA
      16:      0       NA       NA       NA       NA       NA       NA       NA
      17:     NA       NA       NA       NA       NA       NA       NA       NA
      18:     NA       NA       NA       NA       NA       NA       NA       NA
      19:     NA       NA       NA       NA       NA       NA       NA       NA
      20:     NA       NA       NA       NA       NA       NA       NA       NA
      21:      0       NA       NA       NA       NA       NA       NA       NA
      22:      0       NA       NA       NA       NA        1       NA       NA
      23:      0       NA       NA       NA       NA       NA       NA       NA
      24:     NA       NA       NA       NA       NA       NA       NA       NA
      25:     NA       NA       NA       NA       NA       NA       NA       NA
      26:     NA       NA       NA       NA       NA       NA       NA       NA
      27:     NA       NA       NA       NA       NA       NA       NA       NA
      28:     NA       NA       NA       NA       NA       NA       NA       NA
      29:      0       NA       NA       NA       NA       NA       NA       NA
      30:     NA       NA       NA       NA       NA       NA       NA       NA
      31:     NA       NA       NA       NA       NA       NA       NA       NA
      32:     NA       NA       NA       NA       NA       NA       NA       NA
      33:     NA       NA       NA       NA       NA       NA       NA       NA
      34:      0       NA       NA       NA       NA       NA       NA       NA
      35:     NA       NA       NA       NA       NA       NA       NA       NA
      36:     NA       NA       NA       NA       NA       NA       NA       NA
      37:     NA       NA       NA       NA       NA       NA       NA       NA
      38:      0       NA       NA       NA       NA       NA        3       NA
      39:     NA       NA       NA       NA       NA       NA       NA       NA
      40:     NA       NA       NA       NA       NA       NA       NA       NA
      41:     NA       NA       NA       NA       NA       NA       NA       NA
      42:     NA       NA       NA       NA       NA       NA       NA       NA
          VASCPS ALCDEMIF ANXIETIF BIPOLDIF BRNINJIF COGOTHIF COGOTH2F COGOTH3F
           <num>    <num>    <num>    <num>    <num>    <num>    <num>    <num>
          CORTIF CVDIF DELIRIF DEMUNIF DEPIF DOWNSIF DYSILLIF EPILEPIF ESSTREIF
           <num> <num>   <num>   <num> <num>   <num>    <num>    <num>    <num>
       1:     NA    NA      NA      NA    NA      NA       NA       NA       NA
       2:     NA    NA      NA      NA    NA      NA       NA       NA       NA
       3:     NA    NA      NA      NA    NA      NA       NA       NA       NA
       4:     NA    NA      NA      NA    NA      NA       NA       NA       NA
       5:     NA    NA      NA      NA    NA      NA       NA       NA       NA
       6:     NA    NA      NA      NA    NA      NA       NA       NA       NA
       7:     NA    NA      NA      NA    NA      NA       NA       NA       NA
       8:     NA    NA      NA      NA    NA      NA       NA       NA       NA
       9:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      10:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      11:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      12:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      13:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      14:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      15:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      16:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      17:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      18:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      19:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      20:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      21:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      22:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      23:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      24:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      25:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      26:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      27:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      28:     NA    NA      NA      NA     3      NA       NA       NA       NA
      29:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      30:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      31:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      32:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      33:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      34:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      35:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      36:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      37:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      38:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      39:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      40:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      41:     NA    NA      NA      NA    NA      NA       NA       NA       NA
      42:     NA    NA      NA      NA    NA      NA       NA       NA       NA
          CORTIF CVDIF DELIRIF DEMUNIF DEPIF DOWNSIF DYSILLIF EPILEPIF ESSTREIF
           <num> <num>   <num>   <num> <num>   <num>    <num>    <num>    <num>
          FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF IMPSUBIF MEDSIF MSAIF NACCALZP
             <num>    <num> <num>  <num>    <num>    <num>  <num> <num>    <num>
       1:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       2:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       3:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       4:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       5:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       6:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       7:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       8:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       9:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      10:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      11:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      12:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      13:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      14:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      15:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      16:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      17:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      18:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      19:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      20:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      21:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      22:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      23:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      24:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      25:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      26:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      27:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      28:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      29:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      30:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      31:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      32:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      33:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      34:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      35:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      36:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      37:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      38:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      39:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      40:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      41:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      42:       NA       NA    NA     NA       NA       NA     NA    NA       NA
          FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF IMPSUBIF MEDSIF MSAIF NACCALZP
             <num>    <num> <num>  <num>    <num>    <num>  <num> <num>    <num>
          NACCLBDP NEOPIF OTHCOGIF OTHPSYIF POSSADIF PPAPHIF PRIONIF PROBADIF PSPIF
             <num>  <num>    <num>    <num>    <num>   <num>   <num>    <num> <num>
       1:       NA     NA       NA       NA       NA      NA      NA       NA    NA
       2:       NA     NA       NA       NA       NA      NA      NA        1    NA
       3:       NA     NA       NA       NA       NA      NA      NA       NA    NA
       4:       NA     NA       NA       NA       NA      NA      NA        1    NA
       5:       NA     NA       NA       NA       NA      NA      NA        1    NA
       6:       NA     NA       NA       NA       NA      NA      NA       NA    NA
       7:       NA     NA       NA       NA       NA      NA      NA       NA    NA
       8:       NA     NA       NA       NA       NA      NA      NA       NA    NA
       9:       NA     NA       NA       NA       NA      NA      NA        1    NA
      10:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      11:       NA     NA       NA       NA       NA      NA      NA        1    NA
      12:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      13:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      14:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      15:       NA     NA       NA       NA       NA      NA      NA        1    NA
      16:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      17:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      18:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      19:       NA     NA       NA       NA       NA      NA      NA        1    NA
      20:       NA     NA       NA       NA       NA      NA      NA        1    NA
      21:       NA     NA       NA       NA       NA      NA      NA        1    NA
      22:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      23:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      24:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      25:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      26:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      27:       NA     NA       NA       NA       NA      NA      NA        1    NA
      28:       NA     NA       NA       NA       NA      NA      NA        1    NA
      29:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      30:       NA     NA       NA       NA       NA      NA      NA        1    NA
      31:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      32:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      33:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      34:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      35:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      36:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      37:       NA     NA       NA       NA       NA      NA      NA        1    NA
      38:       NA     NA       NA       NA       NA      NA      NA        1    NA
      39:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      40:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      41:       NA     NA       NA       NA       NA      NA      NA       NA    NA
      42:       NA     NA       NA       NA       NA      NA      NA        1    NA
          NACCLBDP NEOPIF OTHCOGIF OTHPSYIF POSSADIF PPAPHIF PRIONIF PROBADIF PSPIF
             <num>  <num>    <num>    <num>    <num>   <num>   <num>    <num> <num>
          SCHIZOIF STROKIF VASCIF VASCPSIF COGOTHX COGOTH2X COGOTH3X OTHCOGX OTHPSYX
             <num>   <num>  <num>    <num>  <char>   <char>   <char>   <num>   <num>
       1:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
       2:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
       3:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
       4:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
       5:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
       6:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
       7:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
       8:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
       9:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      10:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      11:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      12:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      13:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      14:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      15:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      16:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      17:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      18:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      19:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      20:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      21:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      22:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      23:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      24:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      25:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      26:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      27:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      28:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      29:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      30:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      31:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      32:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      33:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      34:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      35:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      36:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      37:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      38:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      39:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      40:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      41:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
      42:       NA      NA     NA       NA    <NA>     <NA>     <NA>      NA      NA
          SCHIZOIF STROKIF VASCIF VASCPSIF COGOTHX COGOTH2X COGOTH3X OTHCOGX OTHPSYX
             <num>   <num>  <num>    <num>  <char>   <char>   <char>   <num>   <num>
          CESDTOTAL
              <num>
       1:         6
       2:         1
       3:         7
       4:        NA
       5:        NA
       6:        NA
       7:         5
       8:         3
       9:        27
      10:         6
      11:        NA
      12:        NA
      13:         0
      14:        NA
      15:        NA
      16:        NA
      17:        13
      18:        NA
      19:         2
      20:         1
      21:        NA
      22:        36
      23:        NA
      24:         2
      25:         1
      26:        NA
      27:        18
      28:         1
      29:         6
      30:         5
      31:        NA
      32:        NA
      33:         1
      34:        NA
      35:        NA
      36:         4
      37:        NA
      38:         8
      39:        NA
      40:        NA
      41:         2
      42:        NA
          CESDTOTAL
              <num>

# pull_redcap_data() prepares the frozen UDS-3 fixture

    Code
      out
    Output
            NACCID   SEX  EDUC  RACE IQCODEINFORM IQCODESELF HANDED CDRGLOB MOCATOTS
            <char> <num> <num> <num>        <num>      <num>  <num>   <num>    <num>
       1: sim00001     2    NA    NA         3.69     3.0600     NA     0.0       27
       2: sim00001     2    NA    NA           NA     3.0000     NA      NA       30
       3: sim00001     2    NA    NA         3.00         NA     NA      NA       24
       4: sim00001     2    NA    NA           NA         NA     NA      NA       29
       5: sim00001     1    NA    NA         3.13         NA     NA     0.5       NA
       6: sim00002     2    NA     1         2.94         NA     NA      NA       NA
       7: sim00002     2    NA     1         3.19     2.9400     NA     0.0       25
       8: sim00002     2    NA     1         3.00         NA     NA      NA       29
       9: sim00002     2    NA     1           NA     3.0600     NA      NA       NA
      10: sim00002     2    NA     1           NA     3.0600     NA      NA       NA
      11: sim00002     2    NA     1         3.13         NA     NA     0.0       NA
      12: sim00002     2    NA     1         3.00         NA     NA      NA       24
      13: sim00002     2    NA     1           NA         NA     NA     0.0       NA
      14: sim00003     1    NA    NA           NA         NA     NA     0.0       23
      15: sim00003     2    NA    NA           NA     2.8800     NA      NA       28
      16: sim00003     1    NA    NA         3.19     3.1300     NA      NA       NA
      17: sim00003     1    NA    NA         3.00         NA     NA      NA       NA
      18: sim00003     1    NA    NA           NA     3.7500     NA      NA       NA
      19: sim00003     2    NA    NA           NA     3.0000     NA     0.0       NA
      20: sim00003     1    NA    NA         3.13         NA     NA     0.0       NA
      21: sim00004     1    NA    NA           NA     3.2500     NA     0.5       NA
      22: sim00004     1    NA    NA           NA         NA     NA      NA       28
      23: sim00004     1    NA    NA         1.18     3.1300     NA      NA       NA
      24: sim00004     1    NA    NA         3.00         NA     NA     0.5       NA
      25: sim00004     1    NA    NA           NA         NA     NA     0.0       NA
      26: sim00004     1    NA    NA         3.19         NA     NA      NA       NA
      27: sim00004     1    NA    NA         3.00         NA     NA     0.0       20
      28: sim00005     2    NA    NA           NA     3.4375     NA      NA       27
      29: sim00007     2    NA    NA           NA     2.8100     NA      NA       30
      30: sim00008    NA    NA    NA           NA         NA     NA      NA       NA
      31: sim00008    NA    NA    NA           NA         NA     NA     0.5       NA
      32: sim00008    NA    NA    NA           NA         NA     NA      NA       NA
      33: sim00008    NA    NA    NA         3.00         NA     NA     0.0       NA
      34: sim00008    NA    NA    NA           NA         NA     NA     0.0       28
      35: sim00009    NA    NA    NA           NA     3.0600      2     0.0       NA
      36: sim00010     1    NA    NA         3.25         NA     NA      NA       NA
      37: sim00010     2    NA    NA         3.00         NA     NA      NA       NA
      38: sim00010     2    NA    NA           NA     3.0000     NA      NA       NA
      39: sim00010     2    NA    NA         2.81         NA     NA     0.0       NA
      40: sim00010     2    NA    NA         3.00     3.0000     NA     0.0       26
      41: sim00010     2    NA    NA           NA         NA     NA      NA       NA
      42: sim00010     2    NA    NA         3.00         NA     NA     0.5       NA
      43: sim00010     2    NA    NA           NA     3.8100     NA      NA       29
      44: sim00010     2    NA    NA           NA         NA     NA     0.0       29
      45: sim00010     2    NA    NA           NA         NA     NA      NA       23
      46: sim00010     2    NA    NA         2.63         NA     NA     0.0       25
      47: sim00010     2    NA    NA           NA         NA     NA     0.0       NA
      48: sim00010     2    NA    NA           NA         NA     NA     0.0       NA
      49: sim00010     2    NA    NA         3.00         NA     NA      NA       NA
      50: sim00011     2    NA    NA           NA     2.7500     NA     0.5       27
      51: sim00011     2    NA    NA         3.00     3.3800     NA      NA       28
      52: sim00011     2    NA    NA         3.00     3.0000     NA      NA       24
      53: sim00011     2    NA    NA           NA         NA     NA      NA       NA
      54: sim00011     2    NA    NA           NA         NA     NA      NA       29
      55: sim00011     2    NA    NA         3.00     3.0000     NA      NA       NA
      56: sim00011     2    NA    NA           NA     3.1900     NA     2.0       NA
      57: sim00012     2    NA    NA           NA         NA     NA      NA       NA
      58: sim00012     1    NA    NA         3.38     3.2500     NA      NA       NA
      59: sim00012     1    NA    NA         3.44     3.1200     NA     0.0       NA
      60: sim00012     1    NA    NA         3.13         NA     NA      NA       NA
      61: sim00013     1    NA    NA         3.94     2.2100     NA      NA       NA
      62: sim00013     1    NA    NA         3.06     3.3100     NA     0.0       NA
      63: sim00013     1    NA    NA         3.00         NA     NA     0.0       NA
      64: sim00013     2    NA    NA         3.00         NA     NA      NA       NA
      65: sim00013     2    NA    NA         4.75         NA     NA      NA       NA
      66: sim00014     2    NA    NA           NA         NA      2      NA       30
      67: sim00014     2    NA    NA           NA     3.1900      2     0.5       11
      68: sim00014     2    NA    NA         3.00         NA      2      NA       NA
      69: sim00014     2    NA    NA         3.00     2.9400      2      NA       22
      70: sim00014     2    NA    NA         3.13     3.0600      2     0.0       18
      71: sim00015     2    NA    NA           NA         NA     NA      NA       29
      72: sim00015     2    NA    NA         3.31     3.1300     NA      NA       NA
      73: sim00015     2    NA    NA           NA         NA     NA      NA       NA
      74: sim00015     2    NA    NA           NA     3.0000     NA      NA       NA
      75: sim00015     2    NA    NA           NA         NA     NA      NA       26
      76: sim00015     2    NA    NA         3.00     3.5600     NA      NA       NA
      77: sim00015     2    NA    NA         2.75         NA     NA      NA       30
      78: sim00015     2    NA    NA         3.00         NA     NA      NA       28
      79: sim00015     2    NA    NA           NA         NA     NA      NA       14
      80: sim00016    NA    NA    NA         3.13     3.1300     NA     3.0       29
      81: sim00016    NA    NA    NA           NA         NA     NA      NA       12
      82: sim00017     2    NA    NA         3.00         NA     NA     0.0       NA
      83: sim00017     2    NA    NA           NA         NA     NA      NA       NA
      84: sim00018     1    NA    NA         3.19     3.4400     NA     0.0       20
      85: sim00018     1    NA    NA           NA     3.1900     NA      NA       NA
      86: sim00018     2    NA    NA           NA     3.4400     NA     0.5       25
      87: sim00018     2    NA    NA           NA         NA     NA      NA       26
      88: sim00018     1    NA    NA           NA     3.2500     NA      NA       NA
      89: sim00018     2    NA    NA           NA         NA     NA     0.0       27
      90: sim00018     2    NA    NA           NA         NA     NA      NA       NA
      91: sim00019     2    NA    NA           NA         NA      1      NA       NA
            NACCID   SEX  EDUC  RACE IQCODEINFORM IQCODESELF HANDED CDRGLOB MOCATOTS
            <char> <num> <num> <num>        <num>      <num>  <num>   <num>    <num>
          MOCBTOTS TRAILA TRAILARR TRAILALI OTRAILA OTRLARR DIGFORCT DIGFORSL
             <num>  <num>    <num>    <num>   <num>   <num>    <num>    <num>
       1:       NA     NA       NA       NA      NA      NA        7       NA
       2:       17     36       NA       NA      NA      NA        8        7
       3:       NA     34       NA       24      NA      NA        7       NA
       4:       NA     NA       NA       NA      NA      NA       NA        7
       5:       NA     17        0       NA      NA      NA       NA        8
       6:       NA     32       NA       24      NA      NA       11        3
       7:       NA     NA        0       24     888      NA       NA        8
       8:       NA     43        0       24      NA      NA       NA       NA
       9:       20     26       NA       NA      NA      NA       NA       NA
      10:       NA     NA        0       NA     888      NA       11        5
      11:       NA     NA       NA       24     888      NA       NA        7
      12:       22     NA       NA       NA      NA      NA       11       NA
      13:       17     NA        0       NA      NA      NA       NA       NA
      14:       NA     NA       NA       24      NA      NA       NA        8
      15:       NA     NA       NA       NA      NA      NA       NA       NA
      16:       NA     21        1       24      NA      NA        7       NA
      17:       NA     NA        1       24      NA      NA        6        7
      18:       NA     NA        0       NA      NA      NA       NA       NA
      19:       NA     NA        0       NA      NA      NA        7        7
      20:       NA     NA       NA       24      NA      NA       11       NA
      21:       NA     79       NA       NA      NA      NA       NA       NA
      22:       NA     NA        0       NA      NA      NA        6       NA
      23:       NA     NA       NA       24      NA      NA        8        9
      24:       NA     59        0       24      NA      NA       NA       NA
      25:       NA     31        0       NA      NA      NA        7        6
      26:       NA     23        0       NA      NA      NA       NA       NA
      27:       NA     NA       NA       24      NA      NA       NA       NA
      28:       NA     NA        1       24      NA      NA       NA       NA
      29:       NA     29       NA       24     888      NA       NA       NA
      30:       NA     18       NA       NA      NA      NA        5        8
      31:       NA     NA       NA       24      NA      NA       NA       NA
      32:       NA     48       NA       NA      NA      NA       NA        6
      33:       NA     29        0       24      NA      NA        8       NA
      34:       NA     NA       NA       24      NA      NA        5        6
      35:       NA     39        0       NA      NA      NA        7       NA
      36:       NA     41        0       24      NA      NA        9        9
      37:       NA     NA        0       24      NA      NA       NA       NA
      38:       NA     18       NA       NA      NA      NA       NA        7
      39:       NA     NA       NA       24      NA      NA       NA       NA
      40:       NA     NA        1       NA      NA      NA       NA       NA
      41:       NA     20       NA       NA      NA      NA       NA        6
      42:       20     NA        0       NA      NA      NA        8       NA
      43:       NA     NA       NA       NA      NA      NA       11       NA
      44:       NA     41       NA       24      NA      NA        5       NA
      45:       NA     NA       NA       NA     888      NA       14       NA
      46:       21     NA        0       24      NA      NA       NA        5
      47:       NA     NA        0       24      NA      NA       NA        8
      48:       NA     25       NA       24      NA      NA       NA        6
      49:       NA     NA        0       24      NA      NA        6        5
      50:       NA     21        1       NA      NA      NA       NA        7
      51:       NA     NA        1       24      NA      NA       NA        5
      52:       NA     14       NA       NA      NA      NA       NA       NA
      53:       NA     NA       NA       24      NA      NA       NA        7
      54:       NA     NA       NA       NA      NA      NA       NA       NA
      55:       NA     NA        0       24      NA      NA        6        6
      56:       NA     NA        0       NA      NA      NA        9       NA
      57:       NA     NA       NA       NA      NA      NA        6       NA
      58:       NA     18       NA       24      NA      NA        9        9
      59:       NA     NA        0       24      NA      NA       NA        8
      60:       NA     14       NA       24      NA      NA       11        6
      61:       NA     NA        0       NA      NA      NA        6       NA
      62:       NA     41       NA       NA      NA      NA       10       NA
      63:       NA     NA       NA       NA      NA      NA       NA       NA
      64:       NA     NA       NA       24      NA      NA        7       NA
      65:       NA     NA       NA       24      NA      NA        9       NA
      66:       NA     22       NA       24      NA      NA        6       NA
      67:       NA     NA       NA       NA      NA      NA       NA        7
      68:       NA     NA       NA       NA      NA      NA        7       NA
      69:       NA     NA       NA       NA      NA      NA       NA        7
      70:       NA     34       NA       NA      NA      NA       NA       NA
      71:       NA     NA       NA       NA      NA      NA       NA        5
      72:       22     22       NA       NA      NA      NA        8        8
      73:       NA     16       NA       NA      NA      NA       NA       NA
      74:       NA     NA       NA       NA     888      NA       NA       NA
      75:       NA     16        0       NA      NA      NA        7        8
      76:       NA     NA       NA       NA      NA      NA       NA        7
      77:       NA     NA        0       NA      NA      NA       NA        5
      78:       NA     NA        0       NA      NA      NA       10       NA
      79:       16     17       NA       NA      NA      NA       10        5
      80:       NA     NA       NA       24      NA      NA       NA       NA
      81:       NA     NA       NA       NA      NA      NA        8        8
      82:       NA     NA       NA       24      NA      NA       NA       NA
      83:       NA     NA        0       24     888      NA       NA       NA
      84:       NA     NA       NA       NA      NA      NA       NA        6
      85:       NA     NA       NA       24      NA      NA       NA       NA
      86:       NA     33        0       NA      NA      NA        4       NA
      87:       NA     NA        0       NA      NA      NA       NA       NA
      88:       20     31       NA       24      NA      NA       NA        5
      89:       NA     NA       NA       NA      NA      NA       10        6
      90:       NA     66        0       24      NA      NA       NA        7
      91:       NA     NA       NA       NA      NA      NA        7       NA
          MOCBTOTS TRAILA TRAILARR TRAILALI OTRAILA OTRLARR DIGFORCT DIGFORSL
             <num>  <num>    <num>    <num>   <num>   <num>    <num>    <num>
          DIGBACCT DIGBACLS   WAIS MINTTOTS ANIMALS   VEG UDSVERTN UDSVERFC UDSVERLC
             <num>    <num> <char>    <num>   <num> <num>    <num>    <num>    <num>
       1:       NA        6     50       NA      23    NA       25       NA       NA
       2:       NA       NA   <NA>       30       6    NA       NA       NA       NA
       3:       NA       NA   <NA>       95      NA    12       37       23       13
       4:       NA       NA   <NA>       NA      28    NA       NA       NA        7
       5:       NA       NA   <NA>       29      NA    13       NA       NA       15
       6:        6       NA   <NA>       30      NA    NA       25       11       12
       7:        2       NA     63       32      NA    NA       NA       NA        7
       8:       NA       NA     51       NA      NA    NA       NA       NA       11
       9:        6        5     50       NA      13    NA       32        6       12
      10:       NA       NA     47       NA      NA    13       43       NA       NA
      11:       NA       NA     54       NA      25    NA       38       NA       NA
      12:       NA       NA   <NA>       NA      NA    NA       NA       NA       NA
      13:       NA        3   <NA>       32      NA    25       NA       13       NA
      14:       NA       NA     14       26      NA    NA       NA       11       NA
      15:       NA        4   <NA>       NA      21    19       32       17       NA
      16:       NA        4     64       32      26    NA       27       15       NA
      17:       NA       NA   <NA>       NA      NA    NA       28        6       19
      18:       NA        7   <NA>       28      NA    NA       27       NA        7
      19:       NA        8   <NA>       32      NA    13       NA       12       13
      20:        7        6   <NA>       NA      32    NA       NA       15       NA
      21:       11       NA     41       31      NA    NA       NA       NA       NA
      22:       11        5   <NA>       NA      97    15       NA       NA       NA
      23:       NA        8   <NA>       32      NA     9       28       96       NA
      24:       NA       NA     27       NA      NA    11       39       NA       18
      25:       NA        3   <NA>       30      13    NA       NA       13       23
      26:       NA       NA   <NA>       NA      16    NA       26        9       16
      27:        6        5   <NA>       NA      NA    16       28       NA       NA
      28:       NA        7     42       NA      NA    15       NA       NA        8
      29:        9       NA   <NA>       32      NA    NA       NA       NA       NA
      30:        5       NA   <NA>        8       4    16       NA       NA       NA
      31:       12       NA   <NA>       27      NA    NA       11       NA       NA
      32:        5        5   <NA>       NA      32    24       28       10       NA
      33:       NA        6     54       32      18    18       38       13       NA
      34:       98       NA   <NA>       NA      NA     5       NA       18       NA
      35:        4        6   <NA>       NA      NA    NA       NA       26       NA
      36:       NA       NA   <NA>       NA      18    NA       37       NA       NA
      37:        5       NA   <NA>       19      23    NA       NA       NA       NA
      38:        7       NA     50       NA       8    13       NA       15       NA
      39:        4        5     25       NA      NA    NA       NA       NA       13
      40:       NA        4   <NA>       NA      NA    NA       21       19       NA
      41:        7       NA   <NA>       NA      NA    NA       37       19       NA
      42:        4       NA   <NA>       28      24    15       25       16       NA
      43:       NA        5   <NA>       NA      NA    NA       NA       NA       17
      44:       NA       NA   <NA>       NA      NA    NA       34       19       NA
      45:       NA       NA   <NA>       31      26    19       NA       18       NA
      46:       NA       NA   <NA>       NA      NA    NA       49       NA       19
      47:       NA       NA   <NA>       30      22    12       16       NA       NA
      48:       NA        3   <NA>       NA      24     7       30       NA       NA
      49:        6        5   <NA>       NA      NA    16       NA       NA       16
      50:       NA       NA     40       NA      NA    NA       22       NA       14
      51:        5        6     55       NA      16    NA       27       NA       16
      52:       NA        4     47       NA      18     6       16       NA       NA
      53:        4        2   <NA>       31      NA    NA       17       12       14
      54:       NA       NA   <NA>       NA      NA    NA       NA       NA       NA
      55:       NA        4   <NA>       28       9    19       23       15        9
      56:       NA       NA   <NA>       NA      NA    NA       25       12       16
      57:       NA       NA   <NA>       28      NA    NA       NA       NA       12
      58:        7       NA   <NA>       29      NA    NA       NA       10       NA
      59:        5       NA   <NA>       NA      NA    NA       27       NA       12
      60:       11       NA     81       29      21    NA       NA        9       NA
      61:        7       NA   <NA>       NA      23    NA       NA       NA       NA
      62:        4       NA   <NA>       NA      19     8       35       19       NA
      63:        7        7     43       32      11    NA       23       15       17
      64:       NA       NA   <NA>       NA      NA    NA       30       NA       15
      65:        7        5   <NA>       27      24    10       31       26       17
      66:        6       NA   <NA>       NA      NA    NA       29       NA       13
      67:        6        5   <NA>       NA      NA    NA       NA       21       NA
      68:        6       NA   <NA>       29      NA    NA       NA       NA       NA
      69:        8        6   <NA>       NA      NA    NA       NA       14       NA
      70:       NA       NA   <NA>       NA      20    NA       31       NA       10
      71:       NA        2     45       30      NA    NA       NA       NA       NA
      72:       10        5   <NA>       NA      NA    23       30       NA       NA
      73:        5        7   <NA>       NA      NA     9       NA       NA       NA
      74:       NA        8   <NA>       NA      21    NA       11       NA       NA
      75:       NA       NA   <NA>       32      30    NA       NA       13        9
      76:       NA       NA   <NA>       28      NA     2       30       NA       NA
      77:       NA       NA   <NA>       NA      NA    17       28       NA       NA
      78:        8       NA   <NA>       26      19    NA       34       NA        9
      79:       NA       NA   <NA>       NA      16    13       NA       21        7
      80:       NA       NA   <NA>       NA      NA    NA       NA       NA       NA
      81:        9        6   <NA>       NA      NA    NA       29       NA       NA
      82:       NA        5   <NA>       NA      NA    NA       NA       15       NA
      83:       NA       NA   <NA>       20      NA    NA       22       NA       NA
      84:        6        3   <NA>       NA      NA    NA       27       NA       NA
      85:        8       NA   <NA>       31      10     9       NA       20       NA
      86:       NA        5   <NA>       30      NA    NA       NA       13       NA
      87:        7        5   <NA>       NA      NA    NA       33       14       14
      88:       NA        6   <NA>       NA      NA    11       28       29       13
      89:       NA       NA   <NA>       32      NA    18       30       NA       23
      90:        6        2     71       NA      22    NA       NA       NA       NA
      91:        5       NA     40       NA      NA    NA       32       NA       NA
          DIGBACCT DIGBACLS   WAIS MINTTOTS ANIMALS   VEG UDSVERTN UDSVERFC UDSVERLC
             <num>    <num> <char>    <num>   <num> <num>    <num>    <num>    <num>
          UDSBENTC UDSBENTD CRAFTVRS CRAFTURS CRAFTDVR CRAFTDRE REY1REC REY2REC
             <num>    <num>    <num>    <num>    <num>    <num>   <num>   <num>
       1:       16       NA       19       NA       NA       NA      14      NA
       2:       NA       13       29       NA        8       12      NA      NA
       3:       NA       NA       28       NA       19       17       3       6
       4:       16       NA       NA       NA       NA        1       6       6
       5:       NA       12       17       NA       NA       NA      NA      NA
       6:       11       98       NA       20       17       NA       3       6
       7:       16        5       31       NA       16       10       9      11
       8:       16       NA       NA        2       NA       22       6      NA
       9:       15       NA       27       NA       NA        9      NA      NA
      10:       NA       NA       NA       22       25       NA       3      NA
      11:       14       NA       10       16       NA       NA      NA      10
      12:       NA       NA       11       NA       NA       11      NA      NA
      13:       16       NA       NA       10       NA       NA      10      NA
      14:       16       NA       23       NA       NA       18      NA       8
      15:       16       NA       24       NA       18       NA       5      NA
      16:       NA       NA       NA       NA       NA       NA       4      14
      17:       NA       NA       27       NA       18       NA      NA       9
      18:       NA       NA       NA       NA       NA        5       2      NA
      19:       NA       16       NA       NA       NA       NA       8      NA
      20:       NA       NA       NA       21       23       NA       6      NA
      21:       NA        0       NA       NA       24       17      NA       8
      22:       16       NA       22       NA       22       NA      NA      NA
      23:       17       11       NA       NA       NA        0      NA      NA
      24:       NA        9       NA       15       NA       NA      NA      NA
      25:       NA       NA       16       23       23       20      NA      10
      26:       NA       NA       NA       18       NA       NA       8      NA
      27:       NA       13       NA       NA       NA       NA       5       3
      28:       15       NA       NA       NA       NA       NA       3      13
      29:       15       NA       35       11       19        0       6      NA
      30:       NA       14       NA       18       22       NA       4      10
      31:       17       16       NA       NA       NA        9      NA      NA
      32:       NA       NA       26       NA       15       NA      NA      13
      33:       16       NA       24       NA       20       17      NA      NA
      34:       NA       15       22       21       NA       20       4       4
      35:       17       12       NA       NA       23       15      10      NA
      36:       NA       NA       NA       24       NA       22      NA      NA
      37:       NA       NA       NA       NA       19       NA       3      NA
      38:       NA       NA       16       NA       NA       NA      NA      NA
      39:       NA       NA       NA       NA       20       NA      NA       9
      40:       14       NA       NA       15       23       NA      NA       9
      41:       NA        8       20        4       NA       NA      12       9
      42:       NA       NA       21       NA       NA       NA       7      NA
      43:       NA       NA       NA       NA       NA        9      NA       9
      44:       NA       13       NA       11       NA       19      NA      NA
      45:       NA       NA       NA       NA       NA        0       5      13
      46:       16       10       NA       NA       NA        4      NA      NA
      47:       NA       NA       NA       18       NA       NA      NA       3
      48:       NA       NA       NA       19       12       NA      NA       9
      49:       15       NA       17       NA       15       15      NA      NA
      50:       NA       NA       NA       10       NA        5       6       6
      51:       16       12       NA       17        9       NA      NA      NA
      52:       NA       NA       NA       NA       NA        9      NA      NA
      53:       NA        2       NA       11       25        8       4      NA
      54:       15       NA       NA       NA       NA       16      NA       7
      55:       16       NA       NA       NA       NA       16      NA       7
      56:       NA       13       28       18       NA       15      NA      NA
      57:       17       NA       NA       NA        2       14       6      NA
      58:       16       NA       NA       NA       23       20      NA      13
      59:       NA       16       NA       NA       NA       13       6       4
      60:       NA       12       NA       NA        0       NA      NA      12
      61:       NA       NA       NA       NA       NA       22       4      NA
      62:       16       NA       NA       21       12       NA      NA       9
      63:       NA       NA        8       NA       NA       NA      NA      NA
      64:       NA       12       NA       22       NA        9      15      10
      65:       17       NA       23       20       NA       NA       5      NA
      66:       NA       NA       18       17       NA       15      NA      NA
      67:       NA       NA       14       20        9       21       3       8
      68:       NA       NA       NA       16       24       14      NA      10
      69:       15       NA       25       NA       NA       NA      NA      NA
      70:       17        0       19       NA       NA       NA      NA      10
      71:       NA       10       NA       NA       22       NA      NA      14
      72:       16       NA       NA       NA       21       10      NA       9
      73:       16       NA       21       16       19       16      NA       8
      74:       15       16        5       NA       NA       NA      NA      NA
      75:       17       NA       24       NA        0       15       4       6
      76:       NA       NA       NA       20       19       10       5      NA
      77:       NA       14       22       NA       NA       NA      11       6
      78:       NA       NA       NA       11       NA       17      NA       6
      79:       NA       NA       24        4       26       NA      NA       9
      80:       NA       NA       NA       23       NA       NA      NA      12
      81:       NA       NA       NA       10       NA       17      NA       8
      82:       NA       NA       NA       NA       NA       NA      NA       8
      83:       NA       NA       NA        0       NA       NA       5      NA
      84:       17       10       NA       NA       18       NA       5       5
      85:       16       16       NA       NA       NA       NA      NA      NA
      86:       NA       NA       26       NA       NA        4      NA      NA
      87:       15       NA       20       12       NA       17      11       7
      88:       NA       15       25       NA       NA       NA      NA       4
      89:       NA        0       NA       NA       NA       NA      NA      NA
      90:       NA       NA       NA       19       18       12       7      NA
      91:       16       NA       25       NA       20       23       5      NA
          UDSBENTC UDSBENTD CRAFTVRS CRAFTURS CRAFTDVR CRAFTDRE REY1REC REY2REC
             <num>    <num>    <num>    <num>    <num>    <num>   <num>   <num>
          REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC REYTCOR REYTNEG TRAILB
            <num>   <num>   <num>    <num>   <num>   <num>   <num>   <num>  <num>
       1:      NA      15      NA        4       8       9      NA      13     37
       2:       7      10       9        5      NA      NA      14      15     NA
       3:       0      NA      NA       NA      12      15      15      15     59
       4:      NA      NA      NA        6       7      NA      NA      NA    300
       5:      NA      NA      NA       NA      NA       0      NA      NA     NA
       6:      NA      NA       6       NA      NA      15      NA      NA     NA
       7:      NA      15      NA        4      11      NA      NA      NA     NA
       8:      11      13      NA       NA      NA       8      15      15     90
       9:      14      12      12       NA      NA      NA      NA      NA     NA
      10:       6      15      NA       NA       5       6      NA      NA     NA
      11:      NA      NA      NA        5      NA      NA      14      NA     NA
      12:      10      NA       9       NA       0      NA      12      14     NA
      13:      15      NA      NA        7      NA      NA      NA      NA     NA
      14:       9      13      NA        4      NA      NA      15      NA     NA
      15:      14      NA      NA       NA      13       9      15      14     NA
      16:      10       8      15       NA       0      15      NA      15     NA
      17:      NA      13      NA        3      NA      13      NA      15     NA
      18:      NA      NA      NA        3      NA      NA      NA      15     40
      19:      NA       5      12       NA      NA      14      NA      NA     NA
      20:      10      11       6        4       4       9      NA      13     NA
      21:      10      11       7        5      15      13      14      15     NA
      22:      12      NA       9        5       9      NA      NA      15     NA
      23:      NA      12      NA       NA      NA      NA      13       0     40
      24:      NA      13      14       NA      12      10      15      NA    253
      25:      NA      11       6       NA      14       4      NA      NA    109
      26:      NA      NA      NA        4      13      15      NA      13     NA
      27:      10      NA      15        6      NA      NA      NA       7     NA
      28:      NA      NA      NA        6      NA      10      NA      NA     NA
      29:      NA      NA      NA        5      14      NA      NA      NA     40
      30:      NA      14      14       NA      NA       4       5      15    168
      31:      NA      10      15        6      11      NA      13      NA     NA
      32:      NA      NA      NA        6      NA      12      NA      12     NA
      33:      12      NA      NA        1      NA       6      10      NA     NA
      34:      NA      10      14        3      NA      NA      14      15     42
      35:       5      NA      15        5       3      14      NA      NA     NA
      36:       7      NA      12        6      NA      NA      15      NA     NA
      37:      NA      NA      15       NA      NA      NA      NA      NA     NA
      38:      10      14      12       NA       7      NA      NA      15     NA
      39:      12      NA      NA       NA      NA      NA      15      NA     34
      40:       9      NA      15       NA      NA      NA      NA      NA     NA
      41:      NA      14      NA       NA      12      NA      NA      14     72
      42:       9      NA      NA        6      NA      12      NA      NA    127
      43:      11      13      NA       NA      NA      NA      NA      NA     NA
      44:      NA      NA      11       NA      NA      NA      12      NA     49
      45:      NA      NA      NA       NA       9      NA      NA      NA     29
      46:       4      NA       4        5      13      NA      NA      NA     NA
      47:      NA      NA      NA       NA       1      NA      15      12     44
      48:      10      NA      NA        5      NA      NA      NA      12     NA
      49:       8      NA       7       NA       9      NA      13      14     NA
      50:      NA      97      NA       NA      NA       5      NA      NA     NA
      51:      NA      NA      NA        9       9      NA      15      15     NA
      52:      NA      NA      NA       NA      NA      NA      NA      NA     48
      53:       9      NA      NA       NA      12      NA      NA      NA     41
      54:       9      NA       6       NA       0      14       9      NA     84
      55:      12      NA      NA       NA      NA      NA      NA      15     NA
      56:       9      15      NA       NA      NA      11      15      NA     NA
      57:       9       9      NA        7      NA      15      NA      NA     NA
      58:      12      NA       3       NA      NA       7      NA      NA     NA
      59:       9      NA      NA       NA      14      10      NA      NA     24
      60:       7      13       9        9      11      NA      NA      NA     59
      61:      NA       7      NA        7      NA      NA      NA      NA     NA
      62:      NA      NA      13       NA       8      NA      13      NA     NA
      63:      12      NA      NA       NA      15      NA      NA      15     NA
      64:      NA      12      NA       NA       0      NA      NA      12     NA
      65:      NA      NA      11       NA      NA      11      NA      NA     66
      66:      NA      NA      11        3       0      NA      14      NA     88
      67:      13      14      NA       NA      NA      NA      NA      15    996
      68:      NA      NA      NA       NA      NA      NA      12      NA     NA
      69:      NA      NA      11        7      NA      NA      14      NA     42
      70:      NA      11       9        5      12      NA      NA      NA     NA
      71:      11      13      NA       NA      NA      NA      12      12     NA
      72:      NA      NA      NA        6      NA       5      NA      15     NA
      73:       2      13      NA        5      NA      NA      NA      NA     NA
      74:       7      NA      NA       NA      13       8      NA      NA     NA
      75:      NA      NA      NA        6      NA      NA      11      NA     87
      76:      11      NA      NA       NA      NA      NA      NA      NA     47
      77:      NA       6       6       NA      NA      12      NA      13    162
      78:      NA      NA      NA       NA      NA      NA      NA       8     NA
      79:       4      NA      NA        4      11       8      NA      NA     NA
      80:      NA      10      NA        2      13      NA      NA      NA     45
      81:      13      10      12       NA       9       2      NA      NA     NA
      82:      NA      15      NA        5      11      12      15      NA     NA
      83:      NA      NA      NA       NA      NA      NA      NA      13     NA
      84:      NA       5       4       NA      13      NA      NA      NA    211
      85:      11       6      13       NA       9      NA      NA       1     70
      86:      10      15      NA       NA      NA      NA      15      NA     51
      87:       7      NA      NA        2      NA      NA      NA      15     NA
      88:       8      13      NA        5       7       6      NA      13     NA
      89:      NA      12      NA       NA      NA      14      NA      NA     NA
      90:      NA      NA      NA       NA      NA      13      15      15     69
      91:      11      NA      14        2      15      NA      NA      11     53
          REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC REYTCOR REYTNEG TRAILB
            <num>   <num>   <num>    <num>   <num>   <num>   <num>   <num>  <num>
          TRAILBRR TRAILBLI MOCACLOC MOCACLOH MOCACLON OTRAILB OTRLBRR OTRLBLI
             <num>    <num>    <num>    <num>    <num>   <num>   <num>   <num>
       1:       NA       NA       NA       NA        1      NA      NA      NA
       2:        0       24        1       NA       NA      NA      NA      NA
       3:        0       24       NA       NA        1      NA      NA      NA
       4:       NA       NA       NA        0       NA      NA      NA      NA
       5:       NA       24       NA        0       NA      NA      NA      NA
       6:        0       NA       NA       NA       NA      NA      NA      NA
       7:        0       NA        1       NA       NA      NA      NA      NA
       8:        0       NA       NA        1        1      NA      NA      NA
       9:       NA       NA        1       NA       NA      NA      NA      NA
      10:        0       NA        1       NA       NA      NA      NA      NA
      11:       NA       24        1        1        1      NA      NA      NA
      12:       NA       NA        1       NA        0      NA      NA      NA
      13:        0       NA        1        1        0      NA      NA      NA
      14:        0       24       NA        0        1      NA      NA      NA
      15:       NA       NA       NA       NA       NA      NA      NA      NA
      16:        0       NA       NA        1       NA      NA      NA      NA
      17:       NA       24       NA        1       NA      NA      NA      NA
      18:       NA       24       NA       NA       NA      NA      NA      NA
      19:        0       NA       NA        1       NA      NA      NA      NA
      20:       NA       NA       NA       NA        1      NA      NA      NA
      21:       NA       NA        1       NA        1      NA      NA      NA
      22:       NA       NA       NA        1        1      NA      NA      NA
      23:       NA       24       NA        0        1      NA      NA      NA
      24:        1       NA       NA        0        1      NA      NA      NA
      25:       NA       NA        1       NA        0      NA      NA      NA
      26:        1       16        1       NA       NA     888      NA      NA
      27:        2       NA       NA       NA       NA      NA      NA      NA
      28:       NA       24       NA        0       NA      NA      NA      NA
      29:        0       NA        1       NA       NA      NA      NA      NA
      30:       NA       NA        1        1        1      NA      NA      NA
      31:       NA       NA       NA       NA       NA     888      NA      NA
      32:       NA       24        1        0       NA      NA      NA      NA
      33:       NA       NA        0       NA       NA      NA      NA      NA
      34:       NA       24       NA       NA       NA      NA      NA      NA
      35:        0       NA       NA       NA       NA      NA      NA      NA
      36:       NA       NA       NA        1       NA      NA      NA      NA
      37:        1       NA       NA       NA        1      NA      NA      NA
      38:        0       NA        1       NA       NA      NA      NA      NA
      39:       NA       NA        1        1       NA      NA      NA      NA
      40:       NA       24       NA        0       NA      NA      NA      NA
      41:       NA       NA       NA       NA        1      NA      NA      NA
      42:        2       NA        1        0        1      NA      NA      NA
      43:       NA       NA        1        1       NA      NA      NA      NA
      44:       NA       24       NA       NA       NA      NA      NA      NA
      45:        3       NA        1        1       NA      NA      NA      NA
      46:        0       24        1        1       NA      NA      NA      NA
      47:       NA       NA        1       NA       NA      NA      NA      NA
      48:       NA       NA       NA       NA        1     888      NA      NA
      49:        0       NA        1        0       NA      NA      NA      NA
      50:       NA       NA        1       NA       NA      NA      NA      NA
      51:       NA       NA       NA       NA        1      NA      NA      NA
      52:        0       NA       NA       NA       NA      NA      NA      NA
      53:        0       24       NA        1       NA      NA      NA      NA
      54:       NA       24        1       NA       NA      NA      NA      NA
      55:       NA       18        1       NA        1      NA      NA      NA
      56:       NA       NA        1       NA        1      NA      NA      NA
      57:       NA       24        1       NA       NA      NA      NA      NA
      58:       NA       NA        1        1       NA      NA      NA      NA
      59:        0       24       NA       NA        1      NA      NA      NA
      60:       NA       NA        1       NA        1      NA      NA      NA
      61:        1       NA       NA       NA       NA      NA      NA      NA
      62:        0       NA        1       NA       NA      NA      NA      NA
      63:        0       NA       NA        0       NA      NA      NA      NA
      64:       NA       NA        1        1        1      NA      NA      NA
      65:       NA       24       NA       NA       NA      NA      NA      NA
      66:       NA       24        1        1       NA      NA      NA      NA
      67:       NA       NA       NA       NA       NA      NA      NA      NA
      68:        0       NA        1        1        1      NA      NA      NA
      69:       NA       NA       NA       NA       NA      NA      NA      NA
      70:       NA       NA       NA       NA       NA     888      NA      NA
      71:        2       NA        1        0        1      NA      NA      NA
      72:        3       NA       NA        1       NA      NA      NA      NA
      73:        1       24        1        1       NA     888      NA      NA
      74:        2       NA       NA       NA       NA      NA      NA      NA
      75:        0       24       NA        1       NA      NA      NA      NA
      76:       NA       24       NA       NA        1      NA      NA      NA
      77:       NA       24       NA        1       NA      NA      NA      NA
      78:       NA       NA       NA        0       NA      NA      NA      NA
      79:        0       24       NA       NA        1      NA      NA      NA
      80:       NA       24        1       NA       NA      NA      NA      NA
      81:        2       NA        1       NA       NA      NA      NA      NA
      82:        0       24        1       NA       NA      NA      NA      NA
      83:        0       24        1        1       NA      NA      NA      NA
      84:       NA       NA       NA       NA       NA      NA      NA      NA
      85:       NA       NA       NA       NA        1      NA      NA      NA
      86:       NA       24        1        1        1      NA      NA      NA
      87:        0       NA        1       NA        1      NA      NA      NA
      88:        0       24       NA        1        1      NA      NA      NA
      89:       NA       NA       NA       NA       NA      NA      NA      NA
      90:        0       NA        1       NA       NA      NA      NA      NA
      91:       NA       24       NA        1       NA     888      NA      NA
          TRAILBRR TRAILBLI MOCACLOC MOCACLOH MOCACLON OTRAILB OTRLBRR OTRLBLI
             <num>    <num>    <num>    <num>    <num>   <num>   <num>   <num>
          NACCGDS CDRSUM UDSBENRS NACCUDSD NACCMMSE BOSTON LOGIMEM MEMUNITS DIGIF
            <num>  <num>    <num>    <num>    <num>  <num>   <num>    <num> <num>
       1:       0     NA       NA        1       NA     NA      NA       NA    NA
       2:       2    0.0       NA        3       NA     NA      NA       NA    NA
       3:       1    0.0       NA       NA       NA     NA      NA       NA    NA
       4:      NA    0.0       NA       NA       NA     NA      NA       NA    NA
       5:       0     NA        0       NA       NA     NA      NA       NA    NA
       6:       0    0.0       NA       NA       NA     NA      NA       NA    NA
       7:       1    0.0        1       NA       NA     NA      NA       NA    NA
       8:       1    0.0       NA        4       NA     NA      NA       NA    NA
       9:      NA     NA       NA       NA       NA     NA      NA       NA    NA
      10:       1     NA       NA        4       NA     NA      NA       NA    NA
      11:       0     NA       NA       NA       NA     NA      NA       NA    NA
      12:      NA    0.5        1       NA       NA     NA      NA       NA    NA
      13:       5     NA       NA       NA       NA     NA      NA       NA    NA
      14:       2     NA        1        1       NA     NA      NA       NA    NA
      15:       0    0.0       NA       NA       NA     NA      NA       NA    NA
      16:       1    1.0        1        1       NA     NA      NA       NA    NA
      17:       0    0.0        1       NA       NA     NA      NA       NA    NA
      18:       1     NA       NA        1       NA     NA      NA       NA    NA
      19:       1    3.5        1       NA       NA     NA      NA       NA    NA
      20:       0   11.0       NA        1       NA     NA      NA       NA    NA
      21:       0    1.5       NA       NA       NA     NA      NA       NA    NA
      22:       2    0.0        1        3       NA     NA      NA       NA    NA
      23:       4     NA       NA        4       NA     NA      NA       NA    NA
      24:       3    3.5        1       NA       NA     NA      NA       NA    NA
      25:       0    4.5       NA       NA       NA     NA      NA       NA    NA
      26:       1    0.0       NA       NA       NA     NA      NA       NA    NA
      27:      NA    0.5       NA        4       NA     NA      NA       NA    NA
      28:      NA    0.0       NA       NA       NA     NA      NA       NA    NA
      29:       1     NA       NA       NA       NA     NA      NA       NA    NA
      30:      NA    0.0        1        2       NA     NA      NA       NA    NA
      31:       2    1.0       NA        4       NA     NA      NA       NA    NA
      32:       1    4.0       NA       NA       NA     NA      NA       NA    NA
      33:      NA     NA        0       NA       NA     NA      NA       NA    NA
      34:      NA     NA       NA       NA       NA     NA      NA       NA    NA
      35:      NA    0.0        0        1       NA     NA      NA       NA    NA
      36:      NA     NA       NA        1       NA     NA      NA       NA    NA
      37:       4     NA        1        1       NA     NA      NA       NA    NA
      38:       1     NA       NA        2       NA     NA      NA       NA    NA
      39:       9    0.0       NA        4       NA     NA      NA       NA    NA
      40:      NA     NA        1       NA       NA     NA      NA       NA    NA
      41:       1    0.0       NA        1       NA     NA      NA       NA    NA
      42:       9    0.0        0        4       NA     NA      NA       NA    NA
      43:      11    1.0        1       NA       NA     NA      NA       NA    NA
      44:       2     NA       NA       NA       NA     NA      NA       NA    NA
      45:       0     NA        1        1       NA     NA      NA       NA    NA
      46:      NA    0.0       NA        1       NA     NA      NA       NA    NA
      47:       0     NA        1       NA       NA     NA      NA       NA    NA
      48:      NA    0.0       NA        1       NA     NA      NA       NA    NA
      49:       2    0.0       NA        1       NA     NA      NA       NA    NA
      50:      NA     NA       NA       NA       NA     NA      NA       NA    NA
      51:       2     NA        1       NA       NA     NA      NA       NA    NA
      52:      NA    0.5       NA        3       NA     NA      NA       NA    NA
      53:       0    0.0       NA       NA       NA     NA      NA       NA    NA
      54:      NA     NA        1       NA       NA     NA      NA       NA    NA
      55:       2    0.0       NA       NA       NA     NA      NA       NA    NA
      56:       0     NA       NA       NA       NA     NA      NA       NA    NA
      57:      NA     NA       NA       NA       NA     NA      NA       NA    NA
      58:       0     NA        1        2       NA     NA      NA       NA    NA
      59:       0    0.5       NA        4       NA     NA      NA       NA    NA
      60:       2     NA        1       NA       NA     NA      NA       NA    NA
      61:       5    0.0       NA        1       NA     NA      NA       NA    NA
      62:       1     NA        1        1       NA     NA      NA       NA    NA
      63:      NA    0.0       NA       NA       NA     NA      NA       NA    NA
      64:       0     NA        1       NA       NA     NA      NA       NA    NA
      65:       0    0.0        1       NA       NA     NA      NA       NA    NA
      66:       0     NA       NA       NA       NA     NA      NA       NA    NA
      67:       0     NA       NA       NA       NA     NA      NA       NA    NA
      68:      NA    0.0        1        4       NA     NA      NA       NA    NA
      69:       0     NA        1        2       NA     NA      NA       NA    NA
      70:       0    0.0       NA       NA       NA     NA      NA       NA    NA
      71:       0     NA       NA        1       NA     NA      NA       NA    NA
      72:      NA     NA       NA       NA       NA     NA      NA       NA    NA
      73:       2     NA        1        4       NA     NA      NA       NA    NA
      74:       2    0.0        1       NA       NA     NA      NA       NA    NA
      75:       0     NA       NA        1       NA     NA      NA       NA    NA
      76:       0     NA       NA        3       NA     NA      NA       NA    NA
      77:      NA     NA       NA        1       NA     NA      NA       NA    NA
      78:       1    0.5       NA       NA       NA     NA      NA       NA    NA
      79:       2     NA       NA       NA       NA     NA      NA       NA    NA
      80:       0     NA       NA        4       NA     NA      NA       NA    NA
      81:       2    3.5       NA        4       NA     NA      NA       NA    NA
      82:       0     NA       NA       NA       NA     NA      NA       NA    NA
      83:       2     NA       NA       NA       NA     NA      NA       NA    NA
      84:       0    6.0       NA       NA       NA     NA      NA       NA    NA
      85:       0     NA       NA       NA       NA     NA      NA       NA    NA
      86:       0     NA       NA       NA       NA     NA      NA       NA    NA
      87:      NA     NA        1        1       NA     NA      NA       NA    NA
      88:       2    0.0        1       NA       NA     NA      NA       NA    NA
      89:       0     NA        1        1       NA     NA      NA       NA    NA
      90:       1    0.0        1       NA       NA     NA      NA       NA    NA
      91:       0    0.0       NA        1       NA     NA      NA       NA    NA
          NACCGDS CDRSUM UDSBENRS NACCUDSD NACCMMSE BOSTON LOGIMEM MEMUNITS DIGIF
            <num>  <num>    <num>    <num>    <num>  <num>   <num>    <num> <num>
          DIGIFLEN DIGIB DIGIBLEN MEMTIME BILLS TAXES SHOPPING GAMES STOVE MEALPREP
             <num> <num>    <num>   <num> <num> <num>    <num> <num> <num>    <num>
       1:       NA    NA       NA      NA    NA     0       NA     0     0        0
       2:       NA    NA       NA      NA    NA     0       NA     0    NA        0
       3:       NA    NA       NA      NA     0    NA        0    NA    NA        0
       4:       NA    NA       NA      NA    NA     0       NA     0     0        0
       5:       NA    NA       NA      NA    NA     0        0     0     3        0
       6:       NA    NA       NA      NA    NA    NA       NA    NA    NA        3
       7:       NA    NA       NA      NA    NA    NA        0    NA    NA        0
       8:       NA    NA       NA      NA    NA     0       NA    NA     0       NA
       9:       NA    NA       NA      NA     0    NA       NA     0    NA       NA
      10:       NA    NA       NA      NA     0    NA        0     0    NA        0
      11:       NA    NA       NA      NA     0    NA        0    NA     0       NA
      12:       NA    NA       NA      NA     0    NA        0    NA    NA       NA
      13:       NA    NA       NA      NA    NA    NA        0    NA     0       NA
      14:       NA    NA       NA      NA    NA    NA        0    NA     0        0
      15:       NA    NA       NA      NA    NA    NA        0     0     0        0
      16:       NA    NA       NA      NA     0    NA       NA     0     0       NA
      17:       NA    NA       NA      NA    NA    NA        3     0     0        3
      18:       NA    NA       NA      NA     8    NA        0    NA    NA       NA
      19:       NA    NA       NA      NA     0     0        0    NA    NA        0
      20:       NA    NA       NA      NA    NA     3       NA    NA    NA       NA
      21:       NA    NA       NA      NA    NA    NA       NA    NA    NA       NA
      22:       NA    NA       NA      NA     0     1       NA    NA    NA        3
      23:       NA    NA       NA      NA     0     3       NA    NA    NA        0
      24:       NA    NA       NA      NA    NA    NA       NA     0    NA       NA
      25:       NA    NA       NA      NA     0    NA        0    NA     0       NA
      26:       NA    NA       NA      NA     0     3        0    NA     1        0
      27:       NA    NA       NA      NA     0    NA        0    NA     0        0
      28:       NA    NA       NA      NA     0     0        0     0     3       NA
      29:       NA    NA       NA      NA    NA    NA        0     0     2       NA
      30:       NA    NA       NA      NA    NA    NA        0     0    NA        0
      31:       NA    NA       NA      NA     0    NA       NA     0     0       NA
      32:       NA    NA       NA      NA    NA    NA        3     0    NA       NA
      33:       NA    NA       NA      NA     3    NA        2     0     0       NA
      34:       NA    NA       NA      NA     0     0        0    NA     0       NA
      35:       NA    NA       NA      NA     0     0        2    NA     0        0
      36:       NA    NA       NA      NA    NA    NA       NA     0     0       NA
      37:       NA    NA       NA      NA     0     2       NA    NA    NA       NA
      38:       NA    NA       NA      NA    NA     0        0    NA     0       NA
      39:       NA    NA       NA      NA    NA     0        3    NA     0       NA
      40:       NA    NA       NA      NA    NA    NA       NA    NA    NA       NA
      41:       NA    NA       NA      NA     0    NA        0    NA     0        0
      42:       NA    NA       NA      NA    NA     0        0     0     0       NA
      43:       NA    NA       NA      NA    NA     0       NA    NA     0        3
      44:       NA    NA       NA      NA     0    NA       NA     0     0        0
      45:       NA    NA       NA      NA    NA     0        3    NA    NA        0
      46:       NA    NA       NA      NA    NA     0       NA     0    NA        0
      47:       NA    NA       NA      NA    NA     0       NA    NA    NA       NA
      48:       NA    NA       NA      NA     0    NA        0    NA    NA       NA
      49:       NA    NA       NA      NA    NA     0        0     0    NA       NA
      50:       NA    NA       NA      NA     0     0        0    NA    NA       NA
      51:       NA    NA       NA      NA     0     0       NA     0    NA       NA
      52:       NA    NA       NA      NA     1    NA       NA    NA     0        0
      53:       NA    NA       NA      NA     0     0       NA    NA     0       NA
      54:       NA    NA       NA      NA     0    NA       NA     0     0       NA
      55:       NA    NA       NA      NA    NA     0        0    NA     0       NA
      56:       NA    NA       NA      NA     1    NA        0     0     0        0
      57:       NA    NA       NA      NA     0    NA       NA    NA    NA        0
      58:       NA    NA       NA      NA    NA    NA        0     0    NA       NA
      59:       NA    NA       NA      NA    NA     3        0     0    NA       NA
      60:       NA    NA       NA      NA    NA     3       NA    NA     0       NA
      61:       NA    NA       NA      NA     0    NA       NA     0    NA       NA
      62:       NA    NA       NA      NA     0     0        0    NA    NA       NA
      63:       NA    NA       NA      NA    NA     8       NA    NA     0        0
      64:       NA    NA       NA      NA    NA     0        1     2    NA       NA
      65:       NA    NA       NA      NA     0    NA       NA     0     0        0
      66:       NA    NA       NA      NA     0    NA       NA     0    NA        0
      67:       NA    NA       NA      NA    NA    NA        0     0     0       NA
      68:       NA    NA       NA      NA    NA     0        0    NA     0        0
      69:       NA    NA       NA      NA    NA     0       NA    NA     0        0
      70:       NA    NA       NA      NA    NA    NA       NA    NA    NA       NA
      71:       NA    NA       NA      NA    NA    NA        3     1    NA       NA
      72:       NA    NA       NA      NA     0     3       NA    NA     0       NA
      73:       NA    NA       NA      NA     3    NA       NA     0    NA       NA
      74:       NA    NA       NA      NA     0    NA       NA    NA     0       NA
      75:       NA    NA       NA      NA     0     3        0    NA     0       NA
      76:       NA    NA       NA      NA    NA    NA       NA    NA    NA        0
      77:       NA    NA       NA      NA     0     0       NA    NA     0       NA
      78:       NA    NA       NA      NA    NA     0        0    NA    NA       NA
      79:       NA    NA       NA      NA     0     0        0     1     0        0
      80:       NA    NA       NA      NA    NA     0       NA    NA     0       NA
      81:       NA    NA       NA      NA    NA    NA       NA     0    NA       NA
      82:       NA    NA       NA      NA     3    NA       NA     0     0       NA
      83:       NA    NA       NA      NA     0     0       NA     0     0        0
      84:       NA    NA       NA      NA    NA    NA       NA    NA    NA        0
      85:       NA    NA       NA      NA     0     0       NA    NA    NA        0
      86:       NA    NA       NA      NA    NA    NA        0    NA    NA        0
      87:       NA    NA       NA      NA    NA     0       NA    NA     0       NA
      88:       NA    NA       NA      NA     3    NA        1    NA    NA       NA
      89:       NA    NA       NA      NA     0    NA        0     0     0       NA
      90:       NA    NA       NA      NA     0    NA       NA    NA    NA       NA
      91:       NA    NA       NA      NA     0     0       NA     0     0        0
          DIGIFLEN DIGIB DIGIBLEN MEMTIME BILLS TAXES SHOPPING GAMES STOVE MEALPREP
             <num> <num>    <num>   <num> <num> <num>    <num> <num> <num>    <num>
          EVENTS PAYATTN REMDATES TRAVEL BIRTHYR BIRTHMO VISITYR VISITMO VISITDAY
           <num>   <num>    <num>  <num>   <num>   <num>   <num>   <num>    <num>
       1:     NA      NA       NA      0    1948       3    2012       8       28
       2:      0      NA       NA     NA    1948       3    2016      12        6
       3:     NA      NA       NA      0    1948       3    2018      10       10
       4:     NA      NA       NA     NA    1954       3    2020      10       14
       5:      0      NA        0     NA    1954       3    2020      12        8
       6:     NA      NA       NA      1    1958       9    2012       3       16
       7:     NA      NA       NA     NA    1958       9    2012       8       15
       8:      0       0        0     NA    1958       9    2013       3        7
       9:      0       0       NA     NA    1951       9    2013      10       17
      10:     NA       1        0     NA    1966       9    2016       5       27
      11:      0      NA        0      0    1966       6    2019       5       13
      12:      0       0       NA     NA    1966       6    2019       6        6
      13:     NA      NA       NA     NA    1954       2    2023       3       21
      14:     NA       0       NA     NA    1951       9    2013      11       19
      15:     NA      NA       NA      0    1951      11    2018       6       27
      16:     NA      NA       NA     NA    1958       8    2020       6       25
      17:     NA      NA        0      0    1966      11    2020      11       25
      18:      0      NA       NA     NA    1966      11    2023       6       19
      19:     NA       0        3     NA    1952       5    2024       8       27
      20:      0      NA        0     NA    1952       3    2024      10       15
      21:      0      NA       NA      0    1945       2    2011      12        7
      22:      0       0        0     NA    1945       2    2016       3        2
      23:      0      NA        0     NA    1945       2    2017       7       27
      24:     NA      NA       NA      0    1970       2    2018       2        7
      25:     NA       0        0     NA    1946       2    2018      11       29
      26:     NA      NA        0     NA    1946       2    2019       2       11
      27:     NA       0       NA     NA    1946       2    2022       7       14
      28:      0      NA       NA      0    1961       2    2023       3        2
      29:     NA       3       NA      0      NA      NA    2014       6       23
      30:      0      NA       NA     NA    1960      11    2016       9        8
      31:      0       0       NA     NA    1943      11    2019      11       21
      32:      0       0       NA      0    1962      11    2020       9       29
      33:      0       0        0      0    1938       8    2022       6        9
      34:      0      NA       NA      0    1951       8    2023       3       23
      35:     NA       0        0      0    1945      NA    2017       6        7
      36:     NA      NA       NA     NA    1946      12    2011       1       28
      37:     NA      NA        0      0    1943      12    2016       4       12
      38:     NA       0       NA      0    1943      12    2017       5       10
      39:     NA       0        0     NA    1943      12    2017       7       17
      40:     NA       0        0     NA    1958      12    2017      12       12
      41:     NA       0       NA      0    1969      12    2018       3       28
      42:      0      NA        0     NA    1969      12    2018       5       31
      43:     NA      NA       NA     NA    1969      12    2021       8       12
      44:      0      NA       NA     NA    1969      12    2022       1        5
      45:      0      NA        3      0    1969      12    2023       6        8
      46:      0      NA       NA      0    1951      12    2023      12       18
      47:     NA       0        1     NA    1951      12    2024       8       20
      48:      0       0        0      3    1947      10    2025       3       20
      49:      0       0        0     NA    1944      10    2025       6       26
      50:     NA      NA       NA     NA    1945       7    2012       6       13
      51:     NA       0        0      3    1945      11    2018       3       15
      52:      0       0       NA     NA    1945      11    2021      12       20
      53:      0      NA        0      0    1945       9    2024      10       22
      54:      0      NA        0      0    1945       9    2024      12       12
      55:      0       0        1     NA    1945       9    2025       2        3
      56:     NA      NA        0      0    1945       9    2025       3       10
      57:     NA       2       NA      0    1952       2    2016       3        2
      58:      0       0       NA     NA    1952       2    2017       3       20
      59:      0       0       NA      0    1952       6    2017       9       11
      60:     NA       0       NA      0    1952       1    2023       8       21
      61:      0       0        2     NA    1939       8    2011       9        9
      62:      0       0        0      0    1930       8    2013       9       12
      63:     NA       3        0     NA    1930       8    2014       3       10
      64:      0      NA       NA      0    1930       8    2015       2       13
      65:     NA       0        0      0    1959       8    2024       8       28
      66:     NA      NA       NA      0    1949       7    2010       7        8
      67:     NA      NA        0      0    1949       7    2017       6        2
      68:     NA       0        0     NA    1949       7    2022       6       28
      69:     NA      NA       NA     NA    1956       7    2023      12        5
      70:     NA       0        0     NA    1951       7    2024       4        2
      71:      0       0        3      0    1949       9    2011      10       21
      72:     NA       0        0      0    1932       9    2012       5       14
      73:      0       3        0     NA    1932      12    2015       6       29
      74:      0       0        0      0    1945      12    2015       8       10
      75:     NA       0       NA     NA    1955      12    2018      12       19
      76:      0       0        0     NA    1953      12    2019      10       11
      77:      0       0        0      0    1953       4    2019      12        3
      78:     NA      NA       NA      0    1953       2    2022       9       12
      79:      2      NA        0      0    1953       1    2024       3       27
      80:      0      NA       NA      0    1962       3    2016      10       12
      81:     NA      NA       NA     NA    1962       3    2019      10       14
      82:     NA      NA       NA      3    1962       9    2022       8       12
      83:     NA       0        0      0    1962       9    2025       2        7
      84:     NA      NA       NA     NA    1965      12    2013      12       17
      85:     NA       0        0      0    1965      12    2017       6       14
      86:      0       0        0     NA    1953      12    2018       6       21
      87:     NA       1       NA     NA    1952      12    2021       9       29
      88:     NA       0        0      0    1934      11    2022       2        3
      89:      0       0       NA      0    1934       6    2023       7       14
      90:     NA       0        0     NA    1948       6    2024      10        1
      91:      0       0       NA      0      NA       3    2017      10        3
          EVENTS PAYATTN REMDATES TRAVEL BIRTHYR BIRTHMO VISITYR VISITMO VISITDAY
           <num>   <num>    <num>  <num>   <num>   <num>   <num>   <num>    <num>
          ALCDEM ANXIET BIPOLDX BRNINJ COGOTH COGOTH2 COGOTH3  CORT   CVD DELIR   DEP
           <num>  <num>   <num>  <num>  <num>   <num>   <num> <num> <num> <num> <num>
       1:     NA     NA      NA      1     NA      NA      NA    NA    NA    NA     1
       2:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
       3:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
       4:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA     1
       5:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
       6:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
       7:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
       8:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
       9:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      10:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      11:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      12:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      13:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      14:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA     1
      15:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      16:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      17:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      18:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA     1
      19:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      20:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      21:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      22:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      23:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      24:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      25:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      26:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      27:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      28:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      29:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      30:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA     1
      31:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      32:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      33:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      34:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      35:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      36:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      37:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      38:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      39:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      40:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      41:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      42:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      43:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA     1
      44:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      45:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      46:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      47:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      48:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      49:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      50:     NA      1      NA     NA     NA      NA      NA    NA    NA    NA    NA
      51:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      52:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      53:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA     1
      54:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      55:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      56:      1      1      NA     NA     NA      NA      NA    NA    NA    NA    NA
      57:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      58:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      59:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      60:     NA     NA      NA      1     NA      NA      NA    NA    NA    NA    NA
      61:     NA     NA      NA     NA      1      NA      NA    NA    NA    NA    NA
      62:     NA      1      NA     NA      1      NA      NA    NA    NA    NA    NA
      63:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      64:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      65:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      66:     NA     NA      NA     NA     NA      NA      NA    NA     1    NA    NA
      67:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      68:     NA      1      NA     NA     NA      NA      NA    NA    NA    NA    NA
      69:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      70:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      71:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      72:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      73:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      74:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA     1
      75:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      76:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      77:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      78:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      79:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      80:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      81:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      82:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      83:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      84:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      85:     NA     NA      NA     NA      1      NA      NA    NA    NA    NA    NA
      86:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      87:     NA      1      NA     NA     NA      NA      NA    NA    NA    NA    NA
      88:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      89:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      90:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
      91:     NA     NA      NA     NA     NA      NA      NA    NA    NA    NA    NA
          ALCDEM ANXIET BIPOLDX BRNINJ COGOTH COGOTH2 COGOTH3  CORT   CVD DELIR   DEP
           <num>  <num>   <num>  <num>  <num>   <num>   <num> <num> <num> <num> <num>
          DOWNS DYSILL EPILEP ESSTREM FTLDMO FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS
          <num>  <num>  <num>   <num>  <num>   <num> <num> <num>  <num>  <num> <num>
       1:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
       2:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
       3:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
       4:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
       5:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA     1
       6:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
       7:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
       8:    NA     NA      1      NA     NA      NA    NA    NA     NA     NA    NA
       9:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      10:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      11:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      12:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      13:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      14:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      15:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      16:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      17:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      18:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      19:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      20:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      21:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      22:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      23:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      24:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      25:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      26:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      27:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      28:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      29:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      30:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      31:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      32:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      33:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      34:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      35:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      36:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      37:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      38:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      39:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      40:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      41:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      42:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      43:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      44:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      45:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      46:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      47:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      48:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      49:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      50:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      51:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      52:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      53:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      54:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      55:    NA     NA      1      NA     NA      NA    NA    NA     NA     NA    NA
      56:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      57:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      58:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      59:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      60:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      61:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      62:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      63:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      64:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      65:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      66:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      67:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      68:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      69:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      70:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      71:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      72:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      73:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      74:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      75:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      76:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      77:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      78:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      79:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      80:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      81:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      82:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      83:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      84:    NA     NA     NA       1     NA      NA    NA    NA     NA     NA    NA
      85:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      86:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      87:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      88:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      89:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      90:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
      91:    NA     NA     NA      NA     NA      NA    NA    NA     NA     NA    NA
          DOWNS DYSILL EPILEP ESSTREM FTLDMO FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS
          <num>  <num>  <num>   <num>  <num>   <num> <num> <num>  <num>  <num> <num>
            MSA NACCALZD NACCLBDE  NEOP OTHCOG OTHPSY PPAPH PRION   PSP PTSDDX
          <num>    <num>    <num> <num>  <num>  <num> <num> <num> <num>  <num>
       1:    NA        1       NA    NA     NA     NA    NA    NA    NA     NA
       2:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
       3:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
       4:    NA        1       NA    NA     NA     NA    NA    NA    NA     NA
       5:    NA        1       NA    NA     NA     NA    NA    NA    NA     NA
       6:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
       7:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
       8:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
       9:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      10:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      11:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      12:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      13:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      14:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      15:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      16:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      17:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      18:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      19:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      20:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      21:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      22:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      23:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      24:    NA        1       NA    NA     NA     NA    NA    NA    NA     NA
      25:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      26:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      27:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      28:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      29:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      30:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      31:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      32:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      33:    NA       NA       NA    NA     NA     NA    NA    NA    NA      1
      34:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      35:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      36:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      37:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      38:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      39:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      40:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      41:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      42:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      43:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      44:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      45:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      46:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      47:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      48:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      49:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      50:    NA        1       NA    NA     NA     NA    NA    NA    NA     NA
      51:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      52:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      53:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      54:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      55:    NA        1       NA    NA     NA     NA    NA    NA    NA     NA
      56:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      57:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      58:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      59:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      60:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      61:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      62:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      63:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      64:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      65:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      66:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      67:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      68:    NA       NA        1    NA     NA     NA    NA    NA    NA     NA
      69:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      70:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      71:    NA       NA       NA    NA     NA      1    NA    NA    NA     NA
      72:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      73:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      74:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      75:    NA        1       NA    NA     NA     NA    NA    NA    NA     NA
      76:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      77:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      78:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      79:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      80:    NA        1       NA    NA     NA     NA    NA    NA    NA     NA
      81:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      82:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      83:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      84:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      85:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      86:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      87:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      88:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      89:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      90:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
      91:    NA       NA       NA    NA     NA     NA    NA    NA    NA     NA
            MSA NACCALZD NACCLBDE  NEOP OTHCOG OTHPSY PPAPH PRION   PSP PTSDDX
          <num>    <num>    <num> <num>  <num>  <num> <num> <num> <num>  <num>
          SCHIZOP STROKE ALCDEMIF ANXIETIF BIPOLDIF BRNINJIF COGOTHIF COGOTH2F
            <num>  <num>    <num>    <num>    <num>    <num>    <num>    <num>
       1:      NA      0       NA       NA       NA       NA       NA       NA
       2:      NA      0       NA       NA       NA       NA       NA       NA
       3:      NA     NA       NA       NA       NA       NA       NA       NA
       4:      NA     NA       NA       NA       NA       NA       NA       NA
       5:      NA     NA       NA       NA       NA       NA       NA       NA
       6:      NA     NA       NA       NA       NA       NA       NA       NA
       7:      NA     NA       NA       NA       NA       NA       NA       NA
       8:      NA     NA       NA       NA       NA       NA       NA       NA
       9:      NA     NA       NA       NA       NA       NA       NA       NA
      10:      NA     NA       NA       NA       NA       NA       NA       NA
      11:      NA     NA       NA       NA       NA       NA       NA       NA
      12:      NA     NA       NA        3       NA       NA       NA       NA
      13:      NA     NA       NA       NA       NA       NA       NA       NA
      14:      NA     NA       NA       NA       NA       NA       NA       NA
      15:      NA     NA       NA       NA       NA       NA       NA       NA
      16:      NA     NA       NA       NA       NA       NA       NA       NA
      17:      NA     NA       NA       NA       NA       NA       NA       NA
      18:      NA     NA       NA       NA       NA       NA       NA       NA
      19:      NA     NA       NA       NA       NA       NA       NA       NA
      20:      NA     NA       NA       NA       NA       NA       NA       NA
      21:      NA     NA       NA       NA       NA       NA       NA       NA
      22:      NA     NA       NA       NA       NA       NA       NA       NA
      23:      NA     NA       NA       NA       NA       NA       NA       NA
      24:      NA     NA       NA       NA       NA       NA       NA       NA
      25:      NA     NA       NA       NA       NA       NA        2       NA
      26:      NA     NA       NA       NA       NA       NA       NA       NA
      27:      NA     NA       NA       NA       NA       NA       NA       NA
      28:      NA     NA       NA       NA       NA       NA       NA       NA
      29:      NA     NA       NA       NA       NA       NA       NA       NA
      30:      NA     NA       NA       NA       NA       NA       NA       NA
      31:      NA     NA       NA       NA       NA       NA       NA       NA
      32:      NA     NA       NA       NA       NA       NA       NA       NA
      33:      NA     NA       NA       NA       NA       NA       NA       NA
      34:      NA     NA       NA       NA       NA       NA       NA       NA
      35:      NA     NA       NA       NA       NA       NA       NA       NA
      36:      NA     NA       NA       NA       NA       NA       NA       NA
      37:      NA     NA       NA       NA       NA       NA       NA       NA
      38:      NA     NA       NA       NA       NA       NA       NA       NA
      39:      NA     NA       NA       NA       NA       NA       NA       NA
      40:      NA     NA       NA       NA       NA       NA       NA       NA
      41:      NA     NA       NA       NA       NA       NA       NA       NA
      42:      NA     NA       NA       NA       NA       NA       NA       NA
      43:      NA     NA       NA       NA       NA       NA       NA       NA
      44:      NA     NA       NA       NA       NA       NA       NA       NA
      45:      NA     NA       NA       NA       NA       NA       NA       NA
      46:      NA     NA       NA       NA       NA       NA       NA       NA
      47:      NA      2       NA       NA       NA       NA       NA       NA
      48:      NA     NA       NA       NA       NA       NA       NA       NA
      49:      NA      0       NA       NA       NA       NA       NA       NA
      50:      NA     NA       NA       NA       NA       NA       NA       NA
      51:      NA     NA       NA       NA       NA       NA       NA       NA
      52:      NA     NA       NA       NA       NA       NA       NA       NA
      53:      NA     NA       NA       NA       NA       NA       NA       NA
      54:      NA     NA       NA       NA       NA       NA       NA       NA
      55:      NA     NA       NA       NA       NA       NA       NA       NA
      56:      NA     NA       NA       NA       NA       NA       NA       NA
      57:      NA     NA       NA       NA       NA       NA       NA       NA
      58:      NA     NA       NA       NA       NA       NA       NA       NA
      59:      NA     NA       NA       NA       NA       NA       NA       NA
      60:      NA     NA       NA       NA       NA       NA       NA       NA
      61:      NA     NA       NA        3       NA       NA       NA       NA
      62:      NA     NA       NA       NA       NA       NA       NA       NA
      63:      NA     NA       NA       NA       NA       NA       NA       NA
      64:      NA     NA       NA       NA       NA       NA       NA       NA
      65:      NA     NA       NA       NA       NA       NA       NA       NA
      66:      NA     NA       NA       NA       NA       NA       NA       NA
      67:      NA     NA       NA       NA       NA       NA       NA       NA
      68:      NA     NA       NA       NA       NA       NA       NA       NA
      69:      NA     NA       NA       NA       NA       NA       NA       NA
      70:      NA     NA       NA       NA       NA       NA       NA       NA
      71:      NA     NA       NA       NA       NA       NA       NA       NA
      72:      NA     NA       NA       NA       NA       NA       NA       NA
      73:      NA     NA        2       NA       NA       NA       NA       NA
      74:      NA     NA       NA       NA       NA       NA       NA       NA
      75:      NA     NA       NA       NA       NA       NA       NA       NA
      76:      NA     NA       NA       NA       NA       NA       NA       NA
      77:      NA     NA       NA       NA       NA       NA       NA       NA
      78:      NA     NA       NA       NA       NA       NA       NA       NA
      79:      NA     NA       NA       NA       NA       NA       NA       NA
      80:      NA     NA       NA       NA       NA       NA       NA       NA
      81:      NA     NA       NA       NA       NA       NA       NA       NA
      82:      NA     NA       NA       NA       NA       NA       NA       NA
      83:      NA     NA       NA       NA       NA       NA       NA       NA
      84:      NA     NA       NA       NA       NA       NA       NA       NA
      85:      NA     NA       NA       NA       NA       NA       NA       NA
      86:      NA     NA       NA       NA       NA       NA       NA       NA
      87:      NA     NA       NA       NA       NA       NA       NA       NA
      88:      NA     NA       NA       NA       NA       NA       NA       NA
      89:      NA     NA       NA       NA       NA       NA       NA       NA
      90:      NA     NA       NA       NA       NA       NA       NA       NA
      91:      NA     NA       NA       NA       NA       NA       NA       NA
          SCHIZOP STROKE ALCDEMIF ANXIETIF BIPOLDIF BRNINJIF COGOTHIF COGOTH2F
            <num>  <num>    <num>    <num>    <num>    <num>    <num>    <num>
          COGOTH3F CORTIF CVDIF DELIRIF DEPIF DOWNSIF DYSILLIF EPILEPIF ESSTREIF
             <num>  <num> <num>   <num> <num>   <num>    <num>    <num>    <num>
       1:       NA     NA    NA      NA    NA      NA       NA       NA       NA
       2:       NA     NA    NA      NA    NA      NA       NA       NA       NA
       3:       NA     NA    NA      NA    NA      NA       NA       NA        3
       4:       NA     NA    NA      NA    NA      NA       NA       NA       NA
       5:       NA     NA    NA      NA    NA      NA       NA       NA       NA
       6:       NA     NA    NA      NA     1      NA       NA       NA       NA
       7:       NA     NA    NA      NA    NA      NA       NA       NA       NA
       8:       NA     NA    NA      NA    NA      NA       NA       NA       NA
       9:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      10:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      11:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      12:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      13:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      14:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      15:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      16:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      17:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      18:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      19:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      20:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      21:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      22:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      23:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      24:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      25:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      26:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      27:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      28:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      29:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      30:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      31:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      32:       NA     NA    NA      NA     3      NA       NA       NA       NA
      33:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      34:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      35:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      36:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      37:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      38:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      39:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      40:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      41:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      42:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      43:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      44:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      45:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      46:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      47:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      48:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      49:       NA     NA     1      NA    NA      NA       NA       NA       NA
      50:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      51:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      52:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      53:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      54:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      55:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      56:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      57:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      58:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      59:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      60:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      61:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      62:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      63:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      64:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      65:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      66:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      67:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      68:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      69:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      70:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      71:       NA     NA    NA      NA     2      NA       NA       NA       NA
      72:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      73:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      74:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      75:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      76:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      77:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      78:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      79:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      80:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      81:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      82:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      83:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      84:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      85:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      86:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      87:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      88:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      89:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      90:       NA     NA    NA      NA    NA      NA       NA       NA       NA
      91:       NA     NA    NA      NA    NA      NA       NA       NA       NA
          COGOTH3F CORTIF CVDIF DELIRIF DEPIF DOWNSIF DYSILLIF EPILEPIF ESSTREIF
             <num>  <num> <num>   <num> <num>   <num>    <num>    <num>    <num>
          FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF IMPSUBIF MEDSIF MSAIF NACCALZP
             <num>    <num> <num>  <num>    <num>    <num>  <num> <num>    <num>
       1:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       2:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       3:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       4:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       5:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       6:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       7:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       8:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       9:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      10:       NA       NA    NA     NA       NA       NA     NA    NA        1
      11:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      12:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      13:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      14:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      15:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      16:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      17:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      18:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      19:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      20:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      21:       NA       NA    NA     NA       NA       NA     NA    NA        1
      22:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      23:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      24:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      25:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      26:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      27:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      28:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      29:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      30:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      31:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      32:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      33:       NA       NA    NA     NA       NA       NA     NA    NA        1
      34:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      35:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      36:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      37:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      38:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      39:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      40:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      41:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      42:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      43:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      44:       NA       NA    NA     NA       NA       NA     NA    NA        1
      45:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      46:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      47:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      48:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      49:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      50:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      51:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      52:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      53:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      54:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      55:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      56:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      57:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      58:       NA       NA    NA     NA       NA       NA     NA    NA        1
      59:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      60:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      61:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      62:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      63:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      64:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      65:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      66:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      67:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      68:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      69:       NA       NA    NA     NA       NA       NA     NA    NA        1
      70:       NA       NA    NA     NA       NA       NA      2    NA       NA
      71:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      72:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      73:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      74:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      75:       NA       NA    NA     NA       NA       NA     NA    NA        1
      76:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      77:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      78:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      79:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      80:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      81:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      82:       NA       NA    NA     NA       NA       NA      2    NA       NA
      83:       NA       NA    NA     NA       NA       NA     NA    NA        1
      84:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      85:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      86:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      87:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      88:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      89:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      90:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      91:       NA       NA    NA     NA       NA       NA      2    NA       NA
          FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF IMPSUBIF MEDSIF MSAIF NACCALZP
             <num>    <num> <num>  <num>    <num>    <num>  <num> <num>    <num>
          NACCLBDP NEOPIF OTHCOGIF OTHPSYIF PRIONIF PSPIF PTSDDXIF SCHIZOIF
             <num>  <num>    <num>    <num>   <num> <num>    <num>    <num>
       1:       NA     NA       NA       NA      NA    NA       NA       NA
       2:       NA     NA       NA       NA      NA    NA       NA       NA
       3:       NA     NA       NA       NA      NA    NA       NA       NA
       4:       NA     NA       NA       NA      NA    NA       NA       NA
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
      15:       NA      2       NA       NA      NA    NA       NA       NA
      16:       NA     NA       NA       NA      NA    NA       NA       NA
      17:       NA     NA       NA       NA      NA    NA       NA       NA
      18:       NA     NA       NA       NA      NA    NA       NA       NA
      19:       NA     NA       NA       NA      NA    NA       NA       NA
      20:       NA     NA       NA       NA      NA    NA       NA       NA
      21:       NA     NA       NA       NA      NA    NA       NA       NA
      22:       NA     NA       NA       NA      NA    NA       NA       NA
      23:       NA     NA       NA       NA      NA    NA       NA       NA
      24:       NA     NA       NA       NA      NA    NA       NA       NA
      25:       NA     NA       NA       NA      NA    NA       NA       NA
      26:       NA     NA       NA       NA      NA    NA       NA       NA
      27:       NA     NA       NA       NA      NA    NA       NA       NA
      28:       NA     NA       NA       NA      NA    NA       NA       NA
      29:       NA     NA       NA       NA      NA    NA       NA       NA
      30:       NA     NA       NA       NA      NA    NA       NA       NA
      31:       NA     NA       NA       NA      NA    NA       NA       NA
      32:       NA     NA       NA       NA      NA    NA       NA       NA
      33:       NA     NA       NA       NA      NA    NA       NA       NA
      34:       NA     NA       NA       NA      NA    NA       NA       NA
      35:       NA     NA       NA       NA      NA    NA       NA       NA
      36:       NA     NA       NA       NA      NA    NA       NA       NA
      37:       NA     NA       NA       NA      NA    NA       NA       NA
      38:       NA     NA       NA       NA      NA    NA       NA       NA
      39:       NA     NA       NA       NA      NA    NA       NA       NA
      40:       NA     NA       NA       NA      NA    NA       NA       NA
      41:       NA     NA       NA       NA      NA    NA       NA       NA
      42:       NA     NA       NA       NA      NA    NA       NA       NA
      43:       NA     NA       NA       NA      NA    NA       NA       NA
      44:       NA     NA       NA       NA      NA    NA       NA       NA
      45:       NA     NA       NA       NA      NA    NA       NA       NA
      46:       NA     NA       NA       NA      NA    NA       NA       NA
      47:       NA     NA       NA       NA      NA    NA       NA       NA
      48:       NA     NA       NA        3      NA    NA       NA       NA
      49:       NA     NA       NA       NA      NA    NA       NA       NA
      50:       NA     NA       NA       NA      NA    NA       NA       NA
      51:       NA     NA       NA       NA      NA    NA       NA       NA
      52:       NA     NA       NA       NA      NA    NA       NA       NA
      53:       NA     NA       NA       NA      NA    NA       NA       NA
      54:       NA     NA       NA       NA      NA    NA       NA       NA
      55:       NA     NA       NA       NA      NA    NA       NA       NA
      56:       NA     NA       NA       NA      NA    NA       NA       NA
      57:       NA     NA       NA       NA      NA    NA       NA       NA
      58:       NA     NA       NA       NA      NA    NA       NA       NA
      59:       NA     NA       NA       NA      NA    NA       NA       NA
      60:       NA     NA       NA       NA      NA    NA       NA       NA
      61:       NA     NA       NA       NA      NA    NA       NA       NA
      62:       NA     NA       NA       NA      NA    NA       NA       NA
      63:       NA     NA       NA       NA      NA    NA       NA       NA
      64:       NA     NA       NA       NA      NA    NA       NA       NA
      65:       NA     NA       NA       NA      NA    NA       NA       NA
      66:       NA     NA       NA       NA      NA    NA       NA       NA
      67:       NA     NA       NA       NA      NA    NA       NA       NA
      68:       NA     NA       NA       NA      NA    NA       NA       NA
      69:       NA     NA       NA       NA      NA    NA       NA       NA
      70:       NA     NA       NA       NA      NA    NA       NA       NA
      71:       NA     NA       NA       NA      NA    NA       NA       NA
      72:       NA     NA       NA       NA      NA    NA       NA       NA
      73:       NA     NA       NA       NA      NA    NA       NA       NA
      74:       NA     NA       NA       NA      NA    NA       NA       NA
      75:       NA     NA       NA       NA      NA    NA       NA       NA
      76:       NA     NA       NA       NA      NA    NA       NA       NA
      77:       NA     NA       NA       NA      NA    NA       NA       NA
      78:       NA     NA       NA       NA      NA    NA       NA       NA
      79:       NA      1       NA       NA      NA    NA       NA       NA
      80:       NA     NA       NA       NA      NA    NA       NA       NA
      81:       NA     NA       NA       NA      NA    NA       NA       NA
      82:       NA     NA       NA       NA      NA    NA       NA       NA
      83:       NA     NA       NA       NA      NA    NA       NA       NA
      84:       NA     NA       NA       NA      NA    NA       NA       NA
      85:       NA     NA       NA       NA      NA    NA       NA       NA
      86:       NA     NA       NA       NA      NA    NA       NA       NA
      87:       NA     NA       NA       NA      NA    NA       NA       NA
      88:       NA     NA       NA       NA      NA    NA       NA       NA
      89:       NA     NA       NA       NA      NA    NA       NA       NA
      90:       NA     NA       NA       NA      NA    NA       NA       NA
      91:       NA     NA       NA       NA      NA    NA       NA       NA
          NACCLBDP NEOPIF OTHCOGIF OTHPSYIF PRIONIF PSPIF PTSDDXIF SCHIZOIF
             <num>  <num>    <num>    <num>   <num> <num>    <num>    <num>
                  COGOTHX COGOTH2X COGOTH3X                  OTHCOGX OTHPSYX
                   <char>    <num>    <num>                   <char>   <num>
       1:            <NA>       NA       NA                     <NA>      NA
       2:            <NA>       NA       NA                     <NA>      NA
       3:            <NA>       NA       NA                     <NA>      NA
       4:            <NA>       NA       NA                     <NA>      NA
       5:            <NA>       NA       NA                     <NA>      NA
       6:            <NA>       NA       NA                     <NA>      NA
       7:            <NA>       NA       NA                     <NA>      NA
       8:            <NA>       NA       NA                     <NA>      NA
       9:            <NA>       NA       NA                     <NA>      NA
      10:            <NA>       NA       NA                     <NA>      NA
      11:            <NA>       NA       NA                     <NA>      NA
      12:            <NA>       NA       NA                     <NA>      NA
      13:            <NA>       NA       NA                     <NA>      NA
      14:            <NA>       NA       NA                     <NA>      NA
      15:            <NA>       NA       NA                     <NA>      NA
      16:            <NA>       NA       NA                     <NA>      NA
      17:            <NA>       NA       NA                     <NA>      NA
      18:            <NA>       NA       NA                     <NA>      NA
      19:            <NA>       NA       NA                     <NA>      NA
      20:            <NA>       NA       NA                     <NA>      NA
      21:            <NA>       NA       NA Spinal cerebellar ataxia      NA
      22:            <NA>       NA       NA                     <NA>      NA
      23:            <NA>       NA       NA                     <NA>      NA
      24:            <NA>       NA       NA                     <NA>      NA
      25:            <NA>       NA       NA                     <NA>      NA
      26:            <NA>       NA       NA                     <NA>      NA
      27:            <NA>       NA       NA                     <NA>      NA
      28:            <NA>       NA       NA                     <NA>      NA
      29:            <NA>       NA       NA                     <NA>      NA
      30:            <NA>       NA       NA                     <NA>      NA
      31:            <NA>       NA       NA                     <NA>      NA
      32:            <NA>       NA       NA                     <NA>      NA
      33:            <NA>       NA       NA                     <NA>      NA
      34:            <NA>       NA       NA                     <NA>      NA
      35:            <NA>       NA       NA                     <NA>      NA
      36:            <NA>       NA       NA                     <NA>      NA
      37:            <NA>       NA       NA                     <NA>      NA
      38:            <NA>       NA       NA                     <NA>      NA
      39:            <NA>       NA       NA                     <NA>      NA
      40:            <NA>       NA       NA                     <NA>      NA
      41:            <NA>       NA       NA                     <NA>      NA
      42:            <NA>       NA       NA                     <NA>      NA
      43:            <NA>       NA       NA                     <NA>      NA
      44:            <NA>       NA       NA                     <NA>      NA
      45:            <NA>       NA       NA                     <NA>      NA
      46:            <NA>       NA       NA                     <NA>      NA
      47:            <NA>       NA       NA                     <NA>      NA
      48:            <NA>       NA       NA                     <NA>      NA
      49:            <NA>       NA       NA                     <NA>      NA
      50:            <NA>       NA       NA                     <NA>      NA
      51:            <NA>       NA       NA                     <NA>      NA
      52:            <NA>       NA       NA                     <NA>      NA
      53:            <NA>       NA       NA                     <NA>      NA
      54:            <NA>       NA       NA                     <NA>      NA
      55:            <NA>       NA       NA                     <NA>      NA
      56:            <NA>       NA       NA                     <NA>      NA
      57:            <NA>       NA       NA                     <NA>      NA
      58:            <NA>       NA       NA                     <NA>      NA
      59:            <NA>       NA       NA                     <NA>      NA
      60:            <NA>       NA       NA                     <NA>      NA
      61:            <NA>       NA       NA                     <NA>      NA
      62:            <NA>       NA       NA                     <NA>      NA
      63:            <NA>       NA       NA                     <NA>      NA
      64:            <NA>       NA       NA                     <NA>      NA
      65:            <NA>       NA       NA                     <NA>      NA
      66:            <NA>       NA       NA                     <NA>      NA
      67:            <NA>       NA       NA                     <NA>      NA
      68:            <NA>       NA       NA                     <NA>      NA
      69:            <NA>       NA       NA                     <NA>      NA
      70:            <NA>       NA       NA                     <NA>      NA
      71:            <NA>       NA       NA                     <NA>      NA
      72:            <NA>       NA       NA                     <NA>      NA
      73:            <NA>       NA       NA                     <NA>      NA
      74:            <NA>       NA       NA                     <NA>      NA
      75:            <NA>       NA       NA                     <NA>      NA
      76:            <NA>       NA       NA                     <NA>      NA
      77:            <NA>       NA       NA                     <NA>      NA
      78:            <NA>       NA       NA                     <NA>      NA
      79:            <NA>       NA       NA                     <NA>      NA
      80:            <NA>       NA       NA                     <NA>      NA
      81:            <NA>       NA       NA                     <NA>      NA
      82:            <NA>       NA       NA                     <NA>      NA
      83:            <NA>       NA       NA                     <NA>      NA
      84:            <NA>       NA       NA                     <NA>      NA
      85:            <NA>       NA       NA                     <NA>      NA
      86:            <NA>       NA       NA                     <NA>      NA
      87:            <NA>       NA       NA                     <NA>      NA
      88:            <NA>       NA       NA                     <NA>      NA
      89: Sleep Disorders       NA       NA                     <NA>      NA
      90:            <NA>       NA       NA                     <NA>      NA
      91:            <NA>       NA       NA                     <NA>      NA
                  COGOTHX COGOTH2X COGOTH3X                  OTHCOGX OTHPSYX
                   <char>    <num>    <num>                   <char>   <num>
          CESDTOTAL respval respothx
              <num>   <num>    <num>
       1:         4      NA       NA
       2:        NA       1       NA
       3:        NA      NA       NA
       4:        NA      NA       NA
       5:        NA       1       NA
       6:         1      NA       NA
       7:         0      NA       NA
       8:        NA      NA       NA
       9:        NA      NA       NA
      10:        NA      NA       NA
      11:        NA      NA       NA
      12:        NA      NA       NA
      13:        NA       2       NA
      14:         1       1       NA
      15:        NA      NA       NA
      16:        NA      NA       NA
      17:         4      NA       NA
      18:        NA       1       NA
      19:        NA      NA       NA
      20:        NA       1       NA
      21:        NA      NA       NA
      22:         6      NA       NA
      23:        NA      NA       NA
      24:        NA      NA       NA
      25:        10      NA       NA
      26:        NA      NA       NA
      27:        NA      NA       NA
      28:        NA      NA       NA
      29:        NA      NA       NA
      30:         1      NA       NA
      31:         4       1       NA
      32:        NA       1       NA
      33:         1      NA       NA
      34:        NA      NA       NA
      35:        NA      NA       NA
      36:        NA      NA       NA
      37:         1      NA       NA
      38:        NA      NA       NA
      39:        NA      NA       NA
      40:        NA      NA       NA
      41:        NA      NA       NA
      42:         2       1       NA
      43:        NA      NA       NA
      44:        NA      NA       NA
      45:        NA      NA       NA
      46:         1      NA       NA
      47:         0      NA       NA
      48:        NA      NA       NA
      49:        NA      NA       NA
      50:        NA      NA       NA
      51:        NA      NA       NA
      52:        NA      NA       NA
      53:         1      NA       NA
      54:         3      NA       NA
      55:        NA      NA       NA
      56:        NA      NA       NA
      57:        NA       2       NA
      58:        NA      NA       NA
      59:        NA      NA       NA
      60:        NA      NA       NA
      61:         4      NA       NA
      62:        NA      NA       NA
      63:        NA      NA       NA
      64:         1      NA       NA
      65:        NA      NA       NA
      66:        NA      NA       NA
      67:        NA      NA       NA
      68:        NA      NA       NA
      69:        NA      NA       NA
      70:         7      NA       NA
      71:        NA      NA       NA
      72:        NA      NA       NA
      73:        13      NA       NA
      74:        NA      NA       NA
      75:        NA      NA       NA
      76:        NA      NA       NA
      77:        NA      NA       NA
      78:        NA      NA       NA
      79:        14      NA       NA
      80:        NA      NA       NA
      81:        NA      NA       NA
      82:         8      NA       NA
      83:         0      NA       NA
      84:        NA      NA       NA
      85:        NA      NA       NA
      86:        NA      NA       NA
      87:        NA      NA       NA
      88:        NA      NA       NA
      89:         1      NA       NA
      90:        NA      NA       NA
      91:        NA      NA       NA
          CESDTOTAL respval respothx
              <num>   <num>    <num>

# pull_redcap_data() prepares the frozen UDS-4 fixture

    Code
      out
    Output
          VISITYR VISITMO VISITDAY   SEX   NACCID  EDUC  RACE IQCODEINFORM IQCODESELF
            <num>   <num>    <int> <num>   <char> <num> <num>        <num>      <num>
       1:    2017      11       20     1 sim00001    NA    NA           NA         NA
       2:    2020      10       26    NA sim00003    NA    NA           NA         NA
       3:    2012       7       16    NA sim00004    NA     5           NA         NA
       4:    2021       6        1    NA sim00004    NA     5           NA         NA
       5:    2013       3       25    NA sim00007    NA     3           NA         NA
       6:    2022      11        1    NA sim00007    NA     3           NA         NA
       7:    2024       6        6    NA sim00007    NA     3           NA         NA
       8:    2024      11       11    NA sim00007    NA     3           NA         NA
       9:    2015       5       21     2 sim00008    NA     1           NA         NA
      10:    2025       3       11    NA sim00010    NA    NA           NA         NA
      11:    2020       3        3    NA sim00013    NA    NA           NA         NA
      12:    2014      12        2    NA sim00015    NA     1            3         NA
      13:    2024       3       27    NA sim00016    NA    NA           NA         NA
      14:    2017       2       23    NA sim00018    NA    NA           NA         NA
          HANDED CDRGLOB MOCATOTS MOCBTOTS TRAILA TRAILARR TRAILALI OTRAILA OTRLARR
           <num>   <num>    <num>    <num>  <num>    <num>    <num>   <num>   <num>
       1:     NA      NA       NA       NA     NA       NA       NA      NA      NA
       2:      2      NA       NA       NA     NA       NA       24      NA      NA
       3:     NA      NA       NA       NA     NA       NA       NA      NA      NA
       4:     NA      NA       NA       NA     NA       NA       NA      NA      NA
       5:     NA      NA       NA       NA     NA       NA       NA      NA      NA
       6:     NA     0.5       NA       NA     NA       NA       24      NA      NA
       7:     NA      NA       NA       NA     18       NA       NA      NA      NA
       8:     NA     0.5       NA       NA     33       NA       NA      NA      NA
       9:     NA      NA       NA       NA     NA       NA       NA      NA      NA
      10:     NA      NA       NA       NA     NA       NA       NA      NA      NA
      11:     NA      NA       NA       NA     NA       NA       NA      NA      NA
      12:     NA     0.0       NA       NA     NA       NA       NA      NA      NA
      13:     NA      NA       NA       NA     NA       NA       NA      NA      NA
      14:     NA      NA       NA       NA     NA       NA       NA      NA      NA
          DIGFORCT DIGFORSL DIGBACCT DIGBACLS  WAIS MINTTOTS ANIMALS   VEG UDSVERTN
             <num>    <num>    <num>    <num> <num>    <num>   <num> <num>    <num>
       1:       NA        6       NA       NA    NA       NA      NA    21       NA
       2:       NA       NA        6       NA    NA       NA      NA    NA       NA
       3:       NA       NA       NA       NA    NA       NA      NA    NA       NA
       4:       NA       NA       NA       NA    NA       NA      NA    NA       NA
       5:       NA       NA       NA       NA    NA       NA      NA    NA       NA
       6:       NA       NA       NA       NA    NA       NA      NA    NA       NA
       7:       NA       NA       NA        5    NA       NA      NA    NA       NA
       8:       NA       NA       NA       NA    NA       NA      NA    NA       NA
       9:       NA       NA       12       NA    NA       NA      NA    NA       NA
      10:       NA       NA       NA       NA    NA       NA      NA    NA       NA
      11:       NA       NA       NA       NA    NA       NA      NA    NA       NA
      12:       NA       NA       NA       NA    NA       NA      NA    NA       NA
      13:       NA       NA       NA       NA    NA       NA      NA    NA       NA
      14:       NA       NA       NA       NA    NA       NA      NA    NA       NA
          UDSVERFC UDSVERLC UDSBENTC UDSBENTD CRAFTVRS CRAFTURS CRAFTDVR CRAFTDRE
             <num>    <num>    <num>    <num>    <num>    <num>    <num>    <num>
       1:       NA       NA       NA       NA       NA       NA       NA       NA
       2:       NA       NA       NA       NA       NA       NA       NA       NA
       3:       NA        9       NA       NA       NA       NA       NA       NA
       4:       NA       NA       NA       NA       NA       NA       NA       NA
       5:       NA       NA       NA       NA       NA       NA       NA       NA
       6:       NA       NA       NA       NA       NA       NA       NA       NA
       7:       NA       NA       NA       NA       NA       NA       NA       NA
       8:       98       NA       NA       NA       NA       NA       NA       NA
       9:       NA       NA       NA       NA       96       NA       NA       NA
      10:       NA       NA       NA       NA       NA       NA       NA       NA
      11:       NA       NA       NA       NA       NA       NA       NA       NA
      12:       NA       NA       NA       NA       NA       NA       NA       NA
      13:       16       NA       NA       NA       NA       NA       NA       NA
      14:       NA       NA       NA       NA       NA       NA       NA        2
          REY1REC REY2REC REY3REC REY4REC REY5REC REYDLIST REY6REC REYDREC REYTCOR
            <num>   <num>   <num>   <num>   <num>    <num>   <num>   <num>   <num>
       1:      NA      NA      NA      NA      NA       NA      NA      NA      NA
       2:      NA      NA      NA      NA      15       NA      NA      NA      NA
       3:      NA      NA      NA      NA      NA       NA      NA      NA      NA
       4:      NA      NA      NA      NA      NA       NA      NA      NA      NA
       5:      NA      NA      NA      NA      NA       NA      NA      NA      NA
       6:      NA      NA      NA      NA      NA       NA      NA      NA      NA
       7:       7      NA      NA      NA      NA       NA      NA      NA      15
       8:      NA      NA      NA      NA      NA       NA      NA      NA      NA
       9:      NA      NA      NA      NA      NA       NA      NA      NA      NA
      10:      NA      NA      NA      NA      NA       NA      NA      NA      15
      11:      NA      NA      NA      NA      13       NA      NA      NA      NA
      12:      NA      NA      NA      NA      NA       NA      NA      NA       9
      13:      NA      NA      NA      NA      15       NA      NA      NA      NA
      14:      NA      NA      NA      NA      NA       NA      NA      NA      NA
          REYFPOS TRAILB TRAILBRR TRAILBLI MOCACLOC MOCACLOH MOCACLON OTRAILB OTRLBRR
            <num>  <num>    <num>    <num>    <num>    <num>    <num>   <num>   <num>
       1:      NA     NA       NA       NA       NA       NA       NA      NA      NA
       2:      NA     NA       NA       NA       NA       NA       NA      NA      NA
       3:      NA     NA       NA       NA       NA       NA       NA      NA      NA
       4:      NA     NA       NA       NA       NA       NA       NA      NA      NA
       5:      NA     NA       NA       NA       NA       NA       NA      NA      NA
       6:      NA     NA       NA       NA       NA       NA       NA      NA      NA
       7:      NA     NA       NA       NA       NA       NA       NA      NA      NA
       8:      NA     NA       NA       NA       NA       NA       NA      NA      NA
       9:      NA     NA        0       NA       NA       NA       NA     997      NA
      10:      NA     NA       NA       NA       NA       NA        1      NA      NA
      11:      NA     NA       NA       NA       NA       NA       NA      NA      NA
      12:      NA     NA       NA       NA       NA       NA       NA      NA      NA
      13:      NA     NA       NA       NA       NA       NA       NA      NA      NA
      14:      NA     NA       NA       NA       NA       NA        0      NA      NA
          OTRLBLI NACCGDS CDRSUM UDSBENRS NACCUDSD BILLS TAXES SHOPPING GAMES STOVE
            <num>   <num>  <num>    <num>    <num> <num> <num>    <num> <num> <num>
       1:      NA      NA     NA       NA       NA    NA    NA       NA    NA    NA
       2:      NA      NA     NA       NA       NA    NA    NA       NA    NA    NA
       3:      NA      NA     NA        1        1    NA     8       NA    NA    NA
       4:      NA      NA     NA       NA       NA     0    NA       NA     1    NA
       5:      NA      NA     NA       NA       NA     0    NA       NA    NA    NA
       6:      NA      NA     NA       NA       NA    NA    NA        0    NA    NA
       7:      NA       3     NA       NA       NA    NA    NA       NA    NA    NA
       8:      NA      NA     NA       NA       NA    NA    NA       NA    NA    NA
       9:      NA      NA     NA       NA       NA    NA    NA       NA     3    NA
      10:      NA      NA     NA       NA       NA    NA    NA       NA    NA    NA
      11:      NA      NA     NA       NA       NA    NA    NA       NA    NA     0
      12:      NA      NA     NA       NA       NA    NA    NA       NA    NA    NA
      13:      NA      NA     NA       NA       NA    NA    NA       NA    NA    NA
      14:      NA      NA     NA       NA       NA    NA    NA       NA    NA    NA
          MEALPREP EVENTS PAYATTN REMDATES TRAVEL BIRTHYR BIRTHMO ALCDEM ANXIET
             <num>  <num>   <num>    <num>  <num>   <num>   <num>  <num>  <num>
       1:       NA     NA      NA       NA     NA      NA      NA     NA     NA
       2:       NA     NA      NA       NA     NA      NA      NA     NA     NA
       3:       NA     NA      NA       NA     NA      NA      NA     NA     NA
       4:       NA     NA      NA       NA     NA      NA      NA     NA     NA
       5:       NA     NA      NA       NA     NA      NA      NA     NA     NA
       6:       NA     NA      NA       NA     NA      NA      NA     NA     NA
       7:       NA     NA      NA       NA     NA      NA      NA     NA     NA
       8:        0     NA      NA       NA     NA      NA      NA     NA     NA
       9:       NA     NA      NA       NA     NA      NA      NA     NA     NA
      10:       NA     NA      NA       NA     NA      NA      NA     NA     NA
      11:       NA     NA      NA       NA     NA      NA      NA     NA     NA
      12:       NA     NA      NA       NA     NA      NA      NA     NA     NA
      13:       NA     NA      NA       NA     NA      NA      NA     NA     NA
      14:       NA     NA      NA        2     NA      NA      NA     NA     NA
          BIPOLDX BRNINJ COGOTH COGOTH2 COGOTH3  CORT   CVD DELIR DOWNS EPILEP FTLDMO
            <num>  <num>  <num>   <num>   <num> <num> <num> <num> <num>  <num>  <num>
       1:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       2:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       3:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       4:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       5:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       6:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       7:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       8:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
       9:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      10:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      11:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      12:       1     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      13:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
      14:      NA     NA     NA      NA      NA    NA    NA    NA    NA     NA     NA
          FTLDNOS   HIV  HUNT HYCEPH IMPSUB  MEDS   MSA NACCALZD NACCLBDE  NEOP
            <num> <num> <num>  <num>  <num> <num> <num>    <num>    <num> <num>
       1:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       2:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       3:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       4:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       5:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       6:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       7:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       8:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
       9:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      10:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      11:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      12:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      13:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
      14:      NA    NA    NA     NA     NA    NA    NA       NA       NA    NA
          OTHCOG OTHPSY PPAPH PRION   PSP PTSDDX SCHIZOP STROKE ALCDEMIF ANXIETIF
           <num>  <num> <num> <num> <num>  <num>   <num>  <num>    <num>    <num>
       1:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
       2:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
       3:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
       4:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
       5:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
       6:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
       7:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
       8:     NA     NA    NA    NA    NA     NA      NA      0       NA       NA
       9:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
      10:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
      11:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
      12:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
      13:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
      14:     NA     NA    NA    NA    NA     NA      NA     NA       NA       NA
          BIPOLDIF COGOTHIF COGOTH2F COGOTH3F CORTIF CVDIF DELIRIF DOWNSIF EPILEPIF
             <num>    <num>    <num>    <num>  <num> <num>   <num>   <num>    <num>
       1:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       2:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       3:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       4:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       5:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       6:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       7:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       8:       NA       NA       NA       NA     NA    NA      NA      NA       NA
       9:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      10:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      11:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      12:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      13:       NA       NA       NA       NA     NA    NA      NA      NA       NA
      14:       NA       NA       NA       NA     NA    NA      NA      NA       NA
          FTLDMOIF FTLDNOIF HIVIF HUNTIF HYCEPHIF IMPSUBIF MEDSIF MSAIF NACCALZP
             <num>    <num> <num>  <num>    <num>    <num>  <num> <num>    <num>
       1:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       2:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       3:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       4:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       5:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       6:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       7:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       8:       NA       NA    NA     NA       NA       NA     NA    NA       NA
       9:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      10:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      11:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      12:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      13:       NA       NA    NA     NA       NA       NA     NA    NA       NA
      14:       NA       NA    NA     NA       NA       NA     NA    NA       NA
          NACCLBDP NEOPIF OTHCOGIF OTHPSYIF PRIONIF PSPIF PTSDDXIF SCHIZOIF COGOTHX
             <num>  <num>    <num>    <num>   <num> <num>    <num>    <num>  <char>
       1:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
       2:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
       3:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
       4:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
       5:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
       6:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
       7:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
       8:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
       9:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
      10:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
      11:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
      12:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
      13:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
      14:       NA     NA       NA       NA      NA    NA       NA       NA    <NA>
          COGOTH2X COGOTH3X OTHCOGX OTHPSYX CESDTOTAL
            <char>   <char>  <char>  <char>     <num>
       1:     <NA>     <NA>    <NA>    <NA>        NA
       2:     <NA>     <NA>    <NA>    <NA>        NA
       3:     <NA>     <NA>    <NA>    <NA>        NA
       4:     <NA>     <NA>    <NA>    <NA>        NA
       5:     <NA>     <NA>    <NA>    <NA>        NA
       6:     <NA>     <NA>    <NA>    <NA>        NA
       7:     <NA>     <NA>    <NA>    <NA>        NA
       8:     <NA>     <NA>    <NA>    <NA>        NA
       9:     <NA>     <NA>    <NA>    <NA>        NA
      10:     <NA>     <NA>    <NA>    <NA>        NA
      11:     <NA>     <NA>    <NA>    <NA>        NA
      12:     <NA>     <NA>    <NA>    <NA>        NA
      13:     <NA>     <NA>    <NA>    <NA>        NA
      14:     <NA>     <NA>    <NA>    <NA>        NA
          wadrc_c2_behavioral_observations_checklist_complete wadrc_c2_boc_mood___1
                                                        <num>                 <num>
       1:                                                  NA                    NA
       2:                                                  NA                    NA
       3:                                                  NA                    NA
       4:                                                  NA                    NA
       5:                                                  NA                    NA
       6:                                                  NA                    NA
       7:                                                  NA                    NA
       8:                                                  NA                    NA
       9:                                                  NA                    NA
      10:                                                  NA                    NA
      11:                                                  NA                    NA
      12:                                                  NA                    NA
      13:                                                  NA                    NA
      14:                                                  NA                    NA
          wadrc_c2_boc_mood___2 wadrc_c2_boc_mood___3 wadrc_c2_boc_mood___4
                          <num>                 <num>                 <num>
       1:                    NA                    NA                    NA
       2:                    NA                    NA                    NA
       3:                     0                    NA                    NA
       4:                    NA                    NA                    NA
       5:                    NA                    NA                    NA
       6:                    NA                    NA                    NA
       7:                    NA                    NA                    NA
       8:                    NA                    NA                    NA
       9:                    NA                    NA                    NA
      10:                    NA                    NA                    NA
      11:                    NA                    NA                     0
      12:                    NA                    NA                    NA
      13:                    NA                    NA                    NA
      14:                    NA                    NA                    NA
          wadrc_c2_boc_mood___5 wadrc_c2_boc_affect___1 wadrc_c2_boc_affect___2
                          <num>                   <num>                   <num>
       1:                    NA                      NA                      NA
       2:                    NA                      NA                       0
       3:                    NA                      NA                      NA
       4:                    NA                      NA                      NA
       5:                    NA                      NA                      NA
       6:                    NA                      NA                      NA
       7:                    NA                      NA                      NA
       8:                    NA                      NA                      NA
       9:                     0                      NA                      NA
      10:                    NA                      NA                      NA
      11:                     0                      NA                      NA
      12:                    NA                      NA                      NA
      13:                    NA                      NA                      NA
      14:                    NA                      NA                      NA
          wadrc_c2_boc_affect___3 wadrc_c2_boc_affect___4 wadrc_c2_boc_affect___5
                            <num>                   <num>                   <num>
       1:                      NA                      NA                      NA
       2:                       0                      NA                      NA
       3:                       0                       0                      NA
       4:                      NA                      NA                      NA
       5:                      NA                      NA                      NA
       6:                      NA                      NA                      NA
       7:                      NA                      NA                      NA
       8:                      NA                      NA                      NA
       9:                      NA                      NA                      NA
      10:                      NA                      NA                      NA
      11:                      NA                      NA                      NA
      12:                      NA                      NA                      NA
      13:                      NA                      NA                      NA
      14:                      NA                      NA                      NA
          wadrc_c2_boc_attitude___1 wadrc_c2_boc_attitude___2
                              <num>                     <num>
       1:                        NA                        NA
       2:                        NA                        NA
       3:                        NA                        NA
       4:                        NA                        NA
       5:                        NA                        NA
       6:                        NA                        NA
       7:                        NA                         0
       8:                        NA                        NA
       9:                        NA                        NA
      10:                        NA                        NA
      11:                        NA                        NA
      12:                        NA                        NA
      13:                        NA                        NA
      14:                        NA                         0
          wadrc_c2_boc_attitude___3 wadrc_c2_boc_attitude___4
                              <num>                     <num>
       1:                        NA                        NA
       2:                        NA                        NA
       3:                        NA                         0
       4:                        NA                        NA
       5:                        NA                        NA
       6:                        NA                        NA
       7:                        NA                        NA
       8:                        NA                        NA
       9:                        NA                        NA
      10:                         0                        NA
      11:                        NA                        NA
      12:                        NA                        NA
      13:                        NA                        NA
      14:                        NA                        NA
          wadrc_c2_boc_attitude___5 wadrc_c2_boc_language___1
                              <num>                     <num>
       1:                        NA                        NA
       2:                        NA                         0
       3:                        NA                        NA
       4:                        NA                        NA
       5:                        NA                        NA
       6:                        NA                        NA
       7:                        NA                        NA
       8:                        NA                        NA
       9:                        NA                        NA
      10:                        NA                        NA
      11:                        NA                        NA
      12:                        NA                        NA
      13:                        NA                        NA
      14:                        NA                        NA
          wadrc_c2_boc_language___2 wadrc_c2_boc_language___3
                              <num>                     <num>
       1:                        NA                        NA
       2:                        NA                        NA
       3:                        NA                         0
       4:                        NA                        NA
       5:                        NA                        NA
       6:                        NA                        NA
       7:                        NA                        NA
       8:                        NA                        NA
       9:                        NA                        NA
      10:                        NA                        NA
      11:                        NA                        NA
      12:                        NA                        NA
      13:                        NA                        NA
      14:                        NA                        NA
          wadrc_c2_boc_language___4 wadrc_c2_boc_language___5
                              <num>                     <num>
       1:                        NA                        NA
       2:                         0                         0
       3:                        NA                        NA
       4:                        NA                        NA
       5:                        NA                         0
       6:                        NA                        NA
       7:                        NA                        NA
       8:                        NA                        NA
       9:                        NA                        NA
      10:                        NA                        NA
      11:                        NA                        NA
      12:                        NA                        NA
      13:                        NA                         0
      14:                        NA                        NA
          wadrc_c2_boc_snsry_fncn___0 wadrc_c2_boc_snsry_fncn___1
                                <num>                       <num>
       1:                          NA                          NA
       2:                           0                          NA
       3:                          NA                          NA
       4:                          NA                          NA
       5:                          NA                          NA
       6:                          NA                          NA
       7:                          NA                          NA
       8:                          NA                          NA
       9:                          NA                          NA
      10:                          NA                          NA
      11:                          NA                          NA
      12:                          NA                          NA
      13:                          NA                          NA
      14:                          NA                          NA
          wadrc_c2_boc_snsry_fncn___2 wadrc_c2_boc_snsry_fncn___3
                                <num>                       <num>
       1:                          NA                          NA
       2:                           0                          NA
       3:                           0                          NA
       4:                          NA                          NA
       5:                          NA                           0
       6:                          NA                          NA
       7:                          NA                          NA
       8:                          NA                          NA
       9:                          NA                          NA
      10:                          NA                          NA
      11:                          NA                          NA
      12:                          NA                          NA
      13:                          NA                          NA
      14:                          NA                          NA
          wadrc_c2_boc_snsry_fncn___4 wadrc_c2_boc_snsry_fncn___5
                                <num>                       <num>
       1:                          NA                          NA
       2:                          NA                          NA
       3:                          NA                          NA
       4:                          NA                          NA
       5:                          NA                          NA
       6:                          NA                          NA
       7:                          NA                          NA
       8:                          NA                          NA
       9:                           0                          NA
      10:                          NA                          NA
      11:                          NA                          NA
      12:                          NA                          NA
      13:                          NA                          NA
      14:                          NA                           0
          wadrc_c2_boc_comprhnsn___1 wadrc_c2_boc_comprhnsn___2
                               <num>                      <num>
       1:                         NA                         NA
       2:                         NA                         NA
       3:                         NA                         NA
       4:                         NA                         NA
       5:                         NA                         NA
       6:                         NA                         NA
       7:                         NA                         NA
       8:                         NA                         NA
       9:                         NA                         NA
      10:                         NA                         NA
      11:                         NA                         NA
      12:                         NA                         NA
      13:                         NA                         NA
      14:                         NA                         NA
          wadrc_c2_boc_comprhnsn___3 wadrc_c2_boc_battery wadrc_c2_boc_notes respval
                               <num>                <num>             <char>   <num>
       1:                         NA                   NA               <NA>      NA
       2:                         NA                   NA               <NA>      NA
       3:                         NA                   NA               <NA>      NA
       4:                         NA                   NA               <NA>      NA
       5:                         NA                   NA               <NA>      NA
       6:                          0                   NA               <NA>      NA
       7:                         NA                   NA               <NA>      NA
       8:                         NA                   NA               <NA>       1
       9:                         NA                   NA               <NA>      NA
      10:                         NA                   NA               <NA>      NA
      11:                         NA                   NA               <NA>      NA
      12:                         NA                   NA               <NA>      NA
      13:                         NA                   NA               <NA>      NA
      14:                         NA                   NA               <NA>      NA
          loc_res___1 loc_res___2 loc_res___3 loc_res___4 loc_res___5 loc_res___6
                <num>       <num>       <num>       <num>       <num>       <num>
       1:          NA          NA          NA          NA          NA          NA
       2:          NA          NA          NA          NA          NA          NA
       3:           0          NA          NA           0          NA          NA
       4:          NA          NA          NA          NA          NA          NA
       5:          NA          NA          NA          NA          NA          NA
       6:          NA          NA          NA          NA          NA          NA
       7:          NA          NA          NA           0          NA          NA
       8:          NA           0          NA          NA          NA          NA
       9:           0          NA          NA          NA          NA          NA
      10:          NA          NA           0          NA          NA          NA
      11:          NA          NA          NA          NA          NA          NA
      12:          NA          NA          NA          NA          NA          NA
      13:          NA          NA          NA          NA          NA          NA
      14:          NA          NA          NA          NA          NA          NA
          loc_res___7 loc_res___8 respothx
                <num>       <num>   <char>
       1:          NA          NA     <NA>
       2:          NA          NA     <NA>
       3:          NA          NA     <NA>
       4:          NA          NA     <NA>
       5:          NA          NA     <NA>
       6:          NA          NA     <NA>
       7:          NA          NA     <NA>
       8:          NA          NA     <NA>
       9:          NA          NA     <NA>
      10:          NA          NA     <NA>
      11:          NA          NA     <NA>
      12:          NA          NA     <NA>
      13:          NA          NA     <NA>
      14:          NA          NA     <NA>

# live UDS-2 pull meets its contract

    Code
      vapply(out, function(x) class(x)[1], character(1))
    Output
            NACCID      VISITYR      VISITMO     VISITDAY          SEX         EDUC 
       "character"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
              RACE IQCODEINFORM   IQCODESELF       HANDED      CDRGLOB       TRAILA 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          TRAILARR     TRAILALI         WAIS      ANIMALS          VEG      REY1REC 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
           REY2REC      REY3REC      REY4REC      REY5REC     REYDLIST      REY6REC 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
           REYDREC       TRAILB     TRAILBRR     TRAILBLI      NACCGDS       CDRSUM 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          NACCMMSE       BOSTON      LOGIMEM     MEMUNITS        DIGIF     DIGIFLEN 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
             DIGIB     DIGIBLEN      MEMTIME        BILLS        TAXES     SHOPPING 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
             GAMES        STOVE     MEALPREP       EVENTS      PAYATTN     REMDATES 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            TRAVEL      BIRTHYR      BIRTHMO       ALCDEM       ANXIET      BIPOLDX 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            BRNINJ       COGOTH      COGOTH2      COGOTH3         CORT          CVD 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
             DELIR        DEMUN          DEP        DOWNS       DYSILL       EPILEP 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
           ESSTREM       FTLDMO      FTLDNOS          HIV         HUNT       HYCEPH 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            IMPSUB         MEDS          MSA     NACCALZD     NACCLBDE         NEOP 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            OTHCOG       OTHPSY       POSSAD        PPAPH        PRION       PROBAD 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
               PSP       PTSDDX      SCHIZOP       STROKE         VASC       VASCPS 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          ALCDEMIF     ANXIETIF     BIPOLDIF     BRNINJIF     COGOTHIF     COGOTH2F 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          COGOTH3F       CORTIF        CVDIF      DELIRIF      DEMUNIF        DEPIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
           DOWNSIF     DYSILLIF     EPILEPIF     ESSTREIF     FTLDMOIF     FTLDNOIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
             HIVIF       HUNTIF     HYCEPHIF     IMPSUBIF       MEDSIF        MSAIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          NACCALZP     NACCLBDP       NEOPIF     OTHCOGIF     OTHPSYIF     POSSADIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
           PPAPHIF      PRIONIF     PROBADIF        PSPIF     SCHIZOIF      STROKIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            VASCIF     VASCPSIF      COGOTHX     COGOTH2X     COGOTH3X      OTHCOGX 
         "numeric"    "numeric"  "character"  "character"  "character"    "numeric" 
           OTHPSYX    CESDTOTAL 
         "numeric"    "numeric" 

# live UDS-3 pull meets its contract

    Code
      vapply(out, function(x) class(x)[1], character(1))
    Output
            NACCID          SEX         EDUC         RACE IQCODEINFORM   IQCODESELF 
       "character"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            HANDED      CDRGLOB     MOCATOTS     MOCBTOTS       TRAILA     TRAILARR 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          TRAILALI      OTRAILA      OTRLARR     DIGFORCT     DIGFORSL     DIGBACCT 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          DIGBACLS         WAIS     MINTTOTS      ANIMALS          VEG     UDSVERTN 
         "numeric"  "character"    "numeric"    "numeric"    "numeric"    "numeric" 
          UDSVERFC     UDSVERLC     UDSBENTC     UDSBENTD     CRAFTVRS     CRAFTURS 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          CRAFTDVR     CRAFTDRE      REY1REC      REY2REC      REY3REC      REY4REC 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
           REY5REC     REYDLIST      REY6REC      REYDREC      REYTCOR      REYTNEG 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            TRAILB     TRAILBRR     TRAILBLI     MOCACLOC     MOCACLOH     MOCACLON 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
           OTRAILB      OTRLBRR      OTRLBLI      NACCGDS       CDRSUM     UDSBENRS 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          NACCUDSD     NACCMMSE       BOSTON      LOGIMEM     MEMUNITS        DIGIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          DIGIFLEN        DIGIB     DIGIBLEN      MEMTIME        BILLS        TAXES 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          SHOPPING        GAMES        STOVE     MEALPREP       EVENTS      PAYATTN 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          REMDATES       TRAVEL      BIRTHYR      BIRTHMO      VISITYR      VISITMO 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          VISITDAY       ALCDEM       ANXIET      BIPOLDX       BRNINJ       COGOTH 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
           COGOTH2      COGOTH3         CORT          CVD        DELIR          DEP 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
             DOWNS       DYSILL       EPILEP      ESSTREM       FTLDMO      FTLDNOS 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
               HIV         HUNT       HYCEPH       IMPSUB         MEDS          MSA 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          NACCALZD     NACCLBDE         NEOP       OTHCOG       OTHPSY        PPAPH 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
             PRION          PSP       PTSDDX      SCHIZOP       STROKE     ALCDEMIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          ANXIETIF     BIPOLDIF     BRNINJIF     COGOTHIF     COGOTH2F     COGOTH3F 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            CORTIF        CVDIF      DELIRIF        DEPIF      DOWNSIF     DYSILLIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          EPILEPIF     ESSTREIF     FTLDMOIF     FTLDNOIF        HIVIF       HUNTIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          HYCEPHIF     IMPSUBIF       MEDSIF        MSAIF     NACCALZP     NACCLBDP 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
            NEOPIF     OTHCOGIF     OTHPSYIF      PRIONIF        PSPIF     PTSDDXIF 
         "numeric"    "numeric"    "numeric"    "numeric"    "numeric"    "numeric" 
          SCHIZOIF      COGOTHX     COGOTH2X     COGOTH3X      OTHCOGX      OTHPSYX 
         "numeric"  "character"  "character"  "character"  "character"  "character" 
         CESDTOTAL      respval     respothx 
         "numeric"    "numeric"  "character" 

# live UDS-4 pull meets its contract

    Code
      vapply(out, function(x) class(x)[1], character(1))
    Output
                                                  VISITYR 
                                                "numeric" 
                                                  VISITMO 
                                                "numeric" 
                                                 VISITDAY 
                                                "integer" 
                                                      SEX 
                                                "numeric" 
                                                   NACCID 
                                              "character" 
                                                     EDUC 
                                                "numeric" 
                                                     RACE 
                                                "numeric" 
                                             IQCODEINFORM 
                                                "numeric" 
                                               IQCODESELF 
                                                "numeric" 
                                                   HANDED 
                                                "numeric" 
                                                  CDRGLOB 
                                                "numeric" 
                                                 MOCATOTS 
                                                "numeric" 
                                                 MOCBTOTS 
                                                "numeric" 
                                                   TRAILA 
                                                "numeric" 
                                                 TRAILARR 
                                                "numeric" 
                                                 TRAILALI 
                                                "numeric" 
                                                  OTRAILA 
                                                "numeric" 
                                                  OTRLARR 
                                                "numeric" 
                                                 DIGFORCT 
                                                "numeric" 
                                                 DIGFORSL 
                                                "numeric" 
                                                 DIGBACCT 
                                                "numeric" 
                                                 DIGBACLS 
                                                "numeric" 
                                                     WAIS 
                                                "numeric" 
                                                 MINTTOTS 
                                                "numeric" 
                                                  ANIMALS 
                                                "numeric" 
                                                      VEG 
                                                "numeric" 
                                                 UDSVERTN 
                                                "numeric" 
                                                 UDSVERFC 
                                                "numeric" 
                                                 UDSVERLC 
                                                "numeric" 
                                                 UDSBENTC 
                                                "numeric" 
                                                 UDSBENTD 
                                                "numeric" 
                                                 CRAFTVRS 
                                                "numeric" 
                                                 CRAFTURS 
                                                "numeric" 
                                                 CRAFTDVR 
                                                "numeric" 
                                                 CRAFTDRE 
                                                "numeric" 
                                                  REY1REC 
                                                "numeric" 
                                                  REY2REC 
                                                "numeric" 
                                                  REY3REC 
                                                "numeric" 
                                                  REY4REC 
                                                "numeric" 
                                                  REY5REC 
                                                "numeric" 
                                                 REYDLIST 
                                                "numeric" 
                                                  REY6REC 
                                                "numeric" 
                                                  REYDREC 
                                                "numeric" 
                                                  REYTCOR 
                                                "numeric" 
                                                  REYFPOS 
                                                "numeric" 
                                                   TRAILB 
                                                "numeric" 
                                                 TRAILBRR 
                                                "numeric" 
                                                 TRAILBLI 
                                                "numeric" 
                                                 MOCACLOC 
                                                "numeric" 
                                                 MOCACLOH 
                                                "numeric" 
                                                 MOCACLON 
                                                "numeric" 
                                                  OTRAILB 
                                                "numeric" 
                                                  OTRLBRR 
                                                "numeric" 
                                                  OTRLBLI 
                                                "numeric" 
                                                  NACCGDS 
                                                "numeric" 
                                                   CDRSUM 
                                                "numeric" 
                                                 UDSBENRS 
                                                "numeric" 
                                                 NACCUDSD 
                                                "numeric" 
                                                    BILLS 
                                                "numeric" 
                                                    TAXES 
                                                "numeric" 
                                                 SHOPPING 
                                                "numeric" 
                                                    GAMES 
                                                "numeric" 
                                                    STOVE 
                                                "numeric" 
                                                 MEALPREP 
                                                "numeric" 
                                                   EVENTS 
                                                "numeric" 
                                                  PAYATTN 
                                                "numeric" 
                                                 REMDATES 
                                                "numeric" 
                                                   TRAVEL 
                                                "numeric" 
                                                  BIRTHYR 
                                                "numeric" 
                                                  BIRTHMO 
                                                "numeric" 
                                                   ALCDEM 
                                                "numeric" 
                                                   ANXIET 
                                                "numeric" 
                                                  BIPOLDX 
                                                "numeric" 
                                                   BRNINJ 
                                                "numeric" 
                                                   COGOTH 
                                                "numeric" 
                                                  COGOTH2 
                                                "numeric" 
                                                  COGOTH3 
                                                "numeric" 
                                                     CORT 
                                                "numeric" 
                                                      CVD 
                                                "numeric" 
                                                    DELIR 
                                                "numeric" 
                                                    DOWNS 
                                                "numeric" 
                                                   EPILEP 
                                                "numeric" 
                                                   FTLDMO 
                                                "numeric" 
                                                  FTLDNOS 
                                                "numeric" 
                                                      HIV 
                                                "numeric" 
                                                     HUNT 
                                                "numeric" 
                                                   HYCEPH 
                                                "numeric" 
                                                   IMPSUB 
                                                "numeric" 
                                                     MEDS 
                                                "numeric" 
                                                      MSA 
                                                "numeric" 
                                                 NACCALZD 
                                                "numeric" 
                                                 NACCLBDE 
                                                "numeric" 
                                                     NEOP 
                                                "numeric" 
                                                   OTHCOG 
                                                "numeric" 
                                                   OTHPSY 
                                                "numeric" 
                                                    PPAPH 
                                                "numeric" 
                                                    PRION 
                                                "numeric" 
                                                      PSP 
                                                "numeric" 
                                                   PTSDDX 
                                                "numeric" 
                                                  SCHIZOP 
                                                "numeric" 
                                                   STROKE 
                                                "numeric" 
                                                 ALCDEMIF 
                                                "numeric" 
                                                 ANXIETIF 
                                                "numeric" 
                                                 BIPOLDIF 
                                                "numeric" 
                                                 COGOTHIF 
                                                "numeric" 
                                                 COGOTH2F 
                                                "numeric" 
                                                 COGOTH3F 
                                                "numeric" 
                                                   CORTIF 
                                                "numeric" 
                                                    CVDIF 
                                                "numeric" 
                                                  DELIRIF 
                                                "numeric" 
                                                  DOWNSIF 
                                                "numeric" 
                                                 EPILEPIF 
                                                "numeric" 
                                                 FTLDMOIF 
                                                "numeric" 
                                                 FTLDNOIF 
                                                "numeric" 
                                                    HIVIF 
                                                "numeric" 
                                                   HUNTIF 
                                                "numeric" 
                                                 HYCEPHIF 
                                                "numeric" 
                                                 IMPSUBIF 
                                                "numeric" 
                                                   MEDSIF 
                                                "numeric" 
                                                    MSAIF 
                                                "numeric" 
                                                 NACCALZP 
                                                "numeric" 
                                                 NACCLBDP 
                                                "numeric" 
                                                   NEOPIF 
                                                "numeric" 
                                                 OTHCOGIF 
                                                "numeric" 
                                                 OTHPSYIF 
                                                "numeric" 
                                                  PRIONIF 
                                                "numeric" 
                                                    PSPIF 
                                                "numeric" 
                                                 PTSDDXIF 
                                                "numeric" 
                                                 SCHIZOIF 
                                                "numeric" 
                                                  COGOTHX 
                                              "character" 
                                                 COGOTH2X 
                                              "character" 
                                                 COGOTH3X 
                                              "character" 
                                                  OTHCOGX 
                                              "character" 
                                                  OTHPSYX 
                                              "character" 
                                                CESDTOTAL 
                                                "numeric" 
      wadrc_c2_behavioral_observations_checklist_complete 
                                                "numeric" 
                                    wadrc_c2_boc_mood___1 
                                                "numeric" 
                                    wadrc_c2_boc_mood___2 
                                                "numeric" 
                                    wadrc_c2_boc_mood___3 
                                                "numeric" 
                                    wadrc_c2_boc_mood___4 
                                                "numeric" 
                                    wadrc_c2_boc_mood___5 
                                                "numeric" 
                                  wadrc_c2_boc_affect___1 
                                                "numeric" 
                                  wadrc_c2_boc_affect___2 
                                                "numeric" 
                                  wadrc_c2_boc_affect___3 
                                                "numeric" 
                                  wadrc_c2_boc_affect___4 
                                                "numeric" 
                                  wadrc_c2_boc_affect___5 
                                                "numeric" 
                                wadrc_c2_boc_attitude___1 
                                                "numeric" 
                                wadrc_c2_boc_attitude___2 
                                                "numeric" 
                                wadrc_c2_boc_attitude___3 
                                                "numeric" 
                                wadrc_c2_boc_attitude___4 
                                                "numeric" 
                                wadrc_c2_boc_attitude___5 
                                                "numeric" 
                                wadrc_c2_boc_language___1 
                                                "numeric" 
                                wadrc_c2_boc_language___2 
                                                "numeric" 
                                wadrc_c2_boc_language___3 
                                                "numeric" 
                                wadrc_c2_boc_language___4 
                                                "numeric" 
                                wadrc_c2_boc_language___5 
                                                "numeric" 
                              wadrc_c2_boc_snsry_fncn___0 
                                                "numeric" 
                              wadrc_c2_boc_snsry_fncn___1 
                                                "numeric" 
                              wadrc_c2_boc_snsry_fncn___2 
                                                "numeric" 
                              wadrc_c2_boc_snsry_fncn___3 
                                                "numeric" 
                              wadrc_c2_boc_snsry_fncn___4 
                                                "numeric" 
                              wadrc_c2_boc_snsry_fncn___5 
                                                "numeric" 
                               wadrc_c2_boc_comprhnsn___1 
                                                "numeric" 
                               wadrc_c2_boc_comprhnsn___2 
                                                "numeric" 
                               wadrc_c2_boc_comprhnsn___3 
                                                "numeric" 
                                     wadrc_c2_boc_battery 
                                                "numeric" 
                                       wadrc_c2_boc_notes 
                                              "character" 
                                                  respval 
                                                "numeric" 
                                              loc_res___1 
                                                "numeric" 
                                              loc_res___2 
                                                "numeric" 
                                              loc_res___3 
                                                "numeric" 
                                              loc_res___4 
                                                "numeric" 
                                              loc_res___5 
                                                "numeric" 
                                              loc_res___6 
                                                "numeric" 
                                              loc_res___7 
                                                "numeric" 
                                              loc_res___8 
                                                "numeric" 
                                                 respothx 
                                              "character" 


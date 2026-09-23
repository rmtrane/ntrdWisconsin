# live: csf pull meets its contract

    Code
      vapply(out, function(x) class(x)[1], character(1))[sort(names(out))]
    Output
                                         age                   csf_amprion_asyn_cat 
                                   "numeric"                               "factor" 
            csf_ratio_lumi_ab42_ab40_fda_cat       csf_ratio_lumi_ab42_ab40_fda_raw 
                                    "factor"                              "numeric" 
      csf_ratio_roche_ptau181_ab42_local_cat csf_ratio_roche_ptau181_ab42_local_raw 
                                    "factor"                              "numeric" 
                                        date                                enumber 
                                      "Date"                            "character" 

# live: plasma pull meets its contract

    Code
      vapply(out, function(x) class(x)[1], character(1))[sort(names(out))]
    Output
                         age                   date                enumber 
                   "numeric"                 "Date"            "character" 
      hdx_ptau217_ashton_cat hdx_ptau217_ashton_raw lumi_ptau217_local_cat 
                    "factor"              "numeric"               "factor" 
      lumi_ptau217_local_raw 
                   "numeric" 

# live: visual_ratings pull meets its contract

    Code
      vapply(out, function(x) class(x)[1], character(1))[sort(names(out))]
    Output
                                  age                     braak_1_cat 
                            "numeric"                       "numeric" 
                          braak_2_cat                     braak_3_cat 
                            "numeric"                       "numeric" 
                          braak_4_cat                     braak_5_cat 
                            "numeric"                       "numeric" 
                          braak_6_cat               braak_comment_cat 
                            "numeric"                       "numeric" 
                   braak_positive_cat                            date 
                          "character"                          "Date" 
                              enumber      nav4694_visual_ratings_cat 
                          "character"                       "numeric" 
      pib_visual_ratings_20180126_cat 
                            "numeric" 


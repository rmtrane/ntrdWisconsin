#' Process behavioral observations and response validity data
#'
#' @description
#' Combines two processing pipelines into a single long-format
#' [data.table::data.table()]:
#'
#' 1. **Behavioral observations** – columns matching `wadrc_c2_boc_*` are
#'    melted into long format, split on `___` to separate the domain variable
#'    from its sub-item index (`subvar`), then labelled with human-readable
#'    `label` and `sublabel` factor columns. Parenthetical clarifications
#'    embedded in sublabel strings are extracted into a separate `extra_info`
#'    column and stripped from `sublabel`. Numeric REDCap codes are recoded to
#'    `"checked"` / `"unchecked"` (or the raw text for `notes` fields).
#'
#' 2. **Response validity** – columns matching `respval`, `loc_res_*`, and
#'    `respothx` are melted and labelled analogously. `respval` codes `1-3`
#'    are recoded to `"Very valid"` / `"Questionably valid"` / `"Invalid"`;
#'    checkbox `loc_res_*` codes become `"checked"` / `"unchecked"`. Free-text
#'    from `respothx` is carried into `extra_info_2` for `loc_res___8`.
#'
#' The two tables are row-bound with [data.table::rbindlist()]. `VISITDATE`,
#' `label`, and `sublabel` are re-levelled so that visits appear in
#' reverse-chronological order and domain/item groups appear in the intended
#' display order. Finally, `value` is packed into a named list column
#' (`list(value = ..., extra_info = ...)`) ready for consumption by
#' `format_date_cell()`.
#'
#' @param dat A [data.table::data.table()] (or coercible object) containing at
#'   minimum the following columns:
#'   \describe{
#'     \item{`NACCID`}{Participant identifier.}
#'     \item{`VISITDATE`}{Visit date (coercible to character for sorting).}
#'     \item{`wadrc_c2_boc_*`}{REDCap checkbox / text columns for behavioral
#'       observation categories. Column names must follow the pattern
#'       `wadrc_c2_boc_<variable>` or `wadrc_c2_boc_<variable>___<subvar>`.}
#'     \item{`respval`}{Integer code for overall response validity (1–3).}
#'     \item{`loc_res___1`, ..., `loc_res___8`}{REDCap checkbox columns indicating
#'       reasons for reduced response validity.}
#'     \item{`respothx`}{Free-text field for "Other" response validity reason
#'       (`loc_res___8`).}
#'   }
#'
#' @returns A [data.table::data.table()] returned invisibly with one row per
#'   participant × visit × behavioral item, and the following key columns:
#'   \describe{
#'     \item{`NACCID`}{Participant identifier.}
#'     \item{`VISITDATE`}{Factor with levels sorted in reverse-chronological
#'       order.}
#'     \item{`label`}{Ordered factor giving the behavioral domain (e.g.,
#'       `"Mood"`, `"Affect"`, `"Response Validity"`).}
#'     \item{`sublabel`}{Ordered factor giving the specific item label within
#'       the domain (parenthetical qualifiers removed).}
#'     \item{`extra_info`}{Character. Parenthetical qualifier extracted from
#'       the original sublabel string, or `NA` if none was present.}
#'     \item{`value`}{List column. Each element is a named list with a
#'       `$value` element (character: `"checked"`, `"unchecked"`, a validity
#'       rating string, raw note text, or `NA`) and an optional `$extra_info`
#'       element (character tooltip text).}
#'   }
#'
#' @export
respval_behavioral_data <- function(dat) {
  dat <- data.table::copy(dat)

  for_melt <- dat[,
    c(
      NACCID = list(NACCID),
      VISITDATE = list(VISITDATE),
      setNames(lapply(.SD, as.character), names(.SD))
    ),
    .SDcols = c(
      unlist(data.table::patterns(
        "wadrc_c2_boc",
        cols = names(dat)
      ))
    )
  ]

  wadrc_behavioral_obs <- data.table::melt(
    for_melt,
    measure.vars = data.table::measure(
      variable,
      pattern = "wadrc_c2_boc_(.*)"
    ),
  )[
    order(NACCID, VISITDATE),
    c("variable", "subvar") := data.table::tstrsplit(
      variable,
      "___",
      fixed = TRUE
    )
  ][
    !is.na(value) & value > 0
  ][,
    c("variable", "subvar") := list(
      factor(
        variable,
        levels = c(
          "notes",
          "battery",
          "mood",
          "affect",
          "attitude",
          "language",
          "snsry_fncn",
          "comprhnsn"
        )
      ),
      data.table::nafill(as.numeric(subvar), "const", 99)
    )
  ][,
    let(
      label = data.table::fcase(
        variable == "mood"       , "Mood"                    ,
        variable == "affect"     , "Affect"                  ,
        variable == "attitude"   , "Attitude Toward Testing" ,
        variable == "language"   , "Language"                ,
        variable == "snsry_fncn" , "Sensory Function"        ,
        variable == "comprhnsn"  , "Comprehension"           ,
        variable == "battery"    , ""                        ,
        variable == "notes"      , ""                        ,
        default = NA_character_
      ) |>
        forcats::fct() |>
        forcats::fct_reorder(as.numeric(variable)),
      sublabel = data.table::fcase(
        variable == "mood" & subvar == 1       , "Happy/positive"                                                                                                             ,
        variable == "mood" & subvar == 2       , "Irritable/angry"                                                                                                            ,
        variable == "mood" & subvar == 3       , "Sad/Tearful"                                                                                                                ,
        variable == "mood" & subvar == 4       , "Frustrated"                                                                                                                 ,
        variable == "mood" & subvar == 5       , "Anxious"                                                                                                                    ,
        variable == "affect" & subvar == 1     , "Normal"                                                                                                                     ,
        variable == "affect" & subvar == 2     , "Restricted"                                                                                                                 ,
        variable == "affect" & subvar == 3     , "Flat/Blunted"                                                                                                               ,
        variable == "affect" & subvar == 4     , "Anxious"                                                                                                                    ,
        variable == "affect" & subvar == 5     , "Pessimistic"                                                                                                                ,
        variable == "attitude" & subvar == 1   , "Affect positive"                                                                                                            ,
        variable == "attitude" & subvar == 2   , "Affect negative (e.g., self-deprecating comments, more distressed about performance than most)"                             ,
        variable == "attitude" & subvar == 3   , "Poor frustration tolerance"                                                                                                 ,
        variable == "attitude" & subvar == 4   , "Disinhibited (e.g., starting tests before instructions finished, talkative/tangential)"                                     ,
        variable == "attitude" & subvar == 5   , "Poor or variable effort/cooperation (e.g., gave up easily, did not seem to be trying their best)"                           ,
        variable == "language" & subvar == 1   , "Fluent/articulate"                                                                                                          ,
        variable == "language" & subvar == 2   , "Word-finding difficulties"                                                                                                  ,
        variable == "language" & subvar == 3   , "Slow to respond"                                                                                                            ,
        variable == "language" & subvar == 4   , "Poor volume regulation"                                                                                                     ,
        variable == "language" & subvar == 5   , "Tangential"                                                                                                                 ,
        variable == "snsry_fncn" & subvar == 0 , "No sensory/motor problem observed"                                                                                          ,
        variable == "snsry_fncn" & subvar == 1 , "Wore hearing aids"                                                                                                          ,
        variable == "snsry_fncn" & subvar == 2 , "Suspected hearing problem (e.g., had difficulty hearing examiner or needed frequent repetition of stimuli or instructions)" ,
        variable == "snsry_fncn" & subvar == 3 , "Wore glasses"                                                                                                               ,
        variable == "snsry_fncn" & subvar == 4 , "Suspected vision problem (e.g., had difficulty reading or seeing test stimuli)"                                             ,
        variable == "snsry_fncn" & subvar == 5 , "Hand or other tremor/shaking"                                                                                               ,
        variable == "comprhnsn" & subvar == 1  , "Intact"                                                                                                                     ,
        variable == "comprhnsn" & subvar == 2  , "Needs repetition"                                                                                                           ,
        variable == "comprhnsn" & subvar == 3  , "Needs clarification"                                                                                                        ,
        variable == "battery"                  , "Test Battery Completed"                                                                                                     ,
        variable == "notes"                    , "Additional Notes"                                                                                                           ,
        default = NA_character_
      ) |>
        forcats::fct() |>
        forcats::fct_reorder2(variable, as.numeric(subvar)) |>
        forcats::fct_relevel("Additional Notes", after = 0L)
    )
  ][,
    c(
      "extra_info",
      "sublabel",
      "value",
      "subvar",
      "extra_info_2"
    ) := list(
      # extract_parentheses(sublabel),
      ifelse(
        grepl("\\(.*?\\)", sublabel),
        gsub(".*\\((.*?)\\).*", "\\1", sublabel),
        NA_character_
      ),
      factor(
        gsub("\\s*\\(.*?\\)", "", as.character(sublabel)),
        levels = gsub("\\s*\\(.*?\\)", "", levels(sublabel))
      ),
      data.table::fcase(
        variable %in% c("battery", "notes") & is.na(value) , NA_character_       ,
        variable == "checklist_complete" & value == 0      , "incomplete"        ,
        variable == "checklist_complete" & value == 1      , "unverified"        ,
        variable == "checklist_complete" & value == 2      , "complete"          ,
        variable == "battery" & value == 1                 , "checked"           ,
        variable == "battery" & value == 2                 , "unchecked"         ,
        variable == "notes"                                , as.character(value) ,
        value == 1                                         , "checked"           ,
        value == 0                                         , "unchecked"
      ),
      NULL,
      data.table::fcase(
        variable == "battery" & value == 1 , "All tests were completed"                                  ,
        variable == "battery" & value == 2 , "Some tests were not completed or only partially completed" ,
        default = NA_character_
      )
    )
  ]

  loc_res_vals <- c(
    "Hearing Impairment",
    "Distractions",
    "Interruptions",
    "Lack of effort of disinterest",
    "Fatigue",
    "Emotional Issues",
    "Unapproved Assistance",
    "Other"
  )

  respothx_lookup <- dat[, list(NACCID, VISITDATE, respothx)]

  respval <- data.table::melt(
    dat[,
      .SD,
      .SDcols = c(
        "NACCID",
        "VISITDATE",
        unlist(data.table::patterns(
          "respval",
          "loc_res",
          "respothx",
          cols = names(dat)
        ))
      )
    ],
    measure.vars = unlist(data.table::patterns(
      "respval",
      "loc_res",
      cols = names(dat)
    ))
  )[
    !is.na(value) & value > 0
  ][,
    `:=`(
      label = data.table::fcase(
        variable == "respval"       , "Response Validity"                                   ,
        grepl("^loc_res", variable) , "What makes this participant's responses less valid?"
      ),
      sublabel = data.table::fcase(
        variable == "respval"     , "How valid do you think the participant's responses are?" ,
        variable == "loc_res___1" , loc_res_vals[1]                                           ,
        variable == "loc_res___2" , loc_res_vals[2]                                           ,
        variable == "loc_res___3" , loc_res_vals[3]                                           ,
        variable == "loc_res___4" , loc_res_vals[4]                                           ,
        variable == "loc_res___5" , loc_res_vals[5]                                           ,
        variable == "loc_res___6" , loc_res_vals[6]                                           ,
        variable == "loc_res___7" , loc_res_vals[7]                                           ,
        variable == "loc_res___8" , loc_res_vals[8]
      ),
      extra_info_2 = data.table::fcase(
        variable == "loc_res___8" & value == 1 , respothx                                                              ,
        variable == "respval" & value == 1     , "probably accurate indication of participant's cognitive abilities"   ,
        variable == "respval" & value == 2     , "possibly inaccurate indication of participant's cognitive abilities" ,
        variable == "respval" & value == 3     , "probably inaccurate indication of participant's cognitive abilities" ,
        default = NA_character_
      ),
      value = data.table::fcase(
        variable == "respval" & value == 1 , "Very valid"         ,
        variable == "respval" & value == 2 , "Questionably valid" ,
        variable == "respval" & value == 3 , "Invalid"            ,
        value == 0                         , "unchecked"          ,
        value == 1                         , "checked"
      )
    )
  ][,
    respothx := NULL
  ]

  out <- data.table::rbindlist(
    list(wadrc_behavioral_obs, respval),
    fill = TRUE
  )[,
    `:=`(
      VISITDATE = factor(
        VISITDATE,
        levels = sort(unique(VISITDATE), decreasing = TRUE)
      ),
      label = forcats::fct_relevel(
        label,
        "Response Validity",
        "What makes this participant's responses less valid?",
        after = 1L
      ),
      sublabel = forcats::fct_relevel(
        sublabel,
        rev(loc_res_vals),
        after = Inf
      ) #,
      # value = purrr::map2(value, extra_info_2, \(x, y) {
      #   list(
      #     value = x,
      #     extra_info = y
      #   )[c(T, !is.na(y))]
      # })
    )
  ][,
    extra_info_2 := NULL
  ]

  out
}

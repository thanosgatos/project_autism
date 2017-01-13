participants_age <- participants %>%
  mutate(
    age1 = ifelse(
      age_min <= 4 & age_max <= 4,
      "1",
      ifelse(
        age_min <= 4 & (age_max >= 5 & age_max <= 12),
        "1;2",
        ifelse(
          age_min <= 4 & (age_max >= 13 & age_max <= 19),
          "1;2;3",
          ifelse(
            age_min <= 4 & age_max >= 20,
            "1;2;3;4",
            ifelse(
              (age_min >= 5 & age_min <= 12) & (age_max >= 5 & age_max <= 12),
              "2",
              ifelse(
                (age_min >= 5 & age_min <= 12) &
                  (age_max >= 13 & age_max <= 19),
                "2;3",
                ifelse(
                  (age_min >= 5 & age_min <= 12) & age_max >= 20,
                  "2;3;4",
                  ifelse(
                    (age_min >= 13 & age_min <= 19) &
                      (age_max >= 13 & age_max <= 19),
                    "3",
                    ifelse(
                      (age_min >= 13 & age_min <= 19) & age_max >= 20,
                      "3;4",
                      ifelse(
                        age_min >= 20 & age_max >= 20,
                        "4",
                        ifelse(
                          age_min > age_max,
                          "AGE_RANGE_ERROR",
                          ifelse(
                            is.na(age_min) & is.na(age_max) &
                              is.na(age_mean) & is.na(dev_stage),
                            "missing",
                            NA
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    age2 = ifelse(
      is.na(age_min) & is.na(age_max) & age_mean < 5,
      "1",
      ifelse(
        is.na(age_min) & is.na(age_max) &
          age_mean > 5 & age_mean < 13,
        "2",
        ifelse(
          is.na(age_min) & is.na(age_max) &
            age_mean > 13 & age_mean < 20,
          "3",
          ifelse(
            is.na(age_min) & is.na(age_max) &
              age_mean > 20,
            "4",
            ifelse(
              is.na(age_min) & is.na(age_max) &
                is.na(age_mean) & is.na(dev_stage),
              "missing",
              NA
            )
          )
        )
      )
    ),
    age3 = ifelse(
      is.na(age_min) & is.na(age_max) &
        is.na(age_mean) &
        dev_stage %in% c("infant", "toddler",
                         "preschooler"),
      "1",
      ifelse(
        is.na(age_min) & is.na(age_max) &
          is.na(age_mean) &
          dev_stage == "child",
        "2",
        ifelse(
          is.na(age_min) & is.na(age_max) &
            is.na(age_mean) &
            dev_stage == "adolescent",
          "3",
          ifelse(
            is.na(age_min) & is.na(age_max) &
              is.na(age_mean) &
              dev_stage %in% c("young adult",
                               "adult",
                               "middle age",
                               "old age"),
            "4",
            ifelse(
              is.na(age_min) & is.na(age_max) &
                is.na(age_mean) & is.na(dev_stage),
              "missing",
              NA
            )
          )
        )
      )
    ),
    age_to_split = ifelse(
      is.na(age1) & is.na(age2) & is.na(age3),
      NA,
      ifelse(
        !is.na(age1),
        age1,
        ifelse(
          !is.na(age2),
          age2,
          ifelse(
            !is.na(age3),
            age3,
            "ERROR"
          )
        )
      )
    )
  ) %>%
  mutate(age = str_split(age_to_split, ";")) %>%
  unnest(age) %>%
  mutate(age = fct_explicit_na(factor(age, levels = 1:4,
                                      labels = c("preschooler", "child",
                                                 "adolescent", "adult")),
                               na_level = "not specified"))

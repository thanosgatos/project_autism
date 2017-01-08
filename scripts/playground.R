#------------------------------------------------------------------------------#

technique <- gs_read(ss = analysis_sheet, ws = "technique", skip = 1,
                     col_names = TRUE, n_max = 210)

#------------------------------------------------------------------------------#

time = factor(
  time,
  levels = 1:2,
  labels = c("one-shot", "longitudinal")
)

1 one-shot
2 longitudinal

#------------------------------------------------------------------------------#

place = factor(
  place,
  levels = 1:8,
  labels = c("autism school", "autism center", "research lab", "home", "familiar setting", "unfamiliar setting", "mixture", "other")
)



1 autism school
2 autism center
3 research lab
4 home
5 familiar setting not specified above
6 unfamiliar setting
7 mixture
8 other (specify with a comment)

#------------------------------------------------------------------------------#

View(
  technique %>%
    rename(
      hci_exp = HCI_exp
    )
)

#------------------------------------------------------------------------------#

designer_role1 = factor(
  designer_role1,
  levels = 1:7,
  labels = c("info source", "ideator", "user", "data collector", "facilitator", "moderator", "other")
)



1 info source (e.g talking about how to design sth based on the user's requirements)
2 ideator / prototyper
3 user / tester
4 data collector
5 facilitator
6 moderator
7 other (specify with a comment)



#------------------------------------------------------------------------------#

teacher_role1 = factor(
  teacher_role1,
  levels = 1:12,
  labels = c("info source", "representative", "consent provider", "data collector", "assessment", "user", "ideator", "facilitator", "moderator", "first aid", "mixture", "other")
)



1 info source (e.g talking about ASD)
2 representative (e.g if the ASD is nonverbal)
3 consent provider
4 data collector
5 assessment (clinical etc)
6 user / tester
7 ideator / prototyper
8 facilitator
9 moderator
10 first aid (passive yet present in case of a tantrum etc)
11 mixture (specify with a comment)
12 other


#------------------------------------------------------------------------------#
res1_role1 = factor(
  res1_role1,
  levels = 1:7,
  labels = c("data collection", "facilitation", "moderation", "user",
             "participant", "trainer", "other")
)


#------------------------------------------------------------------------------#
for (group in 1:4) {

  technique <- technique %>%
    mutate(
      sprintf("group%d_role", group) = factor(
        sprintf("group%d_role", group),
        levels = 1:4,
        labels = c("user/tester", "participant", "information source", "other")
      ),
      sprintf("group%d_density", group) = factor(
        sprintf("group%d_density", group),
        levels = 1:4,
        labels = c("individually", "partly", "all", "not specified")
      ),
      sprintf("group%d_density", group) =
    )




  technique %>%
    filter(
      sprintf("group%d", group) == 1 & is.na(sprintf("group%d_density", group))
    ) %>%
    mutate(
      sprintf("group%d_density", group) =
    )

}


for (i in 1:7) {
  test <- technique %>%
    filter(
      sprintf("group%d", i) == 1 & is.na(sprintf("group%d_density", i))
    )
}


y <- c(1, 2, NA, NA, 5)
z <- c(NA, NA, 3, 4, 5)
coalesce(y, z)



for (i in 1:4) {
  technique[[sprintf("group%d_role", i)]] <- factor(
    sprintf("group%d_role", i),
    levels = 1:4,
    labels = c("user", "participant", "information source", "other")
  )
}



technique <- technique %>%
  mutate_(
     =
  )








for (i in 1:4) {
  transform_group_vars(i)
}

transform_group_vars(1)


View(
  technique %>%
    filter(group2 == 1 & is.na(group2_density))
)

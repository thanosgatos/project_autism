#------------------------------------------------------------------------------#

# Getting tables with frequencies from a single data.frame
technique %>%
  filter(technique_phase == "RE") %>%
  select(teacher_role1) %>%
  table()

technique %>%
  filter(
    technique_phase == "RE",
    technique_type == "deployment"
  ) %>%
  select(res2_role2) %>%
  table()

#------------------------------------------------------------------------------#

# Querying two tables at the same time:
# 1. getting info from technique
# 2. using it to get the required info from participants

test <- technique %>%
  filter(
    technique_type == "interview",
    group1 == 1 | group2 == 1 | group3 == 1 | group4 == 1 | group5 == 1 |
      group6 == 1 | group7 == 1
  ) %>%
  select(paper_id, starts_with("group"), -ends_with("role"),
         -ends_with("density")) %>%
  gather(group_id, status, 2:8) %>%
  filter(status == 1) %>%
  select(-status) %>%
  mutate(group_id = as.integer(str_sub(group_id, -1, -1)))


View(
  participants %>%
    semi_join(test, by = c("paper_id", "group_id"))
)



#------------------------------------------------------------------------------#

technique <- gs_read(ss = analysis_sheet, ws = "technique", skip = 1,
                     col_names = TRUE, n_max = 210)

participants <- gs_read(ss = analysis_sheet, ws = "participants", skip = 1,
                        col_names = TRUE, n_max = 130)

#------------------------------------------------------------------------------#


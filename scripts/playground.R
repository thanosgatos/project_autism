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

mixed_groups <- participants %>%
  filter(
    ASD > 0 | is.na(ASD) | HFA > 0 | is.na(HFA) | MFA > 0 | is.na(MFA) |
      LFA > 0 | is.na(LFA) | severe > 0 | is.na(severe),
    TD > 0 | is.na(TD) | PDD > 0 | is.na(PDD) | ID > 0 | is.na(ID) |
      dyslexia > 0 | is.na(dyslexia) | ADHD > 0 | is.na(ADHD) | other > 0 |
      is.na(other)
  )

#------------------------------------------------------------------------------#

# Question 1

# Which types of techniques are used with autistics?
# Check participants data frame: ASD, HFA, MFA, LFA, severe

# paper/group_id's that have autistics
q1_inter_participants <- participants %>%
  filter(
    ASD > 0 | is.na(ASD) | HFA > 0 | is.na(HFA) | MFA > 0 | is.na(MFA) |
      LFA > 0 | is.na(LFA) | severe > 0 | is.na(severe)
  ) %>%
  select(paper_id, group_id) %>%
  arrange(paper_id, study_id, technique_id, group_id)
# select(paper_id, group_id, ASD, HFA, MFA, LFA, severe)

# number of groups with which studies are done
q1_inter_technique <- technique %>%
  select(paper_id, study_id, technique_id, technique_type, group1, group2,
         group3, group4, group5, group6, group7) %>%
  gather(group_id, status, -paper_id, -study_id, -technique_id,
         -technique_type) %>%
  filter(status == 1) %>%
  mutate(group_id = parse_number(group_id)) %>%
  select(-status) %>%
  arrange(paper_id, study_id, technique_id, group_id)

technique_groups <- technique %>%
  select(paper_id, study_id, technique_id, technique_type, group1, group2,
         group3, group4, group5, group6, group7) %>%
  gather(group_id, status, -paper_id, -study_id, -technique_id,
         -technique_type) %>%
  filter(status == 1) %>%
  mutate(group_id = parse_number(group_id)) %>%
  select(-status) %>%
  arrange(paper_id, study_id, technique_id, group_id)


# %>%
#   group_by(paper_id, study_id, technique_id) %>%
#   distinct(.keep_all = TRUE)
#
# count() %>%
# arrange(-n)

q1_answer <- q1_inter_technique %>%
  semi_join(q1_inter_participants, by = c("paper_id", "group_id")) %>%
  arrange(paper_id, study_id, technique_id, group_id)

View(q1_answer)
View(table(q1_answer$technique_type))

#------------------------------------------------------------------------------#

# Question 2
#
# Which techniques are used with participants?







#------------------------------------------------------------------------------#

# Querying two tables at the same time:
# 1. getting info from technique
# 2. using it to get the required info from participants

library(stringr)

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


participants_test <- participants %>%
  semi_join(test, by = c("paper_id", "group_id"))



#------------------------------------------------------------------------------#

bibliography <- gs_read(ss = analysis_sheet, ws = "bibliography", skip = 1,
                        col_names = TRUE, n_max = 154)

data_collection <- gs_read(ss = analysis_sheet, ws = "data_collection", skip = 1,
                           col_names = TRUE, n_max = 247)

design <- gs_read(ss = analysis_sheet, ws = "design", skip = 1,
                        col_names = TRUE, n_max = 91)

intervention <- gs_read(ss = analysis_sheet, ws = "intervention", skip = 1,
                    col_names = TRUE, n_max = 93)

participants <- gs_read(ss = analysis_sheet, ws = "participants", skip = 1,
                        col_names = TRUE, n_max = 154)

platform <- gs_read(ss = analysis_sheet, ws = "platform", skip = 1,
                    col_names = TRUE, n_max = 95)

technique <- gs_read(ss = analysis_sheet, ws = "technique", skip = 1,
                     col_names = TRUE, n_max = 247)


#------------------------------------------------------------------------------#

# check for duplicates
technique %>%
  group_by(paper_id, study_id, technique_id) %>%
  filter(n() > 1) %>%
  select(1:4)

data_collection_meth %>%
  group_by(paper_id, study_id, technique_id) %>%
  filter(n() >1)

#------------------------------------------------------------------------------#

participants %>%
  filter(
    ASD > 0 | is.na(ASD) | HFA > 0 | is.na(HFA) | MFA > 0 | is.na(MFA) |
      LFA > 0 | is.na(LFA) | severe > 0 | is.na(severe) | TD > 0 | is.na(TD) |
      PDD > 0 | is.na(PDD) | ID > 0 | is.na(ID) | dyslexia > 0 |
      is.na(dyslexia) | ADHD > 0 | is.na(ADHD) | other > 0 | is.na(other)
  ) %>%
  select(paper_id, group_id)


sd1_participants <- participants %>%
  filter(paper_id == "SD1")
sd1_technique <- technique %>%
  filter(paper_id == "SD1")



technique %>%
  filter(group1 == 1 | group2 == 1 | group3 == 1 | group4 == 1 | group5 == 1 |
           group6 == 1 | group7 == 1) %>%
  select(technique_type) %>%
  table()

View(
  technique %>%
    select(paper_id, study_id, technique_id, technique_type, group1, group2,
           group3, group4, group5, group6, group7) %>%
    gather(group_id, status, -paper_id, -study_id, -technique_id,
           -technique_type) %>%
    filter(status == 1) %>%
    mutate(group_id = parse_number(group_id)) %>%
    select(-status) %>%
    arrange(paper_id, group_id)
)

q1_answer %>%
  group_by(paper_id, study_id, technique_id, group_id) %>%
  filter(n() > 1)



# # Use the for loop above in the final version
# bibliography <- gs_read(ss = analysis_sheet, ws = "bibliography", skip = 1,
#                         col_names = TRUE, n_max = 98)
#
# data_collection <- gs_read(ss = analysis_sheet, ws = "data_collection", skip = 1,
#                            col_names = TRUE, n_max = 254)
#
# design <- gs_read(ss = analysis_sheet, ws = "design", skip = 1,
#                   col_names = TRUE, n_max = 95)
#
# intervention <- gs_read(ss = analysis_sheet, ws = "intervention", skip = 1,
#                         col_names = TRUE, n_max = 96)
#
# participants <- gs_read(ss = analysis_sheet, ws = "participants", skip = 1,
#                         col_names = TRUE, n_max = 161)
#
# platform <- gs_read(ss = analysis_sheet, ws = "platform", skip = 1,
#                     col_names = TRUE, n_max = 98)
#
# technique <- gs_read(ss = analysis_sheet, ws = "technique", skip = 1,
#                      col_names = TRUE, n_max = 254)

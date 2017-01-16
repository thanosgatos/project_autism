
# With groups
technique_groups %>%
  group_by(paper_id, study_id, technique_id) %>%
  distinct()
## 181

# NOt with groups
technique %>%
  anti_join(technique_groups, by = c("paper_id", "study_id", "technique_id")) %>%
  group_by(paper_id, study_id, technique_id)
## 72

# With actors
technique_actors %>%
  group_by(paper_id, study_id, technique_id) %>%
  distinct()
## 237

# NOT with actors
technique %>%
  anti_join(technique_actors, by = c("paper_id", "study_id", "technique_id")) %>%
  group_by(paper_id, study_id, technique_id)
## 42

# With ASD+mixed
technique_asd %>%
  group_by(paper_id, study_id, technique_id) %>%
  distinct()
## 173


# Common ALL groups - Actors
technique_actors %>%
  semi_join(technique_groups, by = c("paper_id", "study_id", "technique_id")) %>%
  group_by(paper_id, study_id, technique_id) %>%
  select(paper_id, study_id, technique_id)
## 165


# ALL participants ----------

# Only actors
technique_actors %>%
  anti_join(technique_groups, by = c("paper_id", "study_id", "technique_id")) %>%
  group_by(paper_id, study_id, technique_id)
## 72

# Only participants
technique_groups %>%
  anti_join(technique_actors, by = c("paper_id", "study_id", "technique_id")) %>%
  group_by(paper_id, study_id, technique_id)
## 16


# ASD+mixed -------------

# Common ASD+mixed - actors
technique_actors %>%
  semi_join(technique_asd, by = c("paper_id", "study_id", "technique_id")) %>%
  group_by(paper_id, study_id, technique_id) %>%
  select(paper_id, study_id, technique_id)
## 157

# Only participants
technique_asd %>%
  anti_join(technique_actors, by = c("paper_id", "study_id", "technique_id")) %>%
  group_by(paper_id, study_id, technique_id)
## 16

# Only actors
technique_actors %>%
  anti_join(technique_asd, by = c("paper_id", "study_id", "technique_id")) %>%
  group_by(paper_id, study_id, technique_id)
## 80





View(
  anti_join(x, x2, by = c("paper_id", "study_id", "technique_id"))
)




technique_no_participants %>%
  group_by(paper_id, study_id, technique_id)

technique %>%
  anti_join(technique_groups) %>%
  group_by(paper_id, study_id, technique_id) %>%
  select(paper_id, study_id, technique_id)

technique %>%
  filter(
    group1 == 0 & group2 == 0 & group3 == 0 & group4 == 0 & group5 == 0 &
      group6 == 0 & group7 == 0 & (res1 > 0 | res2 > 0 | teacher > 0 |
      caregiver > 0 | parent > 0 | therapist > 0 | aut_exp > 0 | designer > 0 |
      HCI_exp > 0 | virtual > 0 | other > 0)
  )


technique %>%
  filter(
    group1 == 0 & group2 == 0 & group3 == 0 & group4 == 0 & group5 == 0 &
      group6 == 0 & group7 == 0 & res1 == 0 & res2 == 0 & teacher == 0 &
      caregiver == 0 & parent == 0 & therapist == 0 & aut_exp == 0 &
      designer == 0 & HCI_exp == 0 & virtual == 0 & other == 0)



anti_join(x, y)

anti_join(y, x)

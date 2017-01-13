# paper/study with multiple techniques

multiple_techniques <- technique %>%
  group_by(paper_id, study_id) %>%
  summarise(num_tech = n_distinct(technique_id)) %>%
  filter(num_tech > 1)

technique %>%
  semi_join(multiple_techniques, by = c("paper_id", "study_id"))

View(
  technique %>%
    semi_join(multiple_techniques, by = c("paper_id", "study_id")) %>%
    select(paper_id, study_id, technique_type) %>%
    mutate(status = 1) %>%
    spread(technique_type, status)
)


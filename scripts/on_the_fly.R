#------------------------------------------------------------------------------#

View(
  participants %>%
    gather(diagnosis, num_of_participants, c(ASD:other)) %>%
    arrange(paper_id, group_id)
)

#------------------------------------------------------------------------------#

# Max num of studies within a paper
View(
  technique %>%
    group_by(paper_id) %>%
    summarise(n = n_distinct(study_id)) %>%
    arrange(-n)
)

#------------------------------------------------------------------------------#

# Max num of techniques withing a study
View(
  technique %>%
    group_by(paper_id, study_id) %>%
    summarise(n = n_distinct(technique_id)) %>%
    arrange(-n)
)

#------------------------------------------------------------------------------#

# How many papers have X studies?
z <- technique %>%
  group_by(paper_id) %>%
  summarise(n = n_distinct(study_id)) %>%
  arrange(-n)

table(z$n)

#------------------------------------------------------------------------------#

# How many groups have X diagnosis participants
# check quantitative_analysis.Rmd for participants_temp
table(participants_temp$diagnosis)

#------------------------------------------------------------------------------#


View(
  participants %>%
    gather(diagnosis, num_diagnosis, c(ASD:other)) %>%
    filter(num_diagnosis > 0 | is.na(num_diagnosis)) %>%
    arrange(paper_id, group_id)
)

participants %>%
  group_by(paper_id, group_id)

#------------------------------------------------------------------------------#

#--- participants ---#

# How many participants' groups have autistics or not?
z <- participants_temp %>%
  group_by(paper_id, group_id) %>%
  distinct(.keep_all = TRUE)

table(z$diagnosis_type)

# Should add up to the number of distinct paper/group_id
participants %>% distinct() %>% nrow()

# Check: Groups with no participants?
participants %>%
  filter(ASD == 0 & HFA == 0 & MFA == 0 & LFA == 0 & severe == 0 & TD == 0 &
           PDD == 0 & ID == 0 & dyslexia == 0 & ADHD == 0 & other == 0)

w <- participants_diagnosis %>%
  group_by(paper_id, group_id)

table(w$diagnosis_type)

ggplot(w)+geom_bar()


q <- participants_diagnosis %>%
  group_by(paper_id, group_id) %>%
  distinct(.keep_all = TRUE)

participants_diagnosis %>%
  semi_join(q, by = c("paper_id", "group_id"))

# Number of paper/group_id that have more than
participants_diagnosis %>%
  group_by(paper_id, group_id) %>%
  filter(n() > 1)

# List of number of diagnosis types (n) the groups above have
participants_diagnosis %>%
  group_by(paper_id, group_id) %>%
  filter(n() > 1) %>%
  summarise(n = n()) %>%
  arrange(-n)

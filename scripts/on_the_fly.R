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

#Checking number of diagnosis and diagnosis_type per paper/group
View(
  participants_temp %>%
    group_by(paper_id, group_id) %>%
    summarise(
      n_diag = n_distinct(diagnosis),
      n_type = n_distinct(diagnosis_type)
    )
)

# Check: Groups with no participants?
participants %>%
  group_by(paper_id, group_id) %>%
  filter(ASD == 0 & HFA == 0 & MFA == 0 & LFA == 0 & severe == 0 & TD == 0 &
           PDD == 0 & ID == 0 & dyslexia == 0 & ADHD == 0 & other == 0)
## IEEE3 -- We should check this paper again!



participants %>%
  group_by(paper_id, group_id) %>%
  filter(paper_id == "IEEE3")

x <- participants_temp %>%
  group_by(paper_id, group_id) %>%
  summarise(
    n_diag = n_distinct(diagnosis),
    n_type = n_distinct(diagnosis_type)
  )

y <- participants %>%
  filter(!(paper_id == "IEEE3" & (group_id == 2 | group_id == 3))) %>%
  select(ASD:other) %>%
  mutate_all(funs(if_else(. > 0 | is.na(.), 1, 0))) %>%
  rowSums()

which(y != x$n_diag)


#creating a mixed diagnosis_type
participants_temp %>%
  left_join(participants_mixed_index, by = c("paper_id", "group_id"))

View(
  participants_temp %>%
    left_join(participants_mixed_index, by = c("paper_id", "group_id"))
)

View(
  participants_temp %>%
    left_join(participants_mixed_index, by = c("paper_id", "group_id")) %>%
    mutate(
      diagnosis_type = ifelse(diagnosis_mixed == 1, "mixed", diagnosis_type)
    )
)


participants_temp %>%
  group_by(paper_id, group_id) %>%
  select(paper_id, group_id, diagnosis, diagnosis_type, diagnosis_mixed) %>%
  distinct(.keep_all = TRUE) %>%
  select(-diagnosis, - diagnosis_mixed)


# What is the difference?
ggplot(participants_diagnosis) +
  geom_bar(aes(diagnosis_type))

participants_diagnosis %>%
  group_by(paper_id, group_id) %>%
  ggplot() +
  geom_bar(aes(diagnosis_type))

participants_diagnosis %>%
  group_by(paper_id, group_id)

## We have 152 combos of paper/group


# Check for multiple entries per paper/group for diagnosis_type
participants_temp %>%
  group_by(paper_id, group_id) %>%
  summarise(n_types = n_distinct(diagnosis_type)) %>%
  filter(n_types > 1)

# Check for multiple entries per paper/group for diagnosis
yy <- participants_temp %>%
  group_by(paper_id, group_id) %>%
  summarise(n_diagnosis = n_distinct(diagnosis)) %>%
  filter(n_diagnosis > 1) %>%
  left_join(
    participants_temp %>%
      group_by(paper_id, group_id) %>%
      summarise(n_types = n_distinct(diagnosis_type)) %>%
      filter(n_types > 1),
    by = c("paper_id", "group_id")
  ) %>%
  filter(!is.na(n_types))

## Check the results




# How many participants' groups have autistics or not?
z <- participants_temp %>%
  group_by(paper_id, group_id) %>%
  distinct(.keep_all = TRUE)

table(z$diagnosis_type)

# Should add up to the number of distinct paper/group_id
participants %>% distinct() %>% nrow()


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


#------------------------------------------------------------------------------#

#--- data_collection ---#


data_collection_meth %>%
  group_by(paper_id, study_id, technique_id)

data_collection_meth %>%
  group_by(paper_id, study_id, technique_id) %>%
  distinct(.keep_all = TRUE)

View(
  data_collection_meth %>%
    group_by(paper_id, study_id, technique_id) %>%
    filter(n() >1)
)

# The sum of the method counts
z <- data_collection_meth %>%
  group_by(method) %>%
  count()
sum(z$n)


# Check if any paper/study has more than 1 method_type
data_collection_meth %>%
  group_by(paper_id, study_id) %>%
  summarise(n = n_distinct(method_type)) %>%
  filter(n > 1)

data_collection_meth %>%
  group_by()

# Check if any paper/study/technique/method has duplicates
data_collection_meth %>%
  group_by(paper_id, study_id, technique_id, method) %>%
  filter(n() > 1)

# Keep only distinct paper/study/technique/method
data_collection_meth %>%
  group_by(paper_id, study_id, technique_id, method) %>%
  distinct(.keep_all = TRUE)

# The count is 241 - NOT 242!
data_collection_meth %>%
  group_by(paper_id, study_id, technique_id, method) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup() %>%
  group_by(method) %>%
  count() %>% arrange(-n)

# Check if any paper/study/technique has multiple method_type -> NO!
data_collection_meth %>%
  group_by(paper_id, study_id, technique_id) %>%
  summarise(types = n_distinct(method_type)) %>%
  filter(types > 1)

data_collection_meth %>%
  group_by(paper_id, study_id, technique_id) %>%
  distinct(.keep_all = TRUE)


# Number of data collectors per paper/study/technique
data_collection %>%
  group_by(paper_id, study_id, technique_id) %>%
  filter(n() > 1)

# paper/study/technique with multiple lines
View(
  data_collection %>%
    group_by(paper_id, study_id, technique_id) %>%
    filter(n() > 1)
)
## Marked pairs of lines in data_collection WS to be checked

View(
  data_collection %>%
    group_by(paper_id, study_id, technique_id) %>%
    summarize(n_collectors = n_distinct(data_collector)) %>%
    filter(n_collectors > 1)
)

data_collectors_temp %>%
  group_by(paper_id, study_id, technique_id, data_collector) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup() %>%
  group_by(data_collector) %>%
  count()


#------------------------------------------------------------------------------#

#--- design ---#

# Are there any papers with NO product?
product_names <- names(design)[-1]

design %>%
  filter(
    app == 0 & program == 0 & interface == 0 & game == 0 & serious_game == 0 &
      social_network == 0 & virtual_character == 0 & schedule == 0 &
      experience == 0 & service == 0 & algorithm == 0 & data_struct == 0 &
      data_viz == 0 & toy == 0 & puzzle == 0 & furniture == 0 &
      environment == 0 & other == 0
  )
## YES! -> 7

# Number of "product" from all papers
design %>%
  select(-paper_id) %>%
  colSums() %>%
  sum()
## Should be the same as the num of rows of design_type - OK!


#------------------------------------------------------------------------------#

#--- intervention ---#

# I need Doa to check the cols (is method wrongly placed here?) and group vars

# Check whether there are papers with NO intervention
x <- intervention %>%
  select(-c(paper_id, method)) %>%
  rowSums()
sum(x == 0)
## 13 papers
## their indices are:
which(x == 0)

View(intervention[which(x == 0), ])
## All, apart from IEEE3, have method = 1. IEEE3 has ALL zeros. Corrected!



#------------------------------------------------------------------------------#

#--- technique ---#

technique %>%
  select(paper_id, study_id, technique_id, other, other_role1, other_role2) %>%
  gather(role_id, other_role, other:other_role2)

View(
  technique %>%
    arrange(paper_id, study_id, technique_id) %>%
    gather(group_id, status, c(group1, group2, group3, group4, group5, group6,
                               group7))
)
## Cannot use this for the whole table -> roles etc are only in the 1st row for
## each paper/study/technique


technique %>%
  select(paper_id, study_id, technique_id, technique_type, group1, group2,
         group3, group4, group5, group6, group7) %>%
  gather(group_id, status, -paper_id, -study_id, -technique_id,
         -technique_type) %>%
  filter(status == 1) %>%
  mutate(group_id = parse_number(group_id)) %>%
  select(-status) %>%
  arrange(paper_id, study_id, technique_id, group_id)


# Q1

caregivers <- c("teacher", "therapist", "caregiver", "parent", "aut_exp",
                "virtual")


technique %>%
  filter(res1 == 0 & res2 == 0) %>%
  select(
    paper_id, study_id, technique_id, contains("teacher_role"),
    contains("therapist_role"), contains("caregiver_role"),
    contains("parent_role"), contains("aut_exp_role"), contains("virtual_role")
  ) %>%
  gather(actor, role, -c(paper_id, study_id, technique_id)) %>%
  filter(!is.na(role)) %>%
  mutate(
    actor = str_sub(actor, 1, -7),
    actor = factor(actor, levels = names(sort(table(actor), decreasing = TRUE))),
    role = factor(role, levels = names(sort(table(role), decreasing = TRUE)))
  ) %>%
  arrange(paper_id, study_id, technique_id)




technique %>%
  filter(res1 == 0 & res2 == 0) %>%
  select(
    contains("teacher_role"),
    contains("therapist_role"), contains("caregiver_role"),
    contains("parent_role"), contains("aut_exp_role"), contains("virtual_role")
  ) %>%
  is.na() %>%
  colSums()




View(
  technique %>%
    filter(res1 == 0 & res2 == 0) %>%
    select(
      paper_id, study_id, technique_id, contains("teacher_role"),
      contains("therapist_role"), contains("caregiver_role"),
      contains("parent_role"), contains("aut_exp_role"), contains("virtual_role")
    ) %>%
    gather(actor, role, -c(paper_id, study_id, technique_id)) %>%
    filter(!is.na(role)) %>%
    mutate(
      actor = str_sub(actor, 1, -7)
    )
)

# Q2

technique %>%
  mutate(
    place_type = ifelse(
      place %in% c("autism school", "autism center", "home", "familiar setting"),
      "familiar setting",
      ifelse(
        place %in% c("unfamiliar setting", "research lab"),
        "unfamiliar setting",
        ifelse(
          place %in% c("mixture", "other"),
          "mixture",
          NA
        )
      )
    ),
    place_type = fct_explicit_na(
      factor(place_type, levels = names(sort(table(place_type),
                                             decreasing = TRUE)))
    )
  )


View(
  technique %>%
    mutate(
      place_type = ifelse(
        place %in% c("autism school", "autism center", "home", "familiar setting"),
        "familiar setting",
        ifelse(
          place %in% c("unfamiliar setting", "research lab"),
          "unfamiliar setting",
          ifelse(
            place %in% c("mixture", "other"),
            "mixture",
            NA
          )
        )
      ),
      place_type = fct_explicit_na(
        factor(place_type, levels = names(sort(table(place_type),
                                               decreasing = TRUE))),
        na_level = "not specified"
      )
    ) %>%
    select(paper_id, study_id, technique_id, place, place_type)
)



technique %>%
  select(paper_id, study_id, technique_id, technique_type, technique_phase,
         group1, group2, group3, group4, group5, group6, group7) %>%
  gather(group_id, status, -paper_id, -study_id, -technique_id,
         -technique_type, -technique_phase) %>%
  filter(status == 1) %>%
  mutate(group_id = parse_number(group_id)) %>%
  select(-status) %>%
  arrange(paper_id, study_id, technique_id, group_id)

z <- technique_asd %>%
  group_by(paper_id, group_id) %>%
  summarise(num_tech = n_distinct(technique_id))

q <- technique_asd %>%
  group_by(paper_id, group_id) %>%
  summarise(num_tech = n_distinct(technique_id) * n_distinct(study_id))


# How many time each technique appears
technique %>%
  group_by(technique_phase) %>%
  count() %>%
  arrange(-n)

technique_asd %>%
  group_by(technique_phase) %>%
  count() %>%
  arrange(-n)

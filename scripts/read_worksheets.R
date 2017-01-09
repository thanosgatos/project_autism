# Load packages ----------------------------------------------------------------
library(tidyverse)
library(googlesheets)
library(forcats)



# Import the Google Sheets -----------------------------------------------------
analysis_sheet <- gs_title("Doga - Analysis")
worksheet_names <- gs_ws_ls(analysis_sheet)
worksheet_names_kept <- worksheet_names[!worksheet_names %in%
                                          c("product", "overall")]


# Read each worksheet as a separate data frame
for (ws in worksheet_names_kept) {
  assign(sprintf("%s", ws),
         gs_read(ss = analysis_sheet, ws = sprintf("%s", ws), skip = 1,
                 col_names = TRUE, n_max = 50))
  # Add a delay between calls to the API to avoid an error - 6 sec and up
  # (https://github.com/jennybc/googlesheets/issues/214)
  Sys.sleep(6)
}


# Variable type ransformations and new variable generation
bibliography <- bibliography %>%
  mutate(
    paper_title = as.character(paper_title),
    year = as.integer(year),
    first_surname = as.character(first_surname),
    second_surname = as.character(second_surname),
    third_surname = as.character(third_surname),
    fourth_surname = as.character(fourth_surname),
    fifth_surname = as.character(fifth_surname),
    database = as.character(database)
  )

data_collection <- data_collection %>%
  mutate(
    data_collector = factor(data_collector, exclude = NULL),
    mixed = as.integer(ifelse(qualitative == 1 & quantitative == 1, 1, 0))
  )

# design needs no changes

# intervention needs no changes

participants <- participants %>%
  mutate(
    dev_stage = factor(
      dev_stage,
      levels = 1:9,
      labels = c("infant", "toddler", "preschooler", "child", "adolescent",
                 "young adult", "adult", "middle age", "old age"),
      ordered = TRUE
    ),
    recruitment = fct_explicit_na(
      factor(recruitment, labels = c("closed community", "open community")),
      na_level = "not specified"
    )
  )

# platform needs no changes

technique <- technique %>%
  mutate(
    technique_type = factor(
      technique_type,
      levels = 1:19,
      labels = c("interview", "field study", "free play","participatory design",
                 "user testing", "pretest", "posttest", "deployment", "survey",
                 "Doga is stupid", "focus group", "workshop","prototyping",
                 "brainstorming", "probe", "digital ethnography",
                 "expert evaluation", "card sorting", "other")
    ),
    technique_phase = factor(
      technique_phase,
      levels = 1:7,
      labels = c("requirements", "design", "evaluation", "RD", "RE", "DE", "RDE")
    ),
    group1_role = factor(
      group1_role,
      levels = 1:4,
      labels = c("user", "participant", "information source", "other")
    ),
    group2_role = factor(
      group2_role,
      levels = 1:4,
      labels = c("user", "participant", "information source", "other")
    ),
    group3_role = factor(
      group3_role,
      levels = 1:4,
      labels = c("user", "participant", "information source", "other")
    ),
    group4_role = factor(
      group4_role,
      levels = 1:4,
      labels = c("user", "participant", "information source", "other")
    ),
    group5_role = factor(
      group5_role,
      levels = 1:4,
      labels = c("user", "participant", "information source", "other")
    ),
    group6_role = factor(
      group6_role,
      levels = 1:4,
      labels = c("user", "participant", "information source", "other")
    ),
    group7_role = factor(
      group7_role,
      levels = 1:4,
      labels = c("user", "participant", "information source", "other")
    ),
    group1_density = factor(
      group1_density,
      levels = 1:4,
      labels = c("individually", "partly", "all", "not specified")
    ),
    group2_density = factor(
      group2_density,
      levels = 1:4,
      labels = c("individually", "partly", "all", "not specified")
    ),
    group3_density = factor(
      group3_density,
      levels = 1:4,
      labels = c("individually", "partly", "all", "not specified")
    ),
    group4_density = factor(
      group4_density,
      levels = 1:4,
      labels = c("individually", "partly", "all", "not specified")
    ),
    group5_density = factor(
      group5_density,
      levels = 1:4,
      labels = c("individually", "partly", "all", "not specified")
    ),
    group6_density = factor(
      group6_density,
      levels = 1:4,
      labels = c("individually", "partly", "all", "not specified")
    ),
    group7_density = factor(
      group7_density,
      levels = 1:4,
      labels = c("individually", "partly", "all", "not specified")
    ),
    res1_role1 = factor(
      res1_role1,
      levels = 1:7,
      labels = c("data collection", "facilitation", "moderation", "user",
                 "participant", "trainer", "other")
    ),
    res1_role2 = factor(
      res1_role2,
      levels = 1:7,
      labels = c("data collection", "facilitation", "moderation", "user",
                 "participant", "trainer", "other")
    ),
    res2_role1 = factor(
      res2_role1,
      levels = 1:7,
      labels = c("data collection", "facilitation", "moderation", "user",
                 "participant", "trainer", "other")
    ),
    res2_role2 = factor(
      res2_role2,
      levels = 1:7,
      labels = c("data collection", "facilitation", "moderation", "user",
                 "participant", "trainer", "other")
    ),
    teacher_role1 = factor(
      teacher_role1,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    teacher_role2 = factor(
      teacher_role2,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    caregiver_role1 = factor(
      caregiver_role1,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    caregiver_role2 = factor(
      caregiver_role2,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    parent_role1 = factor(
      parent_role1,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    parent_role2 = factor(
      parent_role2,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    therapist_role1 = factor(
      therapist_role1,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    therapist_role2 = factor(
      therapist_role2,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    aut_exp_role1 = factor(
      aut_exp_role1,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    aut_exp_role2 = factor(
      aut_exp_role2,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    virtual_role1 = factor(
      virtual_role1,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    virtual_role2 = factor(
      virtual_role2,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    other_role1 = factor(
      other_role1,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    other_role2 = factor(
      other_role2,
      levels = 1:12,
      labels = c("info source", "representative", "consent provider",
                 "data collector", "assessment", "user", "ideator",
                 "facilitator", "moderator", "first aid", "mixture", "other")
    ),
    designer_role1 = factor(
      designer_role1,
      levels = 1:7,
      labels = c("info source", "ideator", "user", "data collector",
                 "facilitator", "moderator", "other")
    ),
    designer_role2 = factor(
      designer_role2,
      levels = 1:7,
      labels = c("info source", "ideator", "user", "data collector",
                 "facilitator", "moderator", "other")
    ),
    HCI_exp_role1 = factor(
      HCI_exp_role1,
      levels = 1:7,
      labels = c("info source", "ideator", "user", "data collector",
                 "facilitator", "moderator", "other")
    ),
    HCI_exp_role2 = factor(
      HCI_exp_role2,
      levels = 1:7,
      labels = c("info source", "ideator", "user", "data collector",
                 "facilitator", "moderator", "other")
    ),
    place = factor(
      place,
      levels = 1:8,
      labels = c("autism school", "autism center", "research lab", "home",
                 "familiar setting", "unfamiliar setting", "mixture", "other")
    ),
    time = factor(
      time,
      levels = 1:2,
      labels = c("one-shot", "longitudinal")
    )
  ) %>%
  rename(
    hci_exp = HCI_exp,
    hci_exp_role1 = HCI_exp_role1,
    hci_exp_role2 = HCI_exp_role2
  ) %>%
  select(-c(technique_name, material, notes))

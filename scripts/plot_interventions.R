intervention_temp_graph <- intervention_temp %>%
  mutate(
    intervention_category = ifelse(
      intervention_type %in% c("social_interact", "emot_recog",
                               "emot_regulation", "reciprocal_inter"),
      "Social Interaction",
      ifelse(
        intervention_type %in% c("communication", "conversation",
                                 "prosody_echo", "word_recog", "speech"),
        "Communication",
        ifelse(
          intervention_type %in% c("learning", "education"),
          "Learning",
          ifelse(
            intervention_type == "attention",
            "Attention",
            ifelse(
              intervention_type %in% c("body_awareness", "motor_skills",
                                       "proximity"),
              "Body Awareness",
              ifelse(
                intervention_type %in% c("life_skills", "vocation", "household",
                                         "selfcare"),
                "Life Skills",
                "Other"
              )
            )
          )
        )
      )
    ),
    intervention_category = factor(
      intervention_category,
      levels = c("Social Interaction", "Learning", "Communication",
                 "Body Awareness", "Attention", "Life Skills", "Other")
    )
  )


ggplot(intervention_temp_graph) +
  geom_bar(aes(intervention_category)) +
  scale_y_continuous(breaks = c(7, 9, 12, 13, 22, 39)) +
  xlab("Intervention Category") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("./graphics/interventions.png", width = 160, height = 80,
       units = "mm")

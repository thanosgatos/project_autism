ggplot(technique_actors) +
  geom_bar(aes(factor(technique_type,
                      levels = names(sort(table(technique_type),
                                          decreasing = TRUE))))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab("Technique") +
  ylab("Frequency") +
  scale_x_discrete(labels = c("user testing" = "User Testing",
                              "interview" = "Interview",
                              "free play" = "Free Play",
                              "posttest" = "Post-test",
                              "participatory design" = "Participatory Design",
                              "deployment" = "Deployment",
                              "workshop" = "Workshop",
                              "focus group" = "Focus Group",
                              "pretest" = "Pre-test",
                              "survey" = "Survey",
                              "field study" = "Field Study",
                              "expert evaluation" = "Expert Evaluation",
                              "prototyping" = "Prototyping",
                              "digital ethnography" = "Digital Ethnography",
                              "probe" = "Probe",
                              "other" = "Other")) +
  scale_y_continuous(breaks = seq(5, 75, 10))


ggsave("./graphics/techniques_actors_all.png", width = 160, height = 80,
       units = "mm")

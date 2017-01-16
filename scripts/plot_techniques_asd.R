ggplot(technique_asd) +
  geom_bar(aes(factor(technique_type,
                      levels = names(sort(table(technique_type),
                                          decreasing = TRUE))))) +
  scale_x_discrete(labels = c("user testing" = "User Testing",
                              "posttest" = "Post-test",
                              "free play" = "Free Play",
                              "deployment" = "Deployment",
                              "participatory design" = "Participatory Design",
                              "interview" = "Interview",
                              "field study" = "Field Study",
                              "pretest" = "Pre-test",
                              "focus group" = "Focus Group",
                              "workshop" = "Workshop",
                              "digital ethnography" = "Digital Ethnography",
                              "probe" = "Probe",
                              "survey" = "Survey",
                              "other" = "Other")) +
  scale_y_continuous(breaks = seq(5, 50, 5)) +
  xlab("Technique") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("./graphics/techniques_asd.png", width = 160, height = 80,
       units = "mm")

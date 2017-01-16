ggplot(technique_no_participants) +
  geom_bar(aes(factor(technique_type,
                      levels = names(sort(table(technique_type),
                                          decreasing = TRUE))))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab("Technique") +
  ylab("Frequency") +
  scale_x_discrete(labels = c("interview" = "Interview",
                              "workshop" = "Workshop",
                              "posttest" = "Post-test",
                              "expert evaluation" = "Expert Evaluation",
                              "survey" = "Survey",
                              "participatory design" = "Participatory Design",
                              "focus group" = "Focus Group",
                              "pretest" = "Pre-test",
                              "field study" = "Field Study",
                              "prototyping" = "Prototyping",
                              "deployment" = "Deployment",
                              "other" = "Other")) +
  scale_y_continuous(breaks = c(1, 2, 4, 5, 6, 7, 8, 10, 21))


ggsave("./graphics/techniques_actors_asd.png", width = 160, height = 80,
       units = "mm")

ggplot(participants_age_asd) +
  geom_bar(aes(age)) +
  scale_x_discrete(labels = c("preschooler" = "Preschooler",
                              "child" = "Child",
                              "adolescent" = "Adolescent",
                              "adult" = "Adult",
                              "not specified" = "Not Specified")) +
  scale_y_continuous(breaks = c(6, 14, 22, 56, 87)) +
  xlab("Age group") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("./graphics/age_asd_participants.png", width = 160, height = 80,
       units = "mm")

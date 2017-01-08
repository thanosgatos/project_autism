transform_group_role <- function(group) {

  technique <- technique %>%
    mutate(
      assign(
        as.name(sprintf("group%d_role", group)),
        factor(
          eval(parse(text = as.name(sprintf("group%d_role", group)))),
          levels = 1:4,
          labels = c("user", "participant", "information source", "other")
        )
      )
    )

}




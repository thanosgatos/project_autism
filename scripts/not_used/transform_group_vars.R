transform_group_vars <- function(group) {

  technique <- technique %>%
    mutate(
      sprintf("group%d_role", group) = factor(
        eval(parse(text = as.name(sprintf("group%d_role", group)))),
        levels = 1:4,
        labels = c("user", "participant", "information source", "other")
      ),
      sprintf("group%d_density", group) = factor(
        eval(parse(text = as.name(sprintf("group%d_density", group)))),
        levels = 1:4,
        labels = c("individually", "partly", "all", "not specified")
      )
    )

}



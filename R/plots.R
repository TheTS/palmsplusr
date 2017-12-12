

palms_plot <- function(data, path = ""){

  for (i in unique(data$identifier)) {
    #TODO make this dynamic
    d <- data %>% filter(identifier == i)
    h <- home.b %>% filter(identifier == i)
    s <- school %>% filter(school_id == as.numeric(participant_basis[participant_basis$identifier == i, "school_id"]))

    p <- ggplot() +
      geom_sf(data = d, aes(colour=factor(mot)), lwd = 0.9) +
      geom_sf(data = h, aes(fill="h"), alpha = 0.8) +
      geom_sf(data = s, aes(fill="s"), alpha = 0.8) +
      scale_fill_manual(labels = c("Home (100m)", "Schoolyard"),
        values = c("#E01616", "#000080")) +
      scale_color_manual(breaks = c("1", "2", "3"),
        labels = c("Walk", "Bicycle", "Vehicle"),
        values = c("1"="forestgreen","2"="#1874CD","3"="#EE7600")) +
      theme_minimal() +
      labs(title = i, colour = "Trajectories", fill = "Polygons")

    ggsave(file = file.path(path, paste0(i, "_new.png")), plot = p, width=18, height=18, units="cm")
  }
}

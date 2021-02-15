popgrowth_end_1619 <- ((56287000-55268100)/55268100)*100
#Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/enpop/pop

eth_plot_data <- filter(ethn_summary,dose=="first") %>%
  filter(.,!(ethnicity_small %in% c("Unknown","Total"))) %>%
  select(.,date,datechar,dose,ethnicity_small,eth_rate)

eth_plot <- eth_plot_data %>%
  ggplot(aes(x=date, y=eth_rate,
              color=ethnicity_small)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Vacination rate by ethnicity (first dose)") +
  scale_y_continuous(name="% vaccinated (first dose)") +
  theme_economist() +
  xlab(" ") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white"),
    legend.title = element_blank()
  )
  
ggplotly(eth_plot)
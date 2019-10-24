library(leaflet)
library(ggplot2)

plotTimeSeries <- function(applications.df, timeseries.type, language) {
  ggplot(
    applications.df %>%
      dplyr::mutate(
        application_month = .[, timeseries.type] %>% format("%Y-%m-01") %>% as.Date()
      ) %>%
      dplyr::filter(group_language %in% language) %>%
      dplyr::group_by(institution_name, institution_id, application_month) %>%
      dplyr::summarise(applications = dplyr::n()) %>%
      dplyr::ungroup()
    , aes(x = application_month, y = applications, group = institution_name)) + 
    geom_point(aes(color = institution_name), size = 3) +
    geom_smooth(aes(color = institution_name), linetype = "dotted", alpha = .15) +
    # scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    theme_minimal() +
    theme(
      legend.position = "none"
      , axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
      , axis.title = element_text(size = 16)
      , axis.text.y = element_text(size = 14)
    ) +
    xlab("Mēnesis") + ylab("Pieteikumu skaits") +
    scale_x_date(breaks = scales::pretty_breaks(n = 15), date_labels = "%b., %Y") +
    scale_color_manual(values = c("#E69F00", "#F0E442", "#56B4E9", "#009E73"))
}

plotAdmissions <- function(admissions) {
  # admissions %>%
    # ggplot(aes(x = school_year, y = value, fill = Skaits)) +
    # geom_bar(stat="identity", color="black", position=position_dodge())+
  ggplot() +
    geom_col(
      data = admissions %>% dplyr::filter(Skaits != "Uzņemti")
      , aes(x = school_year + .25, y = value, fill = Skaits) 
      , width = .35, colour = "grey40", size = 0.4
    ) +
    geom_col(
      data = admissions %>% dplyr::filter(Skaits == "Uzņemti")
      , aes(x = school_year - .15, y = value, fill = Skaits) 
      , width = .3, colour = "grey40", size = 0.4
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(hjust = 1, size = 14, angle = 45)
      , axis.title = element_text(size = 16)
      , axis.text.y = element_text(size = 14)
      , legend.text = element_text(size = 14)
      , legend.title = element_text(size = 16)
    ) + xlab("Uzņemšanas gads") + ylab("Uzņemtie bērni") +
    scale_x_continuous(breaks = unique(admissions$school_year)) +
    scale_fill_manual(values = c("#E69F00", "#F0E442", "#56B4E9", "#009E73"))
}

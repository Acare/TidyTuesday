library(tidyverse)
theme_set(theme_minimal(base_size = 15, base_family = "Serif"))

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_rank_tidy <- breed_rank_all |> 
    pivot_longer(ends_with("Rank"), names_to = "year", values_to = "rank") |> 
    janitor::clean_names() |> 
    mutate(year = str_remove(year, " Rank") |> as.numeric()) |> 
    group_by(breed) |> 
    mutate(median_rank = median(rank)) |> 
    ungroup()

breeds_to_highlight <- c("Retrievers (Labrador)", 
                         "French Bulldogs",
                         "Yorkshire Terriers")

breed_rank_hl <- breed_rank_tidy |> 
    filter(median_rank <= 10, breed %in% breeds_to_highlight)


# Ranking of the top 9 most popular dog breeds over time ------------------

breed_rank_tidy |> 
    filter(median_rank <= 10) |> 
    ggplot(aes(year, rank)) +
    ggbump::geom_bump(aes(group = breed, color = breed), 
                      color = "black", size = 1, alpha = 0.2) +
    ggbump::geom_bump(aes(group = breed, color = breed), 
                      size = 2, alpha = 0.8,
                      data = breed_rank_hl) +
    geom_point(size = 2.5, color = "grey") +
    geom_point(aes(color = breed), size = 3,
               data = breed_rank_hl) +
    ggimage::geom_image(aes(image = image), # asp = 2, 
                        image_fun = \(img) {
                            img |> 
                                magick::image_flop() |> 
                                # magick::image_modulate(brightness = 70, saturation = 100, hue = 280) |> 
                                # magick::image_scale(geometry = "50%x50%") |> 
                                magick::image_border(color = "grey")
                        },
                        data = breed_rank_tidy |> 
                            filter(median_rank <= 10, year == 2013),
                        hjust = 1, position = position_nudge(x = -0.1)) +
    geom_text(aes(label = breed), 
              color = "grey50", alpha = 1, 
              hjust = 0, position = position_nudge(x = 0.15),
              data = breed_rank_tidy |> 
                   filter(year == 2020, median_rank <= 10, 
                          !breed %in% breeds_to_highlight)) +
    geom_label(aes(label = breed, fill = breed), 
               color = "black", alpha = 0.3, label.size = 0,
               hjust = 0, position = position_nudge(x = 0.15),
               data = breed_rank_hl |> filter(year == 2020)) +
    annotate(geom = "curve", x = 2016, y = 11, xend = 2014.6, yend = 7.1,
             curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x = 2016.1, y = 11, hjust = "left",
             label = "The end begins for Yorkies...") +
    paletteer::scale_color_paletteer_d("wesanderson::IsleofDogs1") +
    paletteer::scale_fill_paletteer_d("wesanderson::IsleofDogs1") +
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.3)),
                       breaks = 2013:2020) +
    scale_y_continuous(trans = "reverse", 
                       breaks = 1:20, labels = str_c("#", 1:20)) +
    labs(x = NULL, y = NULL, color = NULL,
         title = "Ranking of Most Popular<sup>*</sup> Dog Breeds from 2013 to 2020",
         subtitle = "Will French Bulldogs dethrone Labradors next year? Why are Yorkies becoming so unpopular?",
         caption = "* Only breeds with at least a median rank of 10 are displayed.") +
    theme(axis.text.x = element_text(vjust = 1, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none",
          plot.title = ggtext::element_markdown(size = 20, face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          plot.background = element_rect(fill = "#fcfce8"))


# Breed traits ------------------------------------------------------------

trait_desc <- trait_description |> 
    janitor::clean_names() |> 
    filter(trait %in% c("Barking Level", 
                        "Coat Grooming Frequency", 
                        "Drooling Level", 
                        "Shedding Level", 
                        "Watchdog/Protective Nature")) |> 
    mutate(breed = "French Bulldogs", 
           point = -1,
           description = str_wrap(description, width = 50))

breed_traits_tidy <- breed_traits |> 
    rename(breed = Breed) |> 
    mutate(across(-breed, as.character),
           breed = str_replace_all(breed, "[^[:graph:]]", " ")) |> 
    pivot_longer(-breed, names_to = "trait", values_to = "point") 

breed_traits_tidy |> 
    filter(!trait %in% c("Coat Type", "Coat Length")) |> 
    mutate(point = as.integer(point)) |> 
    filter(breed %in% breeds_to_highlight) |> 
    group_by(trait) |> 
    mutate(are_pts_equal = ifelse(length(unique(point)) == 1, TRUE, FALSE)) |> 
    ungroup() |> 
    filter(are_pts_equal == FALSE) |> 
    ggplot(aes(breed, point, fill = breed)) +
    geom_col(alpha = 0.7, color = "grey50", size = 0.1) +
    geom_text(aes(label = point), color = "white", vjust = 2,
              family = "Serif", fontface = "bold") +
    geom_text(aes(label = description), size = 2, data = trait_desc,
              position = position_nudge(x = -0.5, y = 0.5),
              hjust = 0, vjust = 1, 
              family = "Serif") +
    facet_wrap(facets = vars(trait), ncol = 5) +
    paletteer::scale_fill_paletteer_d("wesanderson::IsleofDogs1") +
    ylim(-3, 5) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "A Selection<sup>*</sup> of _Traits_ for French Bulldogs, Labradors and Yorkies",
         subtitle = paste("Labradors excel in Energy, Goodness with other dogs, Playfulness and Trainability.",
                          "Traits presenting a description are more discriminative for French Bulldogs and Yorkies.",
                          sep = "\n"),
         caption = "* Only traits which vary among the three breeds are displayed.") +
    theme(panel.grid = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          axis.text = element_blank(),
          legend.position = "top",
          plot.title = ggtext::element_markdown(size = 20, face = "bold"),
          plot.background = element_rect(fill = "#fcfce8"),
          strip.text = element_text(size = 10, color = "#D9D0D3"),
          strip.background = element_rect(fill = "#0F0D0E", color = "#8D8680"))


# Difference in ranks -----------------------------------------------------



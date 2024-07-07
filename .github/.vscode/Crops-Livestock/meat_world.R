# Library
library(tidyverse)

temporario <- readr::read_csv(
    "c:/Users/italo/Downloads/FAOSTAT_data_en_3-25-2024.csv") |>
    janitor::clean_names("snake") |>
    dplyr::glimpse()

temp <- temporario |>
    dplyr::select(item, year, value) |>
    #dplyr::filter(element == "Gross Production Index Number (2014-2016 = 100)") |>
    #dplyr::group_by(item, year) |>
    #dplyr::summarise(soma = sum(value)) |>
    dplyr::mutate(item = str_remove(item, ",.*"),
        item = case_when(
            item == "Meat of cattle with the bone" ~ "Meat of cattle",
            item == "Meat of pig with the bone" ~ "Meat of pig",
            TRUE ~ item
        )
    )  |>
    dplyr::filter(
        item %in% c(
            "Meat of buffalo",
            "Meat of chickens",
            "Meat of cattle",
            #"Meat of goat",
            "Meat of pig",
           "Meat of sheep"
            )) 

font <- "Josefin Sans"
font2 <- "Open Sans"

# Use the font_add_google() function to load fonts from the web
sysfonts::font_add_google(family = font, font, db_cache = FALSE)
sysfonts::font_add_google(family = font2, font2, db_cache = FALSE)

sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "C:/Users/italo/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf") # nolint

theme_set(theme_minimal(base_family = font2, base_size = 3))

showtext::showtext_opts(dpi = 300)
showtext::showtext_auto(enable = TRUE)

github_icon <- "&#xf09b"
linkedin_icon <- "&#xf0e1"
x_icon <- "&#xf099"
instagram_icon <- "&#xf16d"
github_username <- "italomarquesmonteiro"
linkedin_username <- "italomarquesmonteiro"
x_username <- "italommonteiro"
instagram_username <- "italo.m.m"

bg <- "white"
txt_col <- "grey20"
colors <- "grey40"
fundo <- "white"


title_text <- glue::glue('Produção de carne no Brasil') # nolint
subtitle_text <- glue::glue("")
caption_text <- glue::glue(
  "**Data:**  FAOStat (2024)<br>", # nolint
  "**Note:** Main proteins of animal origin produced in Brazil, data from FAO<br>",
  "**Plot:** Ítalo Marques-Monteiro <br><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: black;'>{github_icon};</span> 
  <span style='color: grey20'>{github_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: #1a96fc;'>{linkedin_icon};</span> 
  <span style='color: grey20'>{linkedin_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: steelblue;'>{x_icon};</span>
  <span style='color: grey20'>{x_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: #fd5257;'>{instagram_icon};</span>
  <span style='color: grey20'>{instagram_username}</span>"
)
pal <- c(
    "#3595AC",
    "#C36812",
    "#BE8707",
    "#CC0303")
# Ordem de empilhamento
order <- c("Meat of chickens", "Meat of cattle", "Meat of pig", "Meat of sheep")

animal_protein <- temp |>
    dplyr::mutate(item = factor(item, levels = order)) |>
    ggplot(aes(x = year, y = value, fill = item)) +
    #geom_area()
    ggstream::geom_stream(type = "ridge", bw = 1) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    scale_x_continuous(breaks = c(1961, 1972, 1982, 1992, 2002, 2012, 2022), labels = c("1961", "1972", "1982", "1992"," 2002", "2012", "2022")) + # nolint
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_void() +
    labs(
        #title = title_text,
        #subtitle = "4º trimestre de 2023",
        fill = "Protein",
        caption = caption_text) +
    theme(
       axis.line.x = element_line(linewidth = .75),
       #panel.grid = element_blank(),
       panel.background = element_rect(fill = "white", color = "white"),
       panel.grid = element_line(color = "white"),
       plot.background = element_rect(fill = "white"),
       axis.text.y = element_blank(),
       axis.text.x = element_text(face = "bold", family = font, color = txt_col, size = 12,margin = margin(5, 0, 0, 0)), # nolint
       plot.margin = margin(20, 120, 20, 20),
       legend.title = ggtext::element_markdown(face = "bold", family = font, size = 15),
       legend.text = ggtext::element_markdown(face = "bold", family = font, size = 12),
       plot.caption = ggtext::element_markdown(hjust = 0, margin=margin(10, 0, 0, 0), size = 8, color=txt_col, lineheight = 1.2), # nolint
    ) +
    # Titulo
    annotate("text", x = 1965, y = 25000000,
             label = "Animal protein:\nmillion tons,\nBrazil",
             hjust = 0,
             size = 20,
             lineheight =.9,
             fontface = "bold",
             family = font,
             color = "black") +
    # Segmentos verticais
    geom_segment(aes(x = 1961, y = 0, xend = 1961, yend = 1500000+1000),color="black") + # nolint
    geom_point(aes(x = 1961, y = 1500000+1000),color = "black") +
    annotate("text", x = 1961, y = 2000000+1000,
           label = "2,05",
           hjust = 0.5,
           size = 4,
           lineheight = .8,
           fontface = "bold",
           family = font,
           color = txt_col) +

    geom_segment(aes(x = 1972, y = 0, xend = 1972, yend = 3325500+1000),color="black") + # nolint
    geom_point(aes(x = 1972, y = 3325500+1000),color = "black") +
    annotate("text", x = 1972, y = 4000000+1000,
           label = "3,32",
           hjust = 0.5,
           size = 4,
           lineheight = .8,
           fontface = "bold",
           family = font,
           color = txt_col) +

    geom_segment(aes(x = 1982, y = 0, xend = 1982, yend = 6000000+1000),color = "black") + # nolint
    geom_point(aes(x = 1982, y = 6000000+1000),color = "black") +
    annotate("text", x = 1982, y = 6500000+1000,
        label = "5,54",
        hjust = 0.5,
        size = 4,
        lineheight = .8,
        fontface = "bold",
        family = font,
        color = txt_col) +

    geom_segment(aes(x = 1992, y = 0, xend = 1992, yend = 10500000+1000),color = "black") + # nolint
    geom_point(aes(x = 1992, y = 10500000+1000),color = "black") +
    annotate("text", x = 1992, y = 11000000+1000,
           label = "8,27",
           hjust = 0.5,
           size = 4,
           lineheight = .8,
           fontface = "bold",
           family = font,
           color = txt_col) +

    geom_segment(aes(x = 2002, y = 0, xend = 2002, yend = 17555576+1000),color = "black") + # nolint
    geom_point(aes(x = 2002, y = 17555576+1000),color = "black") +
    annotate("text", x = 2002, y = 18000000+1000,
           label = "17,05",
           hjust = 0.5,
           size = 4,
           lineheight = .8,
           fontface = "bold",
           family = font,
           color = txt_col) +

    geom_segment(aes(x = 2012, y = 0, xend = 2012, yend = 24076698+1000),color = "black") + # nolint
    geom_point(aes(x = 2012, y = 24076698+1000),color = "black") +
    annotate("text", x = 2012, y = 24500000+1000,
           label = "24,07",
           hjust = 0.5,
           size = 4,
           lineheight = .8,
           fontface = "bold",
           family = font,
           color = txt_col) +

    geom_segment(aes(x = 2022, y = 0, xend = 2022, yend = 29000000+1000), color = "black") + # nolint
    geom_point(aes(x = 2022, y = 29000000+1000),color = "black") +
    annotate("text", x = 2022, y = 29500000+1000,
           label = "30,16",
           hjust = 1.1,
           size = 4,
           lineheight = .8,
           fontface = "bold",
           family = font,
           color = txt_col)
animal_protein
ggsave(
    ".vscode/Crops and livestock/Images livestock/animal_protein.png",
    plot = animal_protein,
    width = 13,
    dpi = 300)



#plotly::ggplotly(temp1)
temp |>
    dplyr::group_by(year) |>
    dplyr::summarise(soma = sum(value)) |> print(n = 62)

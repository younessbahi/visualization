tictoc::tic("Building visualization: 1st Plot", quiet = FALSE, func.tic = my.msg.tic)
sysfonts::font_add_google('Roboto Condensed', 'Roboto Condensed')
sysfonts::font_add_google('Baloo 2', 'Baloo 2')
sysfonts::font_add_google('Source Sans Pro', 'Source Sans Pro')
sysfonts::font_add_google('Raleway', 'Raleway')
showtext::showtext_auto()

#####################################################
#_What are the languages used among Respendents that
#  classify themseves as Scientist|Analyst|Researcher?
#_Need to know the proportion of devs by profession and
#  those who aren't in this category to identify the main
#  Scientist|Analyst|Researcher and languages they use.
#####################################################
langSAR <- # (Scientist|Analyst|Researcher)
  df %>%
    filter(
      Period == 2021 &
        ! is.na(Language_split) &
        Field != 'Other' &
        Field == 'Scientist/Analyst/Researcher'
    ) %>%
    group_by(Field, MainBranch, Language_split) %>%
    summarise(Freq = n()) %>%
    mutate(
      Prop   = (Freq / sum(Freq)),
      color_ = {
        case_when(
          Prop > .08 ~ 'A',
          Prop > .06 & Prop <= .08 ~ 'B',
          Prop > .04 & Prop <= .06 ~ 'C',
          TRUE ~ 'D'
        )
      }
    ) %>%
    ungroup()

set.seed(0000)
mainBranch.Lvls <- unique(langSAR$MainBranch)
langSAR %<>%
  mutate(
    MainBranch = {
      case_when(
        MainBranch == mainBranch.Lvls[1] ~ 'Developer by profession',
        MainBranch == mainBranch.Lvls[2] ~ 'learning to code',
        MainBranch == mainBranch.Lvls[3] ~ 'Not primarily a developer, but write code as part of work',
        MainBranch == mainBranch.Lvls[4] ~ 'Code as a hobby',
        MainBranch == mainBranch.Lvls[5] ~ 'No longer a developer',
        MainBranch == mainBranch.Lvls[6] ~ 'None of these'
      )
    }
  )

customThemeP1 <-
  theme(
    text                  = element_text(family = 'Roboto Condensed'),
    strip.text.y          = element_text(angle = 0, color = 'grey65', size = 11),
    strip.background      = element_rect(color = 'transparent'),
    axis.text.x           = element_text(angle = - 90, hjust = 0, colour = 'grey75', size = 11),
    axis.text.y           = element_text(angle = 0, hjust = 0, colour = 'grey75', size = 9),
    strip.background.y    = element_rect(fill = '#44535c'),
    legend.position       = 'bottom',
    legend.title          = element_text(colour = 'grey75'),
    legend.background     = element_rect(fill = 'transparent'),
    legend.key            = element_rect(color = '#36393c'),
    legend.text           = element_text(colour = 'grey75'),
    axis.title            = element_blank(),
    panel.grid.major.y    = element_line(colour = "#455760", size = .12),
    panel.grid.major.x    = element_line(colour = "#455760", size = .3),
    plot.background       = element_rect(fill = "#3d3a3e"),
    panel.border          = element_rect(colour = "#4b575c", fill = NA),
    panel.background      = element_rect(fill = "#36393c", colour = "#3d3a3e", size = 2, linetype = "solid"),
    plot.title            = element_text(face = "bold", colour = "white", size = 14, family = "Roboto Condensed"),
    plot.subtitle         = element_text(colour = "white", family = "Roboto Condensed", size = 10),
    plot.caption          = ggtext::element_markdown(),
    plot.caption.position = 'plot',
    axis.ticks            = element_line(colour = 'grey95'),
    legend.key.height     = unit(.2, 'cm'),
    legend.key.width      = unit(.9, 'cm')
  )
langSAR_plot <-
  langSAR %>%
    ggplot(aes(Prop, tidytext::reorder_within(Language_split, - Prop, MainBranch), fill = color_)) +
    geom_bar(stat = "identity", color = 'transparent') +
    coord_flip() +
    ggh4x::facet_nested(
      MainBranch ~ .,
      independent = 'x',
      scales      = "free",
      space       = "fixed",
      labeller    = label_wrap_gen(25)
    ) +
    tidytext::scale_y_reordered() +
    scale_x_continuous(expand = c(0, 0), labels = scales::percent) +
    scale_fill_manual(
      values = c("#d3f8e2", "#87bba2", "#55828b", "#3b6064"),
      labels = c('> 10%', '7% - 10%', '4% - 7%', '< 4%'),
      name   = 'Language/Total'
    ) +
    labs(
      title = 'Languages used by Scientist | Analyst | Researcher',
      caption  = "<span style = 'color: #cccccc'>Visualzation by: <b>Youness Bahi</b><br>Source data:<b>StackOverflow Developer Survey 2021</b></span>"
    ) +
    theme_linedraw() +
    customThemeP1

g       <- ggplot_gtable(ggplot_build(langSAR_plot))
strips  <- which(grepl('strip-', g$layout$name))
fill_   <- c("transparent", "transparent", "transparent", "transparent", "transparent", "#368B85")
colors_ <- c("grey65", "grey65", "grey65", "grey65", "grey65", "white")

for (i in seq_along(strips)) {
  k <- which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
  l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
  g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- fill_[i]
  g$grobs[[strips[i]]]$grobs[[1]]$children[[l]]$children[[1]]$gp$col  <- colors_[i]
}
langSAR_plot <- grid::grid.draw(g)

tictoc::toc(quiet = FALSE, func.toc = my.msg.toc, info = "DONE")

######################################
# Salaries DevTypes by Technology ----
######################################
tictoc::tic("Building visualization: 2nd Plot", quiet = FALSE, func.tic = my.msg.tic)
topPaidTech <-
  df %>%
    group_by(DevType_split, Language_split) %>%
    summarise(
      Respondent,
      MedianComp = median(Converted_, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      color_ = {
        case_when(
          MedianComp > 200000 ~ 'A',
          MedianComp > 100000 & MedianComp <= 200000 ~ 'B',
          MedianComp > 40000 & MedianComp <= 100000 ~ 'C',
          TRUE ~ 'D'
        )
      }
    )

customThemeP2 <-
  theme(
    axis.text.x          = element_text(angle = - 90, hjust = 0, vjust = .5, colour = '#cccccc'),
    axis.title.x         = ggtext::element_markdown(colour = '#F57665', face = 'bold', size = 11),
    axis.title.y         = ggtext::element_markdown(colour = '#F57665', face = 'bold', size = 11),
    axis.text.y          = element_text(colour = '#cccccc'),
    legend.key.height    = unit(.2, 'cm'),
    legend.key.width     = unit(3, 'cm'),
    legend.justification = 'right',
    legend.position      = 'top',
    legend.title.align   = 1,
    legend.title         = ggtext::element_markdown(margin = margin(t = - 12)),
    plot.caption         = ggtext::element_markdown(),
    plot.title.position  = 'plot',
    plot.title           = ggtext::element_markdown(),
    panel.grid.major     = element_blank()
  )

topPaidTech_plot <-
  topPaidTech %>%
  filter(! is.na(Language_split)) %>%
  ggplot(aes(x = reorder(Language_split, MedianComp), y = reorder(DevType_split, MedianComp), fill = MedianComp)) +
  geom_tile() +
  labs(title    = "Top Paying <span style = 'color:#F57665'>Technologies</span> by <span style = 'color:#F57665'>Job Type</span>",
       y        = "Job Type",
       x        = "Languages",
       fill     = "<span>Median Annual Salary<span style='color:#cccccc;'><i> (Adjusted to current $ rate)</i></span></p>",
       subtitle = "Distribution of median annual total compensation\nbased on 114150 unique respondent",
       caption  = "<span style = 'color: #cccccc'>Visualzation by: <b>Youness Bahi</b><br>Source data:<b>StackOverflow Developer Survey 2021</b></span>") +
  scale_y_discrete(position = 'right') +
  scale_fill_viridis_c(
    option = "inferno",
    breaks = c(50000, 100000, 150000, 190000),
    labels = c("$50k", "$100k", "$150k", "$190k")
  ) +
  hrbrthemes::theme_modern_rc(
    subtitle_family = NULL,
    caption_family = NULL
  ) +
  customThemeP2
tictoc::toc(quiet = FALSE, func.toc = my.msg.toc, info = "DONE")
topPaidTech_plot #plot

##############################################
# Woman Representation in Developer Roles ----
##############################################
tictoc::tic("Building visualization: 3rd Plot", quiet = FALSE, func.tic = my.msg.tic)
genderRep <-
  df %>%
    filter(Gender != 'Other' & ! is.na(Gender)) %>%
    select(DevType_split, Gender, Period) %>%
    group_by(Period, DevType_split, Gender) %>%
    summarise(
      Freq = n()
    ) %>%
    ungroup() %>%
    reshape2::dcast(., DevType_split + Period ~ Gender, value.var = 'Freq') %>%
    mutate(
      Ratio = Woman / Man
    ) %>%
    reshape2::dcast(., DevType_split ~ Period, value.var = 'Ratio') %>%
    na.omit() %>%
    arrange(desc(`2021`)) %>%
    rowwise() %>%
    dplyr::mutate(
      change        = scales::percent(`2021` - `2020`, .01),
      indice        = if (change < 0) '&#x2198;' else '&#x2197;',
      color_link    = if (change <= 0) "#50af92" else "#F57665",
      color_inverse = if (change > 0) "#50af92" else "#F57665",
      color         = "grey50",
      x             = if (`2021` < 0.0317) `2020` else `2021`,
      hjust         = if (`2021` < 0.0317) 0 else 1.2,
      label = {
        glue::glue(
          "<span style = 'font-size:15px;font-weight: bold;color:{color_inverse}'>{indice}{change}</span>"
        )
      },
      vjust         = 0,
      fill          = NA,
      label.colour  = NA
    )

genderRep %<>%
  as.tibble() %>%
  arrange(`2021`, `2020`) %>%
  dplyr::mutate(
    name = fct_inorder(DevType_split),
    rank = row_number()
  )

customThemeP3 <-
  theme(
    panel.grid.minor      = element_blank(),
    legend.position       = "none",
    panel.grid.major.y    = element_line(colour = "#455760", size = .12),
    panel.grid.major.x    = element_line(colour = "#455760", size = .3),
    plot.background       = element_rect(fill = "#3d3a3e"),
    panel.border          = element_rect(colour = "#4b575c", fill = NA),
    panel.background      = element_rect(fill = "#3d3a3e", colour = "#3d3a3e", size = 2, linetype = "solid"),
    axis.title.x          = element_text(size = 13, face = "bold", colour = "white", family = ""),
    axis.title.y          = element_blank(),
    axis.text.x           = element_text(colour = "#cccccc"),
    axis.text.y           = element_text(colour = "#cccccc"),
    plot.title            = element_text(face = "bold", colour = "white", size = 20, family = "Baloo 2"),
    plot.subtitle         = element_text(colour = "#cccccc", family = "Baloo 2", size = 14),
    plot.caption          = ggtext::element_markdown(),
    plot.caption.position = "plot",
    legend.text           = element_text(colour = "#cccccc", family = "Roboto Condensed", size = 14)
  )


plotWRep <-
  ggplot(data = genderRep, aes(`2021`, name)) +
    scale_x_continuous(
      "",
      expand = c(0, 0),
      breaks = c(0.03, 0.05, 0.10, 0.12),
      labels = c("Very Low", "0.05x Men", "0.10x", "0.12x")
    ) +
    ggforce::geom_link(
      aes(
        x     = `2020`,
        y     = name,
        xend  = `2021`,
        yend  = name,
        size  = stat(index),
        color = color_link
      ),
      alpha = .7,
      n     = 2000
    ) +
    annotate("rect", xmin = 0.02, xmax = 0.03, ymin = 0, ymax = 23, alpha = .1, fill = "#DD3E3E", color = '#36393c') +
    geom_vline(xintercept = .03, colour = 'red', linetype = 'dashed', alpha = .5) +
    geom_vline(xintercept = .06, colour = '#45969B', linetype = 'dashed', alpha = .5) +
    geom_point(
      data   = filter(genderRep, change < 0),
      aes(`2021`, rank, color = color_link),
      shape  = 21,
      fill   = "white",
      size   = 5,
      stroke = 1.5
    ) +
    ggtext::geom_richtext(
      mapping      = aes(x, name),
      label        = genderRep$label,
      fill         = genderRep$fill,
      label.colour = genderRep$label.colour,
      hjust        = genderRep$hjust,
      color        = genderRep$color
    ) +
    ggrepel::geom_text_repel(
      data              = filter(genderRep, DevType_split == "Educator"),
      aes(label = 'index: 2021 '),
      nudge_x           = - .02,
      box.padding       = 0.5,
      nudge_y           = 1,
      segment.curvature = - 1,
      segment.ncp       = 3,
      segment.angle     = 20,
      colour            = "grey85"
    ) +
    ggrepel::geom_text_repel(
      data              = filter(genderRep, DevType_split == "Educator"),
      aes(x = `2020`, label = 'index: 2020 '),
      nudge_x           = .01,
      box.padding       = 0.05,
      nudge_y           = - 1,
      segment.curvature = - 0.1,
      segment.ncp       = 3,
      segment.angle     = 90,
      colour            = "grey85"
    ) +
    labs(
      title    = 'Women Representation in Developer Roles',
      subtitle = 'Change in Representation for the Period 2020-2021',
      caption  = "<span style = 'color: #cccccc'>Visualzation by: <b>Youness Bahi</b><br>Source data:<b>StackOverflow
       Developer Survey 2020|2021</b></span>"
    ) +
    theme_minimal(base_size = 14, base_family = "Roboto Condensed") +
    customThemeP3
tictoc::toc(quiet = FALSE, func.toc = my.msg.toc, info = "DONE")
plotWRep #plot
cat(crayon::bold('🚕 All done\n'))
cat(
  paste0(
  crayon::bold('📊 Plots variables are:\n'),
  crayon::blue('--> plotWRep\n--> topPaidTech_plot\n--> langSAR_plot\n')
  )
)
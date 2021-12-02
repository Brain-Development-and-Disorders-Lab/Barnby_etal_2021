# Plot functions


plotdiff <- function(x, y, colname, a, size, height) {
  x %>%
    ggplot(aes(Group, {{y}}, fill = Group))+
    ggdist::stat_histinterval(justification = 0,
                              alpha = 0.5,
                              outline_bars = F,
                              slab_color = "black",
                              slab_type = 'histogram',
                              normalize = 'all', n = 1,
                              aes(thickness = stat(f*(n^0.7))))+
    geom_boxplot(width = 0.1, alpha = 1, notch = F, outlier.colour = NA)+
    gghalves::geom_half_point(side = 'l', width = 0.2, pch = 21, position = position_nudge(x = -0.1), size = 2)+
    ggpubr::stat_compare_means(label.x = 2.75, label.y.npc = height, size = size,
                               method = 'kruskal', label = 'p.signif')+
    labs(y = colname)+
    coord_cartesian(clip = 'off')+
    scale_fill_manual(values = c('#508AA8', '#BA1200'))+
    scale_x_discrete(labels = c(
      expression(paste('CCD')),
      expression(paste('NT'))
    ))+
    theme_tq()+
    theme(axis.text.x = element_text(size = 18),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 18),
          axis.title.y = element_text(size = 16),
          legend.position = 'none')
}

# general tree plotting func
plot_tree <- function(
    tree = NULL,
    clusters = NULL,
    clusters.show = T,
    annotations = NULL,
    annotations_var = NULL,
    color_by = "serovar",
    color.tiplab = F,
    tip.size = 3,
    legend.x = 0.2,
    legend.y = 0.77,
    legend.size = 3,
    legend.ncat.per.col = 8,
    legend.hide = F,
    plot.xlim = 20,
    label_vars = c("ID"),
    label.offset = 5,
    label.size = 4,
    label.align = F,
    annot.offset = 0.5,
    annot.width = 0.4,
    annot.textsize = 4,
    annot.ncol = 6,
    annot.random.sample = F,
    annot.random.seed = 123,
    bottom.margin = 0
) {
  # use random seed
  set.seed(123)
  
  tip_labs <- metadata %>% 
    full_join(data.frame("ID" = tree$tip.label)) %>%
    mutate(ID_2 = ID) %>% # create duplicate column of ID
    pivot_longer(cols = 2:ncol(.),
                 names_to = "var",
                 values_to = "val") %>% 
    mutate(var = if_else(var == "ID_2", "ID", var)) %>% 
    mutate_all(~if_else(is.na(.), "NULL", .)) %>% 
    filter(var %in% label_vars) %>% 
    group_by(ID) %>% 
    group_split() %>% 
    map_dfr(function(x) {
      x %>% 
        arrange(factor(var, levels = label_vars)) %>% 
        group_by(ID) %>% 
        summarize(tip_lab = paste(val, collapse = "/"))
    })
  
  clusters_long <- clusters[,1:if_else(annot.ncol[1] > ncol(clusters), ncol(clusters), annot.ncol[1]+1)] %>% 
    pivot_longer(cols = 2:ncol(.),
                 names_to = "feature",
                 values_to = "value") %>% 
    mutate(feature = as.numeric(str_replace_all(feature, ".*_", "")))
  
  n_colors <- metadata %>% 
    full_join(data.frame("ID" = tree$tip.label)) %>%
    filter(ID %in% tree$tip.label) %>% 
    pull(!!sym(color_by)) %>% 
    unique() %>% 
    length()
  
  local_meta <- metadata %>% 
    full_join(data.frame("ID" = tree$tip.label)) %>%
    filter(ID %in% tree$tip.label) %>% 
    left_join(tip_labs, by = "ID") %>% 
    group_by(!!sym(color_by)) %>% 
    mutate(n = n(),
           "color_by" = paste0(!!sym(color_by), " (", n, ")")) %>% 
    ungroup()
  
  # tip labels colouring
  if ( color.tiplab ) {
    tiplab.geom <- geom_tiplab(
      aes(label = tip_lab,
          color = color_by),
      
      offset = label.offset,
      align = label.algn,
      linetype = NULL,
      size = label.size
    ) 
  } else {
    tiplab.geom <- geom_tiplab(
      aes(label = tip_lab),
      offset = label.offset,
      align = label.align,
      linetype = NULL,
      size = label.size
    )
  }
  
  # plot
  p <- tree %>% 
    ggtree(layout = "fan") %<+% local_meta +
    geom_tippoint(aes(color = color_by),
                  size = tip.size) +
    scale_color_manual(values = distinctColorPalette(n_colors)) +
    theme(legend.position=c(x = legend.x, y = legend.y),
          legend.text = element_text(size = legend.size+6)) +
    labs(color = "") +
    guides(color = guide_legend(override.aes = list(size = legend.size),
                                nrow = legend.ncat.per.col)) +
    geom_treescale(y = 0, x = 500) +
    ggtree::xlim(NA, plot.xlim)
  
  # create annotation lists
  annot_l <- list()
  
  if ( is.null(annotations) ) {
    
    if (clusters.show) {
      annot_l <- append(annot_l, list(clusters_long), after = 0)
      names(annot_l) <- "clustering"
    }
    
  } else {
    
    if (annot.random.sample) { 
      set.seed(NULL)
      annot.random.seed <- sample(1:10000, 1)
      print(paste0("Random seed: ", annot.random.seed))
      set.seed(annot.random.seed) 
    } else {
      set.seed(annot.random.seed)
    }
    
    annot_l <- imap(annotations_var, function(x,y) {
      
      local_annotations <- annotations %>% 
        filter(type == x)
      
      # randomly sample columns
      sample_size <- if_else(clusters.show, annot.ncol[y+1], annot.ncol[y])
      
      local_annotations %>% 
        filter(feature %in% sample(unique(local_annotations$feature), 
                                   if_else(sample_size > length(unique(local_annotations$feature)),
                                           length(unique(local_annotations$feature)),
                                           sample_size))) %>% 
        mutate(value = if_else(value == 0, "Absent", "Present"))
      
    })
    
    if (clusters.show) {
      annot_l <- append(annot_l, list(clusters_long), after = 0)
      names(annot_l)[1] <- "clustering"
    }
    
  }
  
  if (length(annot_l) > 0 ) {
    for (i in 1:length(annot_l) ) {
      # annotation block title
      if ( clusters.show ) {
        annot.title <- ifelse(i == 1, "Clustering", annotations_var[i-1])  
      } else {
        annot.title <- annotations_var[i]
      }
      
      annot.titlesize <- annot.textsize
      # add annotation blocks
      p <- p +
        new_scale_fill() +
        geom_fruit(
          data = annot_l[[i]],
          geom = geom_tile,
          aes(x = feature,
              y = ID,
              fill = value),
          offset = annot.offset[i],
          color = "white",
          pwidth = annot.width[i],
          axis.params = list(
            axis="x", # add axis text of the layer.
            text.angle=90, # the text size of axis.
            hjust=1,  # adjust the horizontal position of axis labels
            text.size = annot.textsize,
            title = annot.title,
            title.height = 0.025,
            title.size = annot.titlesize
          )
        )
      
      # set random seed
      set.seed(123)
      
      if (i == 1 & clusters.show) {
        
          p <- p +
            scale_fill_manual(
              values = distinctColorPalette(length(unique(clusters_long$value)))
            ) +
            guides(fill = "none")  
        
      } else {
        
        p <- p +
          labs(fill = "") +
          scale_fill_manual(
            values = c("grey", "#f76c6a")
          ) +
          guides(fill = guide_legend(override.aes = list(size = legend.size),
                                      nrow = 1))
        
        # whether to hide color legend
        if ( legend.hide ) {
          p <- p + guides(fill = 'none')
        }
      }
    }
  }
  
  p <- p +
    layout_rectangular() +
    #guides(fill = "none") +
    tiplab.geom +
    scale_color_manual(
      values = distinctColorPalette(n_colors)) +
    theme(plot.margin = unit(c(0,0,bottom.margin,0), "cm"))
  
  # whether to hide color legend
  if ( legend.hide ) {
    p <- p + guides(color = 'none')
  }
  
  # return plot
  return(p)
}
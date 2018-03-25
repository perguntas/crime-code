# helper functions for visualisation
require(extrafont)

# enable ghostscript to embed fonts
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.21/bin/gswin32c.exe")
loadfonts()

plot_theme = function(...){
 theme(text = element_text(family="Palatino Linotype", 
                            size=14,
                            colour="black"),
        axis.title = element_text(size = rel(1.25)),
       panel.background = element_rect(fill="white",
                                       colour=NA),
       panel.border = element_rect(fill=NA,
                                   colour="grey40"),
       panel.grid.major = element_line(colour="grey92",
                                       size=0.25),
       panel.grid.minor = element_line(colour = "grey92", 
                                       size = 0.25),
       strip.background = element_rect(fill="grey85",
                                       colour="grey20"),
       legend.key=element_rect(fill="white",
                               colour=NA),
       axis.text = element_text(colour = "black"),
  ...)
}


theme_map = function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0,0,0,0),"mm"),
      text=element_text(size=16, family="Palatino Linotype"),
      ...
    )
}

is_outlier = function(x, quantil, above = T) {
  if (above == T){
    return(x > quantile(x, 1-quantil, na.rm=T) + 
             1.5 * (quantile(x, 1-quantil, na.rm=T) - quantile(x, quantil, na.rm=T)))
  } else {
    return(x < quantile(x, 0.25, na.rm=T) - 
             1.5 * (quantile(x, 1-quantil, na.rm=T) - quantile(x, quantil, na.rm=T)) | 
             x > quantile(x, 1-quantil, na.rm=T) + 
             1.5 * (quantile(x, 1-quantil, na.rm=T) - quantile(x, quantil, na.rm=T)))
  }
}

savemap = function(name, plots, width=1400, height=2100){
  fname = paste0("C:/Users/Lara/Dropbox/Uni/thesis/thesis/thesis/Figures/",name,".png")
  png(fname, width = width, height = height, res=120)
  plot(plots)
  dev.off()
}

saveplot = function(name, plots, width=965, height=600){
  fname = paste0("C:/Users/Lara/Dropbox/Uni/thesis/thesis/thesis/Figures/",name,".png")
  png(fname, width = width, height = height, res=120)
  plot(plots)
  dev.off()
}


# for each setting, create scatterplot of coefficients and save them
scatterplots = function(results, setting, file, crimetype, out = F){
  # check crime types
  stopifnot(crimetype %in% c("violent", "property"))
  
  # create and save plot for each setting
  for (j in 1:length(setting)){
    # create data
    scatter_data = scatterplot_preparation(results,
                                            setting = setting[j],
                                            file = file)
    # create basic point plot with minimal jittering
    p = ggplot(scatter_data, aes(x=variables, y=plot)) +
      geom_point(aes(color = factor(model)), size = 3.3, position=position_dodge(width=0.55)) +
      plot_theme(axis.title.x=element_blank(),
                 axis.title.y=element_blank()) +
      theme(legend.position="top",
            plot.margin=unit(c(1,3.5,1,1), "lines"),
            axis.text = element_text(size = 16),
            legend.title=element_text(size=16),
            legend.text=element_text(size=16)) +
      scale_color_manual(values = c("#FCA007", "#D64B40", "#C20093", "#66277C", "#000004"),
                         guide = guide_legend(title = "Models",
                                              title.position = "top",
                                              title.hjust = 0.5),
                         labels=c("CAR", "GLM", "LR", "GLMM", "SAR")) +
      coord_flip()
    #print(p)

    # set offset for labels
    if (crimetype == "violent"){
      offset = c(5.181118, 5.200335, 5.752347, 17.4157, 5.220357, 6.97816, 5.751898, 6.038212)
    } else if (crimetype == "property"){
      offset = c(9.736319, 7.493419, 2.983404, 5.206568, 3.578831,3.89061, 5.953528, 2.627495)
    }
    
    # ## can be optimised
    # ranges = ggplot_build(p)$layout$panel_ranges[[1]]$x.range
    # 
    # offset = ranges[2] + (ranges[2] / abs(ranges[1])) * 0.1
    # if (j %in% c(4,7,8) & crimetype == "property") offset = offset - 0.2
    # if (j %in% c(2, 7, 8) & crimetype == "violent") offset = offset + 0.05
    
    # add annotations: number of models the coef is significant in
    # for (i in 1:length(unique(scatter_data$variables))){
    #   p = p + annotation_custom(
    #     grob = textGrob(label = scatter_data$Freq[i], hjust = 0, gp = gpar(fontfamily ="Palatino Linotype", 
    #                                                                        size=12)),
    #     xmin = scatter_data$variables[i],
    #     xmax = scatter_data$variables[i],
    #     ymin = offset[j], 
    #     ymax = offset[j]) 
    # }
    # 
    # # Code to override clipping
    # gt = ggplot_gtable(ggplot_build(p))
    # gt$layout$clip[gt$layout$name == "panel"] = "off"
    #grid.draw(gt)
    # show plot (optional)
    if (out) p#grid.draw(gt)
    
    # save plot
    name = paste0(crimetype, "_coefficients", setting[j], "experimental")
    saveplot(name=name, plots=p, width=1200, height=740)
  }
}

saveggpairs = function(name, plots, width=965, height=600){
  fname = paste0("C:/Users/Lara/Dropbox/Uni/thesis/thesis/thesis/Figures/", name, ".png")
  png(fname, width = width, height = height, res = 120)
  print(plots)
  dev.off()
}


POI_plots = function(layer, category){
  # create plot for every category considered
  for (i in 1:length(category)){
    # create upper-case title
    title = paste0(toupper(substr(category[i], 1, 1)), 
                    substr(category[i], 2, nchar(category[i])))
    col = layer[, colnames(layer) == category[i]]
    
    my_breaks = seq(min(col, na.rm=T), max(col, na.rm=T), by = 10)
    if (max(col, na.rm=T) > max(my_breaks)) my_breaks = c(my_breaks, max(my_breaks) + 10)
    
    # make plot
    p = ggplot() + geom_polygon(data = layer, aes(x = long, y = lat,
                                                   group = group,
                                                   fill = get(category[i])),
                                 colour = "black", size = 0.25) +
      labs(x=NULL, y=NULL) +
      theme_map() +
      coord_map() +
      #scale_x_continuous(expand = c(0, 0), limits = c(-74.04190, -73.70001)) +
      #scale_y_continuous(expand = c(0, 0), limits = c(40.56953, 40.91553)) +
      scale_fill_viridis(option = "B", direction = -1,
                         breaks = my_breaks,
                         limits = c(NA, max(my_breaks)),
                         guide = guide_colorbar(direction = "horizontal",
                                                title.position = 'top',
                                                barheight = unit(4, units = "mm"),
                                                barwidth = unit(50, units = "mm"),
                                                draw.ulim = F,
                                                title.hjust = 0.6,
                                                label.hjust = 0.5,
                                                title=NULL)) +
      theme(legend.position = "bottom",
            plot.margin=unit(c(0,0,0,0),"mm"))
    
    plotname = paste0("POI_", title)
    savemap(plotname, p, width = 600, height=900)
    
  }
}


scatterplot_preparation = function(results, setting, file){
  coefs = lapply(1:length(file), function(x) results[[setting]][[x]]$coefficients)
  SE = lapply(1:length(file), function(x) results[[setting]][[x]]$SE[,4])
  
  coef_frame = 
    t(do.call(rbind, coefs)) %>%
    as.data.frame(.) %>%
    set_colnames(names(results[[setting]])) %>%
    # set model names as variable
    mutate(variables = row.names(.)) %>%
    gather(., variables) %>%
    set_colnames(c("variables","model","value"))
    
  SE_frame = 
    t(do.call(rbind, SE)) %>%
    as.data.frame %>%
    set_colnames(names(results[[setting]])) %>%
    # set model names as variable
    mutate(variables = row.names(.)) %>%
    gather(., variables) %>%
    set_colnames(c("variables","model","pvalue"))
  
  # combine SE_frame and coef_frame
  final_data = data.frame(variables=coef_frame$variables, 
                           model=coef_frame$model, 
                           value=coef_frame$value, 
                           pvalue=SE_frame$pvalue)
  
  # set non-significant coefficients to NA
  final_data$plot = ifelse(final_data$pvalue < 0.05, final_data$value, NA)
  
  # count number of significant variables
  final_data = 
    data.frame(table(final_data$variables, is.na(final_data$plot))) %>%
    filter(Var2 == F) %>%
    mutate(Freq = paste0("N = ", Freq)) %>%
    select(Var1, Freq) %>%
    left_join(final_data, ., by = c("variables" = "Var1")) %>%
    mutate(helperstar = ifelse(grepl("0", Freq), "*", ""),
           variables = paste0(variables, helperstar),
           variables = factor(variables, 
                              levels = rev(variables[1:length(unique(variables))]))) %>%
    select(-helperstar)
  
  return(final_data)
    
}

taxi_map = function(mapdata, taxidata, type, alpha_range = c(0.5, 0.95), size_range = c(0.134, 0.173),
                     low = "#374c7c", high = "#171327", save = T){
  if (type %in% c("pickup", "dropoff")){
    x_coord = paste0(type, "_long")
    y_coord = paste0(type, "_lat")
  } else stop("Type must be one of: pickup, dropoff")
  
  p = ggplot() + 
    geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), fill = "grey50") +
    geom_point(data = taxidata, aes(x = get(x_coord), y = get(y_coord),
                                    alpha = count, size = count, color = count)) +
    scale_alpha_continuous(range = alpha_range, trans = "log", limits = range(taxidata$count)) +
    scale_size_continuous(range = size_range, trans = "log", limits = range(taxidata$count)) +
    scale_color_gradient(low = low, high = high, trans = "log") + 
    coord_map(xlim = range(mapdata$long), ylim = range(mapdata$lat)) +
    theme_map(legend.position="none")
  if (save) savemap(paste0(type), p)
  return(p)
}


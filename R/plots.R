###|
###| Plotting
###|
###|

plot_template <- function(data, x, y,
                          label, repel_size, repel_padding,
                          point_size, point_alpha,
                          image, image_size,
                          yintercept, xintercept, intercept, slope,
                          title, subtitle, xlabel, ylabel, caption,
                          annotation_df) {

  # Start off the plot. These four values must always be defined
  plot <- ggplot2::ggplot(data=data, aes(x=x, y=y, label=label))

  # Start a cascade of conditional adding to plot when terms aren't null
  if(!is.null(repel_size)) {
    plot <- plot + ggrepel::geom_label_repel(size=repel_size, point.padding=repel_padding)
  }
  if(!is.null(point_size)) {
    plot <- plot + ggplot2::geom_point(size=point_size, alpha=point_alpha)
  }
  if(!is.null(image)) {
    plot <- plot + ggplot2::geom_point(size=point_size, alpha=point_alpha)
  }
  if(!is.null(yintercept)) {
    plot <- plot + ggplot2::geom_hline(yintercept=yintercept, linetype="dotted")
  }
  if(!is.null(xintercept)) {
    plot <- plot + ggplot2::geom_vline(xintercept=xintercept, linetype="dotted")
  }
  if(!is.null(intercept)) {
    plot <- plot + ggplot2::geom_abline(intercept=intercept, slope=slope)
  }
  if(!is.null(title)) {
    plot <- plot + ggplot2::lab(title=title)
  }
  if(!is.null(subtitle)) {
    plot <- plot + ggplot2::lab(subtitle=subtitle)
  }
  if(!is.null(xlabel)) {
    plot <- plot + ggplot2::lab(x=xlabel)
  }
  if(!is.null(ylabel)) {
    plot <- plot + ggplot2::lab(y=ylabel)
  }
  if(!is.null(caption)) {
    plot <- plot + ggplot2::lab(caption=caption)
  }
  if(!is.null(annotation_df)) {
    for(i in 1:length(annotation_df$x)) {
      plot <- plot + annotate("text", x=annotation_df$x[i], y=annotation_df$y[i], label=annotation_df$label[i])
    }
  }
}



  ggplot(data=punts_meta, aes(x=temperature, y=SHARP_RERUN, label=punter_player_name))+
  #geom_label_repel(size=2, point.padding = 0.1)+
  geom_point(size=1, alpha=0.2)+
  #geom_image(aes(image = url), size = 0.04)+
  geom_hline(yintercept=100, linetype="dotted")+
  geom_vline(xintercept=mean(mini$temperature, na.rm = TRUE), linetype="dotted")+
  geom_abline(intercept = 90.988, slope = 0.144374)
labs(title = "Weathering the weather",
     #subtitle = "Do punters bring their best to the Super Bowl?",
     x="Average temperature for a punt attempt", y="SHARP RERUN", caption="figure @ThePuntRunts | data @nflfastR")+
  # annotate("text", x=97, y=113, label="Precision Punters")+
  #annotate("text", x=103, y=110, label="Big-stage Punters")+
  # annotate("text", x=97, y=95, label="Replacement Punters")+
  #annotate("text", x=103, y=95, label="Career Performers")+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




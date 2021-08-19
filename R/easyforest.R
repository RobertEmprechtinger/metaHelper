library(dplyr)
library(grid)
library(ggplot2)
library(gridExtra)


easyforest <- function(
  rma_results, # a list of rma results
  sig_level = 0.05, # the two sided significance value
  z = qnorm(sig_level / 2) * -1, # transforms the sig_level to z
  null_value = 0, # null effect size. In case of SMD = 0 and RR = 1.
  margin_bottom = 1, # Distance from last effect description to x_axis
  headlines = c("Not Registered", "Registered", "Registered and Unpublished"),
  error_bar_height = 0.2,

  textsize = 3, # general text size
  title_size = 3.5,
  top_space = 0,

  pooled_estimate_position = -1, # position of the pooled effect in relation to the lowermost study
  distance_headline = 1.4, # distance of the grouped headline

  group_distance = 1.5, # distance from one group to the next

  CI_title = "SMD [95% CI]",
  label_title = "Author (Year)",
  values_title = "Standardized Mean Differences",


  outcome_left = "Favours Homeopathy",
  outcome_right = "Favours Placebo",
  outcome_height = 0,

  summary_title = "Random Effects"
){


  create_data <- function(rma_results){
    i <- 0
    for(meta_result in rma_results){
      i <- i + 1

      #creating the description of the pooled effect estimates
      summary_measure = paste(summary_title,
                              "; Q = ", formatC(meta_result$QE, digits=2, format="f"),
                              "; p = ", formatC(meta_result$QEp, digits=2, format="f"),
                              "; I^2 = ",  formatC(meta_result$I2, digits=1, format="f"),
                              sep = ""
      )

      #creating the summary esimates
      y <- tibble(x_pooled = c(meta_result$beta,
                               meta_result$ci.ub, meta_result$beta, meta_result$ci.lb),
                  y_pooled = c(-0.5, 0, 0.5, 0) + 1,
                  group = i)

      #creating the studie effect sizes and CIs
      x <- tibble(row =length(meta_result$slab):1, yi = meta_result$yi.f, vi = meta_result$vi.f,
                  label = meta_result$slab, group=i, type="study", face="plain") %>%
        mutate(ci_low = yi - z * sqrt(vi), ci_up = yi + z * sqrt(vi))

      #adding group headlines
      x <- bind_rows(
        tibble(label = headlines[i], type = "headline", row = max(x$row) + distance_headline, face="bold"),
        x
      )

      # adding the values of the pooled effect estimate
      x <- bind_rows(x,
                     tibble(row=pooled_estimate_position, label=summary_measure, type="summary", face="plain",
                            ci_low = meta_result$ci.lb, ci_up = meta_result$ci.ub, yi = meta_result$beta),
      )

      x$row <- x$row - pooled_estimate_position + 1

      if(i == 1){
        studies <- x
        pooled_es <- y
      } else {
        studies$row <- studies$row + x$row[1] + group_distance
        studies <- rbind(studies, x)
        pooled_es$y_pooled <- pooled_es$y_pooled + x$row[1] + group_distance
        pooled_es <- rbind(pooled_es, y)
      }
    }
    return(list(studies, pooled_es))
  }

  #Creating a study data frame and the coordinates of the summarized effect estimates
  studies_es <- create_data(rma_results)

  studies <- studies_es[[1]]
  pooled_es <- studies_es[[2]]


  # The graphical representation of the CIs
  values <- ggplot() +
    geom_errorbarh(data=filter(studies, type=="study"), aes(y = row, xmin = ci_low, xmax = ci_up),
                   height = error_bar_height) +
    scale_y_continuous(breaks = NULL, limits = c(min(studies$row) - margin_bottom, max(studies$row)+top_space),
                       expand = c(0,0)) +
    geom_vline(xintercept = null_value, linetype = "dashed", alpha=0.5) +
    geom_polygon(data = pooled_es, aes(x = x_pooled, y = y_pooled, group=group), fill="gray", color="black") +
    geom_point(data=filter(studies, type=="study"), aes(y=row, x=yi), color="black", shape=15) +
    xlab("") +
    ylab("") +
    theme_light() +
    ggtitle(values_title) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          axis.line.x = element_line("black"), panel.border = element_blank(),
          axis.ticks.x = element_line("black"), panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          plot.title = element_text(size=textsize * title_size, face = "bold", hjust = 0.9))

  # The study labels
  labels <- studies %>%
    ggplot(aes(y=row, label = label, x=0, fontface = face)) +
    geom_text(size = textsize, hjust="left") +
    scale_y_continuous(breaks = NULL, limits = c(min(studies$row) - margin_bottom, max(studies$row)+top_space),
                       expand = c(0,0)) +
    theme_light() +
    ggtitle(label_title) +
    xlab("") +
    ylab("") +
    scale_x_continuous(position = "top", limits = c(0, 2)) +
    theme(plot.title = element_text(size=textsize * title_size, face = "bold", hjust = 0.1)) +
    coord_cartesian(clip = "off") +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.border = element_blank(), panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.line.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

  # The confidence intervals (plain text)
  CIs <- studies %>% filter(type != "headline") %>%
    ggplot(aes(y=row, label = paste(round(yi, 2) %>% format(nsmall = 2), " [",
                                    round(ci_low, 2) %>% format(nsmall = 2), "; ",
                                    round(ci_up, 2) %>% format(nsmall = 2), "]", sep = ""), x=0)) +
    geom_text(size = textsize, hjust="right") +
    scale_y_continuous(breaks = NULL, limits = c(min(studies$row) - margin_bottom, max(studies$row)+top_space),
                       expand = c(0,0)) +
    theme_light() +
    scale_x_continuous(position = "top", limits = c(-0.02, 0)) +
    ggtitle(CI_title) +
    xlab("") +
    ylab("") +
    theme(plot.title = element_text(size=textsize * title_size, face = "bold", hjust = 0.9),
          panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.line.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

  # Transform to grid
  values <- ggplotGrob(values)
  labels <- ggplotGrob(labels)
  CIs <- ggplotGrob(CIs)

  # Set the width of the individual grids
  labels$widths[5] <- unit(3, "cm")
  values$widths[5] <- unit(7, "cm")
  CIs$widths[5] <- unit(3, "cm")


  g <- cbind(labels, values, CIs)

  grid.newpage()

  grid.draw(g)

}


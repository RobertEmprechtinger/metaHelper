#' Create forest plots for metafor outcomes
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate bind_rows filter
#' @import ggplot2
#'
#' @keywords internal
#'
#' @param rma_results a list of rma results created by metafor
#' @param sig_level the two sided significance value
#' @param z transforms the sig_level to z
#' @param null_value null effect size. In case of SMD = 0 and RR = 1
#' @param margin_bottom distance from last effect description to x_axis
#' @param headlines group headlines
#' @param error_bar_height height of the errorbars
#' @param pooled_estimate_height height of the pooled estimate parallelogram
#' @param textsize general text size
#' @param title_size size of the plot title
#' @param top_space space from panel to top
#' @param pooled_estimate_position position of the pooled effect in relation to the lowermost study
#' @param distance_headline distance of the group headlines
#' @param group_distance distance from one group to the next
#' @param CI_title title for the text CIs
#' @param label_title title for the studies
#' @param values_title title for the plotted CIs
#' @param summary_title title written in front of the summary effects
#'
#' @return
#' @export
#'
#' @examples
#' ### copy BCG vaccine meta-analysis data into 'dat'
#'
#' dat <- metafor::dat.bcg
#'
#' ### calculate log risk ratios and corresponding sampling variances (and use
#' ### the 'slab' argument to store study labels as part of the data frame)
#' dat <- metafor::escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat,
#'              slab=paste(author, year, sep=", "))
#'
#' ### fit random-effects model
#' res <- metafor::rma(yi, vi, data=dat)
#'
#' #' easyforest(res)
easyforest <- function(
  rma_results, #
  sig_level = 0.05, # the two sided significance value
  null_value = 0, # null effect size. In case of SMD = 0 and RR = 1.
  margin_bottom = 1, # Distance from last effect description to x_axis
  headlines = "",
  error_bar_height = 0.2, # Height of the errorbars
  pooled_estimate_height = 0.3, # T

  textsize = 3, # general text size
  title_size = 3.5,
  top_space = 0,

  pooled_estimate_position = -1, #
  distance_headline = 1.4, #

  group_distance = 1.5, #

  show_fav = FALSE,
  fav_height = 0.017,
  fav_space = 0.05,
  fav_textsize = 2.5,
  fav_lab = c('Favours X', "Favours Y"),

  CI_title = "SMD [95% CI]",
  label_title = "Author (Year)",
  values_title = "Standardized Mean Differences",

  summary_title = "Random Effects"
){
  z = qnorm(sig_level / 2) * -1


  create_data <- function(rma_results){
    i <- 0
    for(meta_result in rma_results){
      i <- i + 1

      # creating the description of the pooled effect estimates
      summary_measure = paste(summary_title,
                              "; Q = ", formatC(meta_result$QE, digits=2, format="f"),
                              "; p = ", formatC(meta_result$QEp, digits=2, format="f"),
                              "; I^2 = ",  formatC(meta_result$I2, digits=1, format="f"),
                              sep = ""
      )

      # creating the summary estimates
      y <- data.frame(x_pooled = c(meta_result$beta,
                               meta_result$ci.ub, meta_result$beta, meta_result$ci.lb),
                  y_pooled = c(-pooled_estimate_height, 0, pooled_estimate_height, 0) + 1,
                  group = i)

      #creating the study effect sizes and CIs
      x <- data.frame(row =length(meta_result$slab):1, yi = meta_result$yi.f, vi = meta_result$vi.f,
                  label = meta_result$slab, group=i, type="study", face="plain") %>%
        mutate(ci_low = yi - z * sqrt(vi), ci_up = yi + z * sqrt(vi))

      #adding group headlines
      x <- bind_rows(
        data.frame(label = headlines[i], type = "headline", row = max(x$row) + distance_headline, face="bold"),
        x
      )

      # adding the values of the pooled effect estimate
      x <- bind_rows(x,
                     data.frame(row=pooled_estimate_position, label=summary_measure, type="summary", face="plain",
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


  favour_left <- grid::textGrob(fav_lab[1], 0.58 - fav_space, fav_height, just = "right", gp = grid::gpar(fontsize = 9, cex = 0.1 * fav_textsize * textsize))
  favour_right <- grid::textGrob(fav_lab[2], 0.58 + fav_space, fav_height, just = "left", gp = grid::gpar(fontsize = 9, cex = 0.1 * fav_textsize * textsize))

  grid::grid.newpage()

  grid::grid.draw(g)

  if(show_fav){
    grid::grid.draw(favour_left)
    grid::grid.draw(favour_right)
    }
}


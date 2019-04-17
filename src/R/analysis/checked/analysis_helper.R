# Helper function for making forest plot
formatInterval <- function(mean, lower, upper) {
  upper <- formatC(upper, format = "f", digits = 3)
  upper <-
    ifelse(grepl("-", upper), str_c("", upper), str_c("  ", upper))
  lower <- formatC(lower, format = "f", digits = 3)
  lower <-
    ifelse(grepl("-", lower), str_c(" ", lower), str_c("   ", lower))
  interval <- str_c("[", lower, " , ", upper, " ]")
  mean <- formatC(mean, format = "f", digits = 3)
  return(str_c(mean, "   ", interval))
}

# Function for making forest plots
makePrimaryTopicForestPlot <-
  function(plot,
           file_name,
           covar_label_1,
           covar_label_2) {
    plotDF <-
      data.frame(t(sapply(plot$cis, function(x)
        x[1:max(lengths(plot$cis))])))
    
    prepDF <- data.frame(plot$labels,
                         plot$topics,
                         unlist(plot$means),
                         plotDF$X2.5.,
                         plotDF$X97.5.)
    
    colnames(prepDF) <- c("primary_topic",
                          "topic_n",
                          "mean",
                          "lower",
                          "upper")
    
    prepDF <- prepDF %>%
      group_by(primary_topic) %>%
      summarise(
        avg_ci_point_estimate = mean(mean),
        avg_ci_lower_bound = mean(lower),
        avg_ci_upper_bound = mean(upper)
      )
    
    table_text <- cbind(
      c(NA, seq(1, length(
        prepDF$primary_topic
      ))),
      c("Primary Topic", as.vector(prepDF$primary_topic)),
      c(
        "Confidence Interval (95%)",
        formatInterval(
          prepDF$avg_ci_point_estimate,
          prepDF$avg_ci_lower_bound,
          prepDF$avg_ci_upper_bound
        )
      )
    )
    
    pdf(str_c("~/Documents/", file_name))
    forestplot(
      table_text,
      graph.pos = 3,
      is.summary = c(TRUE, rep(FALSE, length(
        prepDF$primary_topic
      ))),
      align = c("r", "l", "r"),
      mean = c(NA, prepDF$avg_ci_point_estimate),
      lower = c(NA, prepDF$avg_ci_lower_bound),
      upper = c(NA, prepDF$avg_ci_upper_bound),
      fn.ci_norm = fpDrawCircleCI,
      hrzl_lines = list("2" = gpar(lwd = 2, col = "#000000")),
      boxsize = .15,
      xlab = (str_c(
        "\n", covar_label_1, " ... ", covar_label_2
      )),
      cex = 0,
      zero = 0,
      new_page = FALSE,
      txt_gp = fpTxtGp(
        xlab = gpar(cex = .75),
        label = gpar(cex = .75),
        ticks = gpar(cex = .75)
      ),
      col = fpColors(
        box = "black",
        lines = "black",
        zero = "gray50"
      ),
      cex = 0.9,
      lineheight = "auto",
      colgap = unit(4, "mm"),
      lwd.ci = 1,
      clip = c(-.1, 1)
    )
    dev.off()
    
  }
# Function for getting image n from tag
get_image_n <-
  function(image_tag) {
    as.numeric(str_extract_all(image_tag, "[0-9]+")[[1]])
  }

# Function for removing punctuation besides hashtag
remove_punctiation <-
  function (x, preserve_intra_word_dashes = FALSE) {
    if (preserve_intra_word_dashes) {
      x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
      x <- remove_punctiation_helper(x)
      gsub("\001", "-", x, fixed = TRUE)
    } else {
      remove_punctiation_helper(x)
    }
  }

# Helper function for remove_punctiation() function
remove_punctiation_helper <-
  function(x) {
    x <- gsub("#", "\002", x)
    x <- gsub("_", "\003", x)
    x <- gsub("[[:punct:]]+", "", x)
    x <- gsub("\002", "#", x, fixed = TRUE)
    gsub("\003", "_", x, fixed = TRUE)
  }

# Function to collapse hashtags to lemmas
collapse_punctuation <-
  function (x) {
    x <- gsub("# ", "#", x, fixed = TRUE)
    gsub(" _ ", "_", x, fixed = TRUE)
  }

# Function to remove common terms
remove_common_terms <-
  function (x, pct) {
    x[, slam::col_sums(x) / nrow(x) <= pct]
  }

# Function for estimating AMCE
# Fixes `texteffect` function
sibp_amce_temp <-
  function(sibp.fit,
           X,
           Y,
           G,
           seed = 0,
           level = 0.05,
           thresh = 0.9) {
    # Want it to be the case that G %*% beta selects the correct beta
    if (is.null(G)) {
      G <- matrix(1, nrow = nrow(X), ncol = 1)
    }
    
    set.seed(seed)
    
    G.test <- G[sibp.fit$test.ind, , drop = FALSE]
    Z.test <- infer_Z(sibp.fit, X)
    Y.test <- (Y[sibp.fit$test.ind] - sibp.fit$meanY) / sibp.fit$sdY
    
    Z.hard <-
      apply(Z.test, 2, function(z)
        sapply(z, function(zi)
          ifelse(zi >= 0.9, 1, 0)))
    
    L <- sibp.fit$L
    K <- sibp.fit$K
    
    if (L == 1) {
      fit <- lm(Y.test ~ Z.hard)
    }
    else{
      rhsmat <- c()
      for (l in 1:L) {
        rhsmat <- cbind(rhsmat, Z.hard * G.test[, l])
      }
      fit <- lm(Y.test ~ -1 + as.matrix(G.test) + rhsmat)
    }
    
    ci.bounds <-
      cbind(
        coef(fit) + qnorm(level / 2) * summary(fit)$coefficients[, 2],
        coef(fit) + qnorm(1 - level / 2) * summary(fit)$coefficients[, 2]
      )
    
    cidf <- data.frame(
      x = 1:((K + 1) * L),
      effect = coef(fit),
      L = ci.bounds[, 1],
      U = ci.bounds[, 2]
    )
    cidf[, -1] <- cidf[, -1] * sibp.fit$sdY
    sibp.amce <- cidf
    return(sibp.amce)
  }

# Function for getting AMCE lm fit
get_amce_model <-
  function(sibp.fit,
           X,
           Y,
           G,
           seed = 0,
           level = 0.05,
           thresh = 0.9) {
    # Want it to be the case that G %*% beta selects the correct beta
    if (is.null(G)) {
      G <- matrix(1, nrow = nrow(X), ncol = 1)
    }
    
    set.seed(seed)
    
    G.test <- G[sibp.fit$test.ind, , drop = FALSE]
    Z.test <- infer_Z(sibp.fit, X)
    Y.test <- (Y[sibp.fit$test.ind] - sibp.fit$meanY) / sibp.fit$sdY
    
    Z.hard <-
      apply(Z.test, 2, function(z)
        sapply(z, function(zi)
          ifelse(zi >= 0.9, 1, 0)))
    
    L <- sibp.fit$L
    K <- sibp.fit$K
    
    if (L == 1) {
      fit <- lm(Y.test ~ Z.hard)
    }
    else{
      rhsmat <- c()
      for (l in 1:L) {
        rhsmat <- cbind(rhsmat, Z.hard * G.test[, l])
      }
      fit <- lm(Y.test ~ -1 + as.matrix(G.test) + rhsmat)
    }
    return(fit)
  }

# Function for formatting treatment effects matrix
format_treatment_effects <-
  function(sibp.amce,
           treatments) {
    # Supply groups, always same in my case
    groups <- c("Black Democrat",
                "White Democrat",
                "Black Republican",
                "White Republican")
    
    # Subset and label df's coefficients
    subset_start <- length(groups) + 1
    subset_end <- nrow(sibp.amce)
    estimate_df <- sibp.amce[c(subset_start:subset_end), ]
    estimate_df$level <-
      rep(groups, each = nrow(estimate_df) / length(groups))
    estimate_df$treatment <-
      rep(treatments, times = nrow(estimate_df) / length(treatments))
    estimate_df$treatment = factor(
      estimate_df$treatment,
      levels = c('Black Pride', 'Dangerous Society', 'Identity Support')
    )
    
    # Reformat estimate df for clear output
    estimate_df <- estimate_df %>%
      select(effect, L, U, level, treatment) %>%
      arrange(treatment) %>%
      mutate(L = round(L, 2),
             U = round(U, 2),
             effect = round(effect, 2))
    estimate_df <-
      estimate_df[, c("treatment", "level", "effect", "L", "U")]
    
    # Print estimate df
    print(estimate_df)
    
  }

# Function for drawing treatment effects
draw_treatment_effects <-
  function(sibp.amce,
           treatments,
           groups_title,
           effect_title,
           xlim_l,
           xlim_u,
           ratio) {
    # Supply groups, always same in my case
    groups <- c("Black Democrat",
                "White Democrat",
                "Black Republican",
                "White Republican")
    
    # Subset and label df's coefficients
    subset_start <- length(groups) + 1
    subset_end <- nrow(sibp.amce)
    estimate_df <- sibp.amce[c(subset_start:subset_end), ]
    estimate_df$level <-
      rep(groups, each = nrow(estimate_df) / length(groups))
    estimate_df$treatment <-
      rep(treatments, times = nrow(estimate_df) / length(treatments))
    estimate_df$treatment = factor(
      estimate_df$treatment,
      levels = c('Black Pride', 'Dangerous Society', 'Identity Support')
    )
    
    # Filter out groups without significant effects for sparsity
    estimate_df <- estimate_df %>%
      filter(level != "Black Republican" & level != "White Democrat")
    
    estimate_df <- estimate_df %>% 
      mutate(color = ifelse(L * U > 0, 'red', 'black'))
    
    # Plot effects
    estimate_df %>%
      ggplot(., aes(
        x = effect,
        y = level,
        xmin = L,
        xmax = U,
        color = color
      )) +
      geom_vline(xintercept = 0,
                 linetype = "dashed",
                 color = "grey") +
      geom_point() +
      geom_errorbarh(height = .1) +
      facet_grid(. ~ treatment) +
      coord_fixed(ratio = 0.50 * abs(xlim_u)) +
      scale_color_identity() +
      xlim(xlim_l, xlim_u) +
      labs(y = groups_title, x = effect_title) +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "grey"),
        axis.line = element_line(color = "grey", size = 0.5),
        panel.border = element_rect(
          color = "grey",
          fill = NA,
          size = 0.5
        )
      )
    
  }

draw_treatment_effects_full <-
  function(sibp.amce,
           treatments,
           groups_title,
           effect_title,
           xlim_l,
           xlim_u,
           ratio) {
    # Supply groups, always same in my case
    groups <- c("Black Democrat",
                "White Democrat",
                "Black Republican",
                "White Republican")
    
    # Subset and label df's coefficients
    subset_start <- length(groups) + 1
    subset_end <- nrow(sibp.amce)
    estimate_df <- sibp.amce[c(subset_start:subset_end), ]
    estimate_df$level <-
      rep(groups, each = nrow(estimate_df) / length(groups))
    estimate_df$treatment <-
      rep(treatments, times = nrow(estimate_df) / length(treatments))
    estimate_df$treatment = factor(
      estimate_df$treatment,
      levels = c('Black Pride', 'Dangerous Society', 'Identity Support')
    )
    
    estimate_df <- estimate_df %>% 
      mutate(color = ifelse(L * U > 0, 'red', 'black'))
    
    # Plot effects
    estimate_df %>%
      ggplot(., aes(
        x = effect,
        y = level,
        xmin = L,
        xmax = U,
        color = color
      )) +
      geom_vline(xintercept = 0,
                 linetype = "dashed",
                 color = "grey") +
      geom_point() +
      geom_errorbarh(height = .1) +
      facet_grid(. ~ treatment) +
      coord_fixed(ratio = 0.50 * abs(xlim_u)) +
      scale_color_identity() +
      xlim(xlim_l, xlim_u) +
      labs(y = groups_title, x = effect_title) +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "grey"),
        axis.line = element_line(color = "grey", size = 0.5),
        panel.border = element_rect(
          color = "grey",
          fill = NA,
          size = 0.5
        )
      )
    
  }

# Function for calculating power, linear
get_power_analysis_linear <-
  function(amce_model, G) {
    # Calculating power
    #  u: n coefficients
    #  v: n error degrees of freedom
    #     v=n-u-1
    #  n: n observations
    #     n=v+u+1
    # f2: effect size
    #     R^2/(1-R^2)
    
    # Get model summary
    model_summary <- summary(amce_model)
    r_squared <- model_summary$r.squared
    
    # Convert G to data.frame
    G <- as.data.frame(G)
    
    # Setting power variables
    u <- 16
    n.actual <- nrow(G)
    v.actual <- n.actual - u - 1
    f2 <- r_squared / (1 - r_squared)
    sig.level <- 0.05
    power <- 0.8
    
    # Get necessary observations for detecting null effect
    power.n <-
      pwr.f2.test(
        u = u,
        f2 = f2,
        sig.level = sig.level,
        power = power
      )
    estimate.n <- round(power.n$v) + u + 1
    
    # Get power given my number of observations
    power.power <-
      pwr.f2.test(
        u = u,
        v = v.actual,
        f2 = f2,
        sig.level = sig.level
      )
    estimate.power <- power.power$power
    
    print(paste("Required n:", estimate.n))
    print(paste("Power with supplied n:", estimate.power))
    
  }

# Method for viewing interventions with treatment
# r <- df_merged_small_dfm %>%
#   rename(foo_bar = `say`) %>%
#   filter(foo_bar != 0) %>%
#   select(foo_bar, AdText) %>%
#   arrange(desc(foo_bar))
# View(r)
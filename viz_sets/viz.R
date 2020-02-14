resource <- function() {
  source('./viz.R')
  library(ggplot2)
  library(plyr)
}

read.sharing.mat <- function() {
  sharing.mat <<- read.csv('./sharing_mat.csv')
}

read.triggers.sbi.mat <- function() {
  tm.sbi.mat <<- read.csv('./tm_sbi_agg_melted.csv')
}

read.tm.viz.mat <- function() {
  tm.viz <<- read.csv('./tm_viz.csv')
  tm.viz$likelihood <<- exp(tm.viz$value) / (1 + exp(tm.viz$value))
  tm.viz$tt <<- c("social_pred"="Social", "reactive_pred"="Forced", "proactive_pred"="Proactive")[as.character(tm.viz$variable)]
}

plot.sharing <- function(mat=sharing.mat) {
  interim <- ddply(mat, .(social_trigger, value), function(x) { nrow(x) })
  final <- ddply(interim, .(social_trigger), function(x) { x$V1 / sum(x$V1) })
  colnames(final) <- c("Social.Trigger", "Not.Shared.Rate", "Shared.Rate")
  print(final)
  nm.map <- c(
    '0'="No Social Trigger",
    '1'="Social Trigger"
  )
  final$Social.Trigger <- nm.map[as.character(final$Social.Trigger)]
  final$Shared.Rate <- round(final$Shared.Rate*100)
  print(final)

  plot <- ggplot(aes(x=factor(Social.Trigger), y=Shared.Rate, fill=factor(Social.Trigger)), data=final) +
    geom_bar(stat="identity") +
    labs(
      y = "% Behaviors Shared"
    ) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      axis.title.y = element_text(size=11)
    ) + ylim(c(0,60))
  ggsave('./share_rate_by_social_trigger.png', plot, width=3, height=2)
}


plot.triggers.sbi <- function(mat=tm.sbi.mat) {
  tm.sbi.mat$sbi <<- factor(tm.sbi.mat$sbi, levels=c("Low", "Med", "High"))
  plot <- ggplot(aes(x=sbi_sort, y=X..Triggers, color=Trigger, shape=Trigger), data=tm.sbi.mat) +
    geom_line() +
    geom_point(aes(size=2), show.legend=F) +
    labs(
      y = "% Reported Behavior Triggers",
      x = "Security Behavioral Intention",
      color = "Trigger Type",
      shape = "Trigger Type"
    ) +
    theme(
      # legend.position = "bottom",
      # legend.direction = "horizontal",
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      axis.title.y = element_text(size=11)
    ) +
    scale_x_continuous(breaks=c(0,1,2), labels=c("Low", "Med", "High"))


  ggsave('./trigger_rate_by_sbi.png', plot, width=4, height=3)
}

plot.tm.viz.sbi <- function(mat=tm.viz) {
  plot <- ggplot(aes(x=sbi, y=likelihood, color=tt, fill=tt, linetype=tt), data=mat) +
    geom_smooth(alpha=0.3) +
    labs(
      y = "Likelihood of Trigger Type",
      x = "Security Behavioral Intention",
      color = "Trigger Type",
      fill = "Trigger Type",
      linetype = "Trigger Type"
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      axis.title.y = element_text(size=11)
    )

  ggsave('./trigger_rate_by_sbi_continous.png', plot, width=4, height=4)
}

plot.tm.viz.behavior <- function(mat=tm.viz) {
  pretty.behavior.name <- c(
    "AppUninstall" = "App Uninstall",
    "MobileAuth" = "Mobile Auth",
    "ChangedPwd" = "Changed Password",
    "SNSPrivacyChange" = "Facebook Privacy"
  )
  mat$pretty.behavior <- pretty.behavior.name[as.character(mat$behavior)]

  plot <- ggplot(aes(x=tt, y=likelihood), data=mat) +
    geom_jitter(alpha=0.2, height=0, width=0.2) +
    # geom_violin(alpha=0.8, draw_quantiles = c(0.05, 0.5, 0.95)) +
    geom_boxplot(alpha=0.8, width=0.3) +
    facet_grid(. ~ pretty.behavior) +
    labs(
      y = "Likelihood of Trigger Type"
    ) +
    theme(
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=11)
    )
  ggsave('./trigger_rate_by_behavior.png', plot, width=8.5, height=3)
}

plot.tm.viz.nationality <- function(mat=tm.viz) {
  nationality.label <- c(
    # "India" = "Non U.S.",
    "India" = "India",
    "United States" = "United States"
  )
  mat$pretty.nationality <- nationality.label[as.character(mat$Nationality)]
  plot <- ggplot(aes(fill=pretty.nationality, x=tt, y=likelihood), data=mat) +
    geom_jitter(aes(color=pretty.nationality, shape=pretty.nationality), height=0, width=0.1, alpha=0.2, show.legend=F) +
    # geom_violin(alpha=0.8) + # , draw_quantiles = c(0.05, 0.5, 0.95)
    geom_boxplot(alpha=0.8, width=0.2, position="dodge") +
    # geom_density(alpha=0.3) +
    # facet_grid(. ~ tt) +
    labs(
      y = "Likelihood of Trigger Type",
      fill = "Nationality"
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size=11)
    ) + coord_flip()
  ggsave('./trigger_rate_by_nationality.png', plot, width=4, height=4)
}

plot.sbi.dist <- function(mat=tm.viz) {
  tmp <- ddply(mat, .(uid), function(x) { mean(x$sbi) })
  plot <- ggplot(aes(x=V1), data=tmp) +
    geom_histogram() +
    labs(
      y = "Count",
      x = "Security Behavioral Intention"
    ) +
    theme(
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      axis.title.y = element_text(size=11),
      axis.title.x = element_text(size=11)

    )

  ggsave('./sbi_distribution.png', plot, width=3, height=2)
}

plot.age.gender.dist <- function(mat=tm.viz) {
  tmp <- ddply(mat, .(uid), function(x) { c(Age=x$Age[1], Gender=x$Gender[1]) })
  tmp$Gender <- as.character(levels(mat$Gender))[tmp$Gender]
  plot <- ggplot(aes(x=Age, fill=Gender), data=tmp) +
    geom_histogram(position="stack", color='black') +
    labs(
      y = "Count",
      x = "Age"
    ) +
    theme(
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      axis.title.y = element_text(size=11),
      axis.title.x = element_text(size=11)
    )

  ggsave('./age_gender_dist.png', plot, width=3, height=2)
}
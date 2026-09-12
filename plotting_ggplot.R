## Authors:
## -  GreenStickInsect aka Ogrodnik10

## This file contains functions handling the drawing of advanced plots which require ggplot2.
# Do not run this file directly, instead, source() it.

# Some of the functions defined might depend on functions,
# which you need to source() from "data_functions.R"

# Please be aware, following functions are often not designed to fail gracefully if invalid data is supplied.
# Use with care.


# The folllowing constants are defined as promises so as to allow sourcing this file
# even without ggplot2 loaded.

# Standard plot style for variety of charts
delayedAssign("theme_standard",
  theme_minimal() +
    theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
          legend.text = element_text(size=rel(1)),
          legend.title = element_text(size=rel(1.2), face="bold", hjust=0.5))
)

# Plot style for piecharts
delayedAssign("theme_piechart",
  theme_standard +
    theme(panel.grid = element_blank(), axis.text=element_text(size=rel(1), face="bold"),
          axis.title = element_blank())
)


# Draws a pie chart which compares usage of various playing methods.
#   dat - A dataset.
#   battleinfo - An (optional) list of additional info about the battle, usually published along with IP spend data.
#   tofile - path to file to which plot should be exported as .png . If FALSE, instead draws plot within R. Defaults to FALSE.
#   mode - special admin override string, shhh...
#
#   returns: a ggplot object which was used to draw the chart
sourceplot_piechart__ = function(dat, battleinfo=NULL, tofile=FALSE, mode=FALSE)
{
  # The following is basically dark magic practiced through trial and error,
  # because ggplot2 is TERRIBLY documented and does VERY WEIRD things to data, for no apparent reason.
  # Have "fun" trying to understand anything.
  #
  # PS No, this is not optimized, I got it to a roughly working version, quit, and hope I won't need to come back
  
  info = prepare_battleinfo(battleinfo, required=c("number", "place"), dat$raw)
  
  # This plot does not include data from automatic bot attacks,
  # so bot users are filtered out.
  # After all, they use none of the usual playing methods
  cleanplayers = dat$per_player[which(dat$per_player$bot == FALSE),]
  
  num_p = nrow(cleanplayers)
  
  ipdata = cleanplayers
  dpdata = cleanplayers
  ppdata = cleanplayers
  bipdata = cleanplayers
  
  ipdata$bipUsed = rep(0, length(ipdata$bipUsed))
  ipdata$DPUsed = rep(0, length(ipdata$DPUsed))
  ipdata$PPUsed = rep(0, length(ipdata$PPUsed))
  ipdata$sumUsed = ipdata$ipUsed
  
  ipdata = ipdata[order(ipdata$sumUsed),]
  ipdata_rev = ipdata[order(ipdata$sumUsed, decreasing=TRUE),]
  
  dpdata$ipUsed = rep(0, length(dpdata$ipUsed))
  dpdata$bipUsed = rep(0, length(dpdata$bipUsed))
  dpdata$PPUsed = rep(0, length(dpdata$PPUsed))
  dpdata$sumUsed = dpdata$DPUsed
  
  dpdata = dpdata[order(dpdata$sumUsed),]
  dpdata_rev = dpdata[order(dpdata$sumUsed, decreasing=TRUE),]
  
  bipdata$ipUsed = rep(0, length(bipdata$ipUsed))
  bipdata$DPUsed = rep(0, length(bipdata$DPUsed))
  bipdata$PPUsed = rep(0, length(bipdata$PPUsed))
  bipdata$sumUsed = bipdata$bipUsed
  
  bipdata = bipdata[order(bipdata$sumUsed),]
  bipdata_rev = bipdata[order(bipdata$sumUsed, decreasing=TRUE),]
  
  ppdata$ipUsed = rep(0, length(ppdata$ipUsed))
  ppdata$DPUsed = rep(0, length(ppdata$DPUsed))
  ppdata$bipUsed = rep(0, length(ppdata$bipUsed))
  ppdata$sumUsed = ppdata$PPUsed
  
  ppdata = ppdata[order(ppdata$sumUsed),]
  ppdata_rev = ppdata[order(ppdata$sumUsed, decreasing=TRUE),]
  
  sumvalues = c(sum(ipdata$sumUsed), sum(dpdata$sumUsed), sum(ppdata$sumUsed), sum(bipdata$sumUsed))
  values = c(ipdata$sumUsed, dpdata$sumUsed, ppdata$sumUsed, bipdata$sumUsed)
  values_group_rev = c(ipdata_rev$sumUsed, dpdata_rev$sumUsed, ppdata_rev$sumUsed, bipdata_rev$sumUsed)

  groups = c("Influence IP", "Direct Play", "Passive Play", "Discord BIP")
  cols = c("Influence IP"="skyblue1", "Direct Play"="blue2", "Passive Play"="cyan2", "Discord BIP"="purple1")
  data = data.frame(group=factor(rep(groups, each=num_p), groups),
                    value=values)

  full_circle = sum(ipdata$sumUsed) + sum(dpdata$sumUsed) + sum(ppdata$sumUsed) + sum(bipdata$sumUsed)
  circle_part = values_group_rev/full_circle * 360
  circle_rotation = (cumsum(circle_part) - circle_part/2 + 90)

  circle_rotation = c(rev(circle_rotation[1:num_p]),
                      rev(circle_rotation[(num_p+1):(num_p*2)]),
                      rev(circle_rotation[(num_p*2+1):(num_p*3)]),
                      rev(circle_rotation[(num_p*3+1):(num_p*4)]))
  circle_rotation = ifelse(circle_rotation > 90 & circle_rotation <= 270, circle_rotation+180, circle_rotation)
  
  circle_part2 = sumvalues/full_circle * 360
  circle_rotation2 = (cumsum(circle_part2) - circle_part2/2)
  circle_rotation2 = ifelse(circle_rotation2 > 90 & circle_rotation2 <= 270, circle_rotation2+180, circle_rotation2)
  
  labels = c()
  label_cols = c()
  for (currdat in list(ipdata, dpdata, ppdata, bipdata))
  {
    namecol = which(colnames(currdat) == "name")
    scorecol = which(colnames(currdat) == "sumUsed")
    colcol = which(colnames(currdat) == "color")
    for (rowi in seq(nrow(currdat)))
    {
      labels = c(labels, ifelse(currdat[rowi,scorecol] >= full_circle*0.015, currdat[rowi,namecol], ""))
      label_cols = c(label_cols, currdat[rowi,colcol])
    }
  }
  
  num_labels = format(sumvalues, scientific=F, big.mark=" ")
  num_labels = ifelse(circle_part2 >= 4, num_labels, "")
    
  csum = cumsum(sumvalues)
  pos = csum[length(csum)] - csum + sumvalues/2
  
  today = as.POSIXct(Sys.Date())
  its_pa = F
  if (mode == "pa" |
      (format(today, format="%m") == "04" & format(today, format="%d") == "01" & mode != "!pa")) its_pa = T
  
  inner_radius = 0
  if (its_pa) inner_radius = 0.5

  # ggplot complains about "not officially supported vectorized input to `element_text()`"
  # Well, maybe it SHOULD be officially supported, since that's the only way to manually rotate axis tick labels
  oldw <- getOption("warn")
  options(warn = -1)
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  
  plt = ggplot(data=data, mapping=aes(x="", y=value )) +
                geom_col(aes(fill=group), color="white", linewidth=0.2) +
                scale_fill_manual("IP source",
                    values = cols,
                    limits = groups) +
                coord_radial("y", expand=F, inner.radius = inner_radius) +
                theme_piechart +
                theme(axis.text.x = element_text(angle=circle_rotation2)) +
                geom_text(aes(group=group, label=labels, fontface="bold"),
                          position = position_stack(vjust=0.5), srt=circle_rotation,
                          color="black", hjust=0.53)+
                geom_text(aes(group=group, label=labels, fontface="bold"),
                          position = position_stack(vjust=0.5), srt=circle_rotation,
                          color="black", hjust=0.47)+
                geom_text(aes(group=group, label=labels, fontface="bold"),
                          position = position_stack(vjust=0.5), srt=circle_rotation,
                          color="black", vjust=0.55)+
                geom_text(aes(group=group, label=labels, fontface="bold"),
                          position = position_stack(vjust=0.5), srt=circle_rotation,
                          color="black", vjust=0.45)+
                geom_text(aes(group=group, label=labels, fontface="bold"),
                          position = position_stack(vjust=0.5), srt=circle_rotation,
                          color=label_cols)+
                scale_y_continuous(breaks=pos, labels=num_labels) +
                labs(title="Comparison of usage of various playing methods",
                     subtitle=paste("battle #", info$number, " in ", info$place, sep=""))
  
  print(plt)
  options(warn=oldw)
  
  if (tofile != FALSE)
  {
    dev.off()
  }
  return(invisible(plt))
}
sourceplot_piechart = ensure_isolation(sourceplot_piechart__, "sourceplot_piechart") # Just a tiny safety-ensuring wrapper, see data_functions.R


# hitdensity, hitdensity_ridge, hitdensity_data
# Function hitdensity draws a density plot of hits within a cycle (per minute).
#   returns: a ggplot object which was used to draw the plot
# Function hitdensity_ridge draws the same plot, but as a "ridge plot". Requires library ggridge.
#   returns: a ggplot object which was used to draw the plot
# Function hitdensity_data is internal and generates data required to draw the plots.
#   returns: a list with all required values.
# Arguments:
#   dat - A dataset.
#   battleinfo - An (optional) list of additional info about the battle, usually published along with IP spend data.
#   adjust - A multiplier to apply to the automatically calculated smoothing bandwidth.
#             See ggplot2::stat_density for details.
#   tofile - path to file to which plot should be exported as .png . If FALSE, instead draws plot within R. Defaults to FALSE.
hitdensity = function(dat, battleinfo=NULL, adjust = 0.2, tofile=FALSE)
{
  values = hitdensity_data(dat, battleinfo)
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  
  plt = ggplot(values$data, aes(x=minute, color=type, fill=type)) +
    geom_density(alpha=0.35, adjust=adjust) +
    geom_vline(data=values$medians, aes(xintercept=x, color=type, linetype="Median")) +
    scale_fill_manual("Hit method", values=c("Any"="gray40","Manual"="orange",
                                             "Direct Play"="blue", "Passive Play"="cyan2")) +
    scale_color_manual("Hit method", values=c("Any"="gray20","Manual"="orange2",
                                              "Direct Play"="blue2", "Passive Play"="cyan3")) +
    scale_linetype_manual("", values="dashed") +
    scale_x_continuous(breaks=values$xlabs, labels = values$xlabs_str, expand=0) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.02))) +
    labs(title="Probability density of hits across a cycle",
         subtitle=paste0("battle #", values$info$number, " in ", values$info$place)) +
    xlab("Minute of a cycle") +
    ylab("Probability density") +
    theme_standard
  
  print(plt)
  
  if (tofile != FALSE)
  {
    dev.off()
  }
  return(invisible(plt))
}

hitdensity_ridge = function(dat, battleinfo=NULL, adjust = 0.2, tofile=FALSE)
{
  values = hitdensity_data(dat, battleinfo)
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  
  plt = ggplot(values$data, aes(x=minute, y=y, height=after_stat(density), color=type, fill=type)) +
    geom_density_ridges(alpha=0.35, stat="density", adjust=adjust) +
    geom_segment(data=values$medians,
                 aes(x=x, xend=x, y=y, yend=y+2, color=type, linetype="Median"),
                 inherit.aes=F, key_glyph=draw_key_vline) +
    scale_fill_manual("Hit method",
                      values=c("Any"="gray40","Manual"="orange",
                               "Direct Play"="blue", "Passive Play"="cyan2")) +
    scale_color_manual("Hit method",
                       values=c("Any"="gray20","Manual"="orange2",
                                "Direct Play"="blue2", "Passive Play"="cyan3")) +
    scale_linetype_manual("", values="dashed") +
    scale_x_continuous(breaks=values$xlabs, labels = values$xlabs_str, expand=0) +
    scale_y_continuous(breaks=c(), expand=0) +
    labs(title="Probability density of hits across a cycle",
         subtitle=paste0("battle #", values$info$number, " in ", values$info$place)) +
    xlab("Minute of a cycle") +
    ylab("Probability density") +
    theme_standard
  
  print(plt)
  
  if (tofile != FALSE)
  {
    dev.off()
  }
  return(invisible(plt))
}

hitdensity_data = function(dat, battleinfo)
{
  info = prepare_battleinfo(battleinfo, required=c("number", "place"), dat$raw)
  
  minutes = c()
  types = c()
  y_pos = c()
  for (i in seq(1, length(dat$raw$user)))
  {
    row = dat$raw[i,]
    if (row$mode == "PASSIVE_PLAY") next
    else if (row$mode == "AUTO") next
    
    time = as.POSIXct(row$timestamp, tz="UTC", origin="1970-01-01")
    minute = as.integer(format(time, format="%M"))
    second = as.integer(format(time, format="%S"))
    minute = minute + second/60
    
    y = switch(row$mode, DIRECT_PLAY=1, MANUAL=3)
    types = c(types, row$mode)
    types = c(types, "Any")
    minutes = c(minutes, minute, minute)
    y_pos = c(y_pos, y, 2)
  }
  
  df = data.frame(
    minute = minutes,
    type=factor(types, levels=c("Any", "MANUAL", "DIRECT_PLAY"),# "PASSIVE_PLAY"),
                labels=c("Any", "Manual", "Direct Play")),#, "Passive Play"))
    y=y_pos
  )
  
  # Calculate median values
  medians = c()
  for (type in levels(df$type))
  {
    medians = c(medians, median(df$minute[which(df$type == type)]))
  }
  df_medians = data.frame(x=medians, type=levels(df$type))
  df_medians$y = sapply(levels(df$type), switch, "Direct Play"=1,
                        "Any"=2, "Manual"=3)
  
  xlabs = seq(2,60,2)
  xlabs_str = as.character(xlabs)
  xlabs_str = ifelse(nchar(xlabs_str) == 1, paste0("0",xlabs_str), xlabs_str)
  
  return(list(data=df, medians=df_medians, xlabs=xlabs, xlabs_str=xlabs_str, info=info))
}


hitdensity_anim = function(dat, battleinfo=NULL, adjust=0.2, tofile=FALSE)
{
  info = prepare_battleinfo(battleinfo, required=c("number", "place", "start"), dat$raw)
  
  minutes = c()
  hours = c()
  types = c()
  for (i in seq(1, length(dat$raw$user)))
  {
    row = dat$raw[i,]
    if (row$mode == "PASSIVE_PLAY") next
    else if (row$mode == "AUTO") next
    
    time = as.POSIXct(row$timestamp, tz="UTC", origin="1970-01-01")
    minute = as.integer(format(time, format="%M")) + 1
    hour = floor(row$timestamp/3600.0 - floor(info$start/3600.0)+1)
    
    type = switch(row$mode,
                  "MANUAL" = "Manual",
                  "DIRECT_PLAY" = "Direct Play")
    types = c(types, type)
    types = c(types, "Any")
    minutes = c(minutes, minute, minute)
    hours = c(hours, hour, hour)
  }
  
  df = data.frame(
    minute = minutes,
    hour = factor(hours, levels=seq(max(hours))),
    type=factor(types, c("Any", "Manual", "Direct Play"))
  )
  
  xlabs = as.character(seq(2,60,2))
  xlabs = ifelse(nchar(xlabs) == 1, paste0("0",xlabs), paste0("",xlabs))
  
  group_sizes = ave(seq_len(nrow(df)), df$hour, df$type, FUN=length)
  df_clean = df[group_sizes >= 2,]
  singular = df[group_sizes == 1,]
  
  plt = ggplot() +
    geom_density(data=df_clean, aes(x=minute, color=type, fill=type, group=type),
                 alpha=0.35, adjust=adjust) +
    scale_fill_manual("Hit method", values=c("Any"="gray40","Manual"="orange", "Direct Play"="blue"),
                      limits=c("Any", "Manual", "Direct Play")) +
    scale_color_manual("Hit method", values=c("Any"="gray20","Manual"="orange2", "Direct Play"="blue2"),
                       limits=c("Any", "Manual", "Direct Play")) +
    scale_x_continuous(breaks=seq(2,60,2), labels = xlabs, expand=0) +
    gganimate::transition_states(hour, transition_length=2, state_length=1) +
    labs(title="Density of hits on cycle {closest_state}", subtitle=paste0("battle #", info$number, " in ", info$place)) +
    xlab("Minute of a cycle") +
    ylab("Density") +
    theme_standard
  
  if (nrow(singular) > 0)
  {
    plt = plt + geom_vline(data=singular, aes(xintercept=minute, color=type, group=type))
  }
  
  a = gganimate::animate(plt,
                         nframes=300, fps=8, units="in", width=8, height=4, res=200)
  
  if (tofile != FALSE)
  {
    gganimate::anim_save(tofile, animation=a)
  } else
  {
    print(a)
  }
  return(invisible(plt))
}

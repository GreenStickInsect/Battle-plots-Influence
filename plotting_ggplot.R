## Authors:
## -  GreenStickInsect aka Ogrodnik10

## This file contains functions handling the drawing of advanced plots which require ggplot2.
# Do not run this file directly, instead, source() it.

# Some of the functions provided might depend on functions,
# which you need to source() from "data_functions.R"

# Please be aware, following functions are often not designed to fail gracefully if invalid data is supplied.
# Use with care.

# Standard plot style for variety of charts
theme_standard = theme_minimal() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.text = element_text(size=rel(1)), legend.title = element_text(size=rel(1.2), face="bold", hjust=0.5))

# Plot style for piecharts
theme_piechart = theme_standard +
  theme(panel.grid = element_blank(), axis.text=element_text(size=rel(1), face="bold"),
        axis.title = element_blank())

# Draws a pie chart which compares usage of Influence IP, Direct Play and Discord BIP.
#   dat - A dataset.
#   battleinfo - An (optional) list of additional info about the battle, usually published along with IP spend data.
#   tofile - path to file to which plot should be exported as .png . If FALSE, instead draws plot within R. Defaults to FALSE.
#
#   returns: a ggplot object which was used to draw the chart
sourceplot_piechart = function(dat, battleinfo=NULL, tofile=FALSE)
{
  # The following is basically dark magic practiced through trial and error,
  # because ggplot2 is TERRIBLY documented and does VERY WEIRD things to data, for no apparent reason.
  # Have "fun" trying to understand anything.
  #
  # PS No, this is not optimized, I got it to a roughly working version, quit, and hope I won't need to come back
  
  if (! is.null(battleinfo$number) ) battle_num = battleinfo$number
  else battle_num = dat$raw$battle[1]
  
  num_p = nrow(dat$per_player)
  
  ipdata = dat$per_player
  dpdata = dat$per_player
  bipdata = dat$per_player
  
  ipdata$bipUsed = rep(0, length(ipdata$bipUsed))
  ipdata$DPUsed = rep(0, length(ipdata$DPUsed))
  ipdata$sumUsed = ipdata$ipUsed
  
  ipdata = ipdata[order(ipdata$sumUsed),]
  ipdata_rev = ipdata[order(ipdata$sumUsed, decreasing=TRUE),]
  
  dpdata$ipUsed = rep(0, length(dpdata$ipUsed))
  dpdata$bipUsed = rep(0, length(dpdata$bipUsed))
  dpdata$sumUsed = dpdata$DPUsed
  
  dpdata = dpdata[order(dpdata$sumUsed),]
  dpdata_rev = dpdata[order(dpdata$sumUsed, decreasing=TRUE),]
  
  bipdata$ipUsed = rep(0, length(bipdata$ipUsed))
  bipdata$DPUsed = rep(0, length(bipdata$DPUsed))
  bipdata$sumUsed = bipdata$bipUsed
  
  bipdata = bipdata[order(bipdata$sumUsed),]
  bipdata_rev = bipdata[order(bipdata$sumUsed, decreasing=TRUE),]
  
  sumvalues = c(sum(ipdata$sumUsed), sum(dpdata$sumUsed), sum(bipdata$sumUsed))
  values = c(ipdata$sumUsed, dpdata$sumUsed, bipdata$sumUsed)
  values_group_rev = c(ipdata_rev$sumUsed, dpdata_rev$sumUsed, bipdata_rev$sumUsed)

  groups = c("Influence IP", "Direct Play", "Discord BIP")
  cols = c("Influence IP"="skyblue1", "Direct Play"="blue2", "Discord BIP"="purple1")
  data = data.frame(group=factor(rep(groups, each=num_p), groups),
                    value=values)

  full_circle = sum(ipdata$sumUsed) + sum(dpdata$sumUsed) + sum(bipdata$sumUsed)
  circle_part = values_group_rev/full_circle * 360
  circle_rotation = (cumsum(circle_part) - circle_part/2 + 90)

  circle_rotation = c(rev(circle_rotation[1:num_p]),
                      rev(circle_rotation[(num_p+1):(num_p*2)]),
                      rev(circle_rotation[(num_p*2+1):(num_p*3)]))
  circle_rotation = ifelse(circle_rotation > 90 & circle_rotation <= 270, circle_rotation+180, circle_rotation)
  
  circle_part2 = sumvalues/full_circle * 360
  circle_rotation2 = (cumsum(circle_part2) - circle_part2/2)
  circle_rotation2 = ifelse(circle_rotation2 > 90 & circle_rotation2 <= 270, circle_rotation2+180, circle_rotation2)
  
  labels = c()
  label_cols = c()
  for (dat in list(ipdata, dpdata, bipdata))
  {
    namecol = which(colnames(dat) == "name")
    scorecol = which(colnames(dat) == "sumUsed")
    colcol = which(colnames(dat) == "color")
    for (rowi in seq(nrow(dat)))
    {
      labels = c(labels, ifelse(dat[rowi,scorecol] >= full_circle*0.015, dat[rowi,namecol], ""))
      label_cols = c(label_cols, dat[rowi,colcol])
    }
  }
  
  num_labels = format(sumvalues, scientific=F, big.mark=" ")
  num_labels = ifelse(circle_part2 >= 4, num_labels, "")
    
  csum = cumsum(sumvalues)
  pos = csum[length(csum)] - csum + sumvalues/2

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
                coord_radial("y", expand=F) +
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
                labs(title="Comparison of IP, Direct Play and Discord BIP usage",
                     subtitle=paste("in battle #", battle_num, sep=""))
  
  print(plt)
  options(warn=oldw)
  
  if (tofile != FALSE)
  {
    dev.off()
  }
  return(invisible(plt))
}

hitdensity = function(dat, battleinfo=NULL, tofile=FALSE)
{
  if (! is.null(battleinfo$number) ) battle_num = battleinfo$number
  else battle_num = dat$raw$battle[1]
  
  if (! is.null(battleinfo$start)) start = battleinfo$start
  else start = min(dat$raw$timestamp)
  
  if (! is.null(battleinfo$end)) end = battleinfo$end
  else end = max(dat$raw$timestamp)
  
  if (! is.null(battleinfo$battle_length)) battle_length = battleinfo$battle_length
  else battle_length = ceiling((end - start) / 60 / 60)
  
  minutes = c()
  types = c()
  for (i in seq(1, length(dat$raw$user)))
  {
    row = dat$raw[i,]
    
    time = as.POSIXct(row$timestamp, tz="UTC", origin="1970-01-01")
    minute = as.integer(format(time, format="%M")) + 1

    types = c(types, ifelse(row$directPlay, "Direct Play", "Manual"))
    types = c(types, "Any")
    minutes = c(minutes, minute, minute)
  }
  
  df = data.frame(
    minute = minutes,
    type=factor(types, c("Any", "Manual", "Direct Play"))
  )
  
  xlabs = as.character(seq(2,60,2))
  xlabs = ifelse(nchar(xlabs) == 1, paste0("0",xlabs), paste0("",xlabs))
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  
  plt = ggplot(df, aes(x=minute, color=type, fill=type)) +
    geom_density(alpha=0.35) +
    scale_fill_manual("Hit method", values=c("Any"="gray40","Manual"="orange", "Direct Play"="blue"),
                      limits=c("Any", "Manual", "Direct Play")) +
    scale_color_manual("Hit method", values=c("Any"="gray20","Manual"="orange2", "Direct Play"="blue2"),
                       limits=c("Any", "Manual", "Direct Play")) +
    scale_x_continuous(breaks=seq(2,60,2), labels = xlabs) +
    labs(title="Density of hits across a cycle", subtitle=paste0("in battle #", battle_num)) +
    xlab("Minute of a cycle") +
    ylab("Density") +
    theme_standard
  
  print(plt)
  
  if (tofile != FALSE)
  {
    dev.off()
  }
  return(invisible(plt))
}

## Authors:
## -  GreenStickInsect aka Ogrodnik10

## This file contains functions handling the drawing of advanced plots which require ggplot2.
# Do not run this file directly, instead, source() it.

# Some of the functions provided might depend on functions,
# which you need to source() from "data_functions.R"

# Please be aware, following functions are often not designed to fail gracefully if invalid data is supplied.
# Use with care.

# Plot style for piecharts
theme_piechart = theme_minimal() +
  theme(panel.grid = element_blank(), axis.text=element_text(size=rel(1.2), face="bold"),
        axis.title = element_blank(), plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5), legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1.2), face="bold", hjust=0.5))

# Draws a pie chart which compares usage of Influence IP, Direct Play and Discord BIP.
#   dat - A dataset.
#   battleinfo - An (optional) list of additional info about the battle, usually published along with IP spend data.
#   tofile - path to file to which plot should be exported as .png . If FALSE, instead draws plot within R. Defaults to FALSE.
#
#   returns: a ggplot object which was used to draw the chart
sourceplot_piechart = function(dat, battleinfo=NULL, tofile=FALSE)
{
  if (! is.null(battleinfo$number) ) battle_num = battleinfo$number
  else battle_num = dat$raw$battle[1]
  
  ipdata = dat$per_player
  dpdata = dat$per_player
  bipdata = dat$per_player
  
  ipdata$bipUsed = rep(0, length(ipdata$bipUsed))
  ipdata$DPUsed = rep(0, length(ipdata$DPUsed))
  ipdata$sumUsed = ipdata$ipUsed
  
  dpdata$ipUsed = rep(0, length(dpdata$ipUsed))
  dpdata$bipUsed = rep(0, length(dpdata$bipUsed))
  dpdata$sumUsed = dpdata$DPUsed
  
  bipdata$ipUsed = rep(0, length(bipdata$ipUsed))
  bipdata$DPUsed = rep(0, length(bipdata$DPUsed))
  bipdata$sumUsed = bipdata$bipUsed
  
  nullcol = rgb(0,0,0,alpha=0)
  density = c(rep(-1, length(ipdata$name)), rep(-1, length(dpdata$name)), rep(35, length(bipdata$name)))
  col = c(rep(nullcol, length(ipdata$name)), rep(nullcol, length(dpdata$name)), rep("black", length(bipdata$name)))
  
  exprs = c(bquote("-"))
  for (i in bipdata$name)
  {
    exprs = c(exprs, as.expression(substitute(paste(bold(i)))) )
  }
  exprs = exprs[-1]
  bipdata$name = exprs
  
  groups = c("Influence IP", "Direct Play", "Discord BIP")
  values = c(sum(ipdata$sumUsed), sum(dpdata$sumUsed), sum(bipdata$sumUsed))
  data = data.frame(group=factor(groups, groups),
                    value=values,
                    cols=c("Influence IP"="skyblue1", "Direct Play"="blue2", "Discord BIP"="purple1"))
  
  csum = cumsum(values)
  pos = csum[length(csum)] - csum + values/2
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  
  plt = ggplot(data=data, mapping=aes(x="", y=value )) +
                geom_col(aes(fill=group), color="white") +
                scale_fill_manual("IP source",
                    values = data$cols,
                    limits = groups) +
                coord_polar("y") +
                theme_piechart +
                geom_text(aes(group=group, label=c("IP", "DP", "BIP"), fontface="bold"),
                           position = position_stack(vjust=0.5)) +
                scale_y_continuous(breaks=pos, labels=format(values, scientific=F, big.mark=" ")) +
                labs(title="Comparison of IP, Direct Play and Discord BIP usage",
                     subtitle=paste("in battle #", battle_num, sep=""))
  
  print(plt)
  if (tofile != FALSE)
  {
    dev.off()
  }
  return(plt)
}

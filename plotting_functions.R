## Authors:
## -  GreenStickInsect aka Ogrodnik10

## This file contains functions handling the drawing of plots.
# Do not run this file directly, instead, source() it.

# Some of the functions provided might depend on functions,
# which you need to source() from "data_functions.R"

# Please be aware, following functions are often not designed to fail gracefully if invalid data is supplied.
# Use with care.


# Decide text colors based on which will be more visible on specified background colors.
# Only uses black or white text as of now.
#   colorlist - a vector of background colors, as RGB or color name
#
#   returns: vector of same length as colorlist, with colors for text.
textcontrast = function(colorlist)
{
  return(ifelse(apply(col2rgb(colorlist), 2, mean) >= 128, "black", "white"))
}

# Draws rectangle-shaped areas filled with dots.
# Primarily intended for shading.
#   x - vector of x positions of left boundaries of areas
#   y - vector of y positions of bottom boundaries of areas
#   x2 - vector of x positions of right boundaries of areas
#   y2 - vector of y positions of top boundaries of areas
#   by - spacing between dots on x axis
#   by2 - spacing between dots on y axis
#   col - color of dots
#   ... - additional parameters to supply to points()
dot_box = function(x, y, x2, y2, by, by2, col="black", ...)
{
  if (length(x) != length(x2) || length(x) != length(y) || length(x) != length(y2))
  {
    stop("Function dot_box: Lenghts of coordinate vectors differ!")
  }
  
  if (length(by) < length(x))
  {
    byn = rep(by, length(x)/length(by))
    if (length(byn) < length(x))
    {
      byn = c(byn, by[1:length(x)-length(byn)])
    }
    by = byn
  }
  if (length(by2) < length(x))
  {
    by2n = rep(by2, length(x)/length(by2))
    if (length(by2n) < length(x))
    {
      by2n = c(by2n, by[1:length(x)-length(by2n)])
    }
    by2 = by2n
  }
  if (length(col) < length(x))
  {
    coln = rep(col, length(x)/length(col))
    if (length(coln) < length(x))
    {
      coln = c(coln, col[1:length(x)-length(coln)])
    }
    col = coln
  }
  
  for (i in seq(length(x)))
  {
    ptsx = seq(x[i], x2[i], by=by[i])
    ptsy = seq(y[i], y2[i], by=by2[i])
    
    diffx = x2[i] - ptsx[length(ptsx)]
    diffy = y2[i] - ptsy[length(ptsy)]
    ptsx = ptsx + diffx/2
    ptsy = ptsy + diffy/2
    
    all_ptsx = rep(ptsx, length(ptsy))
    all_ptsy = rep(ptsy, each=length(ptsx))
    
    shft=0
    for (j in seq(0,length(ptsy)-1, by=2)*length(ptsx)+1)
    {
      jj = j+shft
      all_ptsx[seq(jj, jj+length(ptsx)-1)] = all_ptsx[seq(jj, jj+length(ptsx)-1)] + by[i]/2
      all_ptsx = append(all_ptsx, all_ptsx[jj]-by[i], after=jj-1)
      all_ptsy = append(all_ptsy, all_ptsy[jj], after=jj-1)
      shft = shft+1
    }
    #all_ptsy = all_ptsy + sample(-2:2,length(all_ptsy), replace=TRUE) * by2[i] / 10
    
    points(all_ptsx, all_ptsy, cex=0.1, bg=col[i], col=col[i], ...)
  }
}

# Draws a plot legend explaining IP, DirectPlay and BIP shading on some of barplots
#   x - x position of the legend
#   y - y positions of the lengend
#   xjust - x justification of the legend. See documentation of legend() for details.
#   yjust - y justification of the legend. See documentation of legend() for details.
#   ... - additional parameters to supply to legend()
iptype_legend = function(x, y, xjust=0, yjust=1, ...)
{
  labels = c("Influence IP", "Direct Play", "Discord BIP")
  lgp = legend(x, y, col=c("white", "black", "black"), density=c(0, 0, 25), y.intersp = 1,
               legend=labels, xjust=xjust, yjust=yjust, ...)
  
  cx = par("cxy")[1]
  cy = par("cxy")[2]
  bott = lgp$rect$top - lgp$rect$h
  txth = max(mapply(strheight, labels))
  lh = par("lheight")
  
  #rect(lgp$rect$left+1*cx, bott+txth*2,
  #     lgp$rect$left+(1+0.8)*cx, bott+txth*2-0.5*cy, col="red")
  
  dotspacex = 4*(par("cxy")[1]/par("cra")[1])
  dot_box(lgp$rect$left+1.14*cx, bott+txth*2+(0.1-0.5)*cy+1*cy*lh,
          lgp$rect$left+(1+0.8-0.14)*cx, bott+txth*2-0.1*cy+1*cy*lh, by=dotspacex, by2=0.15*cy)
}

# Draws a barplot with scores of each player in dataset.
#   dat - a dataset
#   lim - a numerical vector of y axis limits, ordered as (min, max)
#   horiz - whether the barplot should be drawn horizontally. Defaults to FALSE.
#   text.cex - numeric character expansion vector for text()
#   tofile - path to file to which plot should be exported as .png . If FALSE, instead draws plot within R. Defaults to FALSE.
#   ... - additional parameters to supply to barplot()
#
#   returns: vector of x positions of bars
player_plot = function(dat, lim=NULL, horiz=FALSE, text.cex=1, tofile=FALSE, ...)
{
  
  sumvals = unlist(dat$per_player$sumUsed)
  
  vals = data.frame(matrix(rep(0, length(sumvals)*3), nrow=3))
  vals[1,] = unlist(dat$per_player$bipUsed)
  vals[2,] = unlist(dat$per_player$DPUsed)
  vals[3,] = unlist(dat$per_player$ipUsed)
  colnames(vals) = dat$per_player$name
  
  if (is.null(lim))
  {
    vmax = max(sumvals)
    vmin = min(sumvals)
  } else
  {
    vmin = lim[1]
    vmax = lim[2]
  }
  ylim = c(vmax*-0.3, vmax + vmax*0.3)
  xlim = NULL
  if (horiz)
  {
    xlim = ylim
    ylim = NULL
  }
  
  valmatrix = matrix(c(vals[,1], rep(0, length(sumvals)*3 - 3)), ncol=1)
  if (length(sumvals) > 1)
  {
    for (i in seq(2, length(sumvals)))
    {
      valmatrix = cbind(valmatrix, c(rep(0, (i-1)*3), vals[,i], rep(0, length(sumvals)*3 - i*3)))
    }
  }
  
  barcolors = rep(dat$per_player$color[1], 3)
  for (i in seq(2, length(dat$per_player$color))) barcolors = c(barcolors, rep(dat$per_player$color[i], 3))
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  positions = barplot(valmatrix, names.arg=dat$per_player$name, horiz=horiz,
                      col=barcolors, ylim=ylim, xlim=xlim, space=0.1,
                      main=bquote(atop(paste(.(dat$faction), " players in battle #", .(dat$raw$battle[1]), sep=""), scriptstyle("(IP spent, before bonuses)") )),
                      axes=FALSE, las=1, srt=35, axisnames=FALSE, ...)
  
  positions = barplot(valmatrix, horiz=horiz, add=TRUE, col=c("black", rgb(0,0,0,alpha=0), rgb(0,0,0,alpha=0)),
                      ylim=ylim, xlim=xlim, space=0.1,
                      main="", axes=FALSE, axisnames=FALSE, density=c(25, -1, -1), ...)
  
  if (length(positions) > 1) { bardist = (positions[2] - positions[1]) / 1.1
  } else { bardist = positions[1] * 2 / 1.2 }
  
  dotspacey = max(sumvals)/100
  dotspacex = 4*(par("cxy")[1]/par("cra")[1])
  
  dot_box(positions-0.5*bardist+dotspacex/2, unlist(vals[1,]),
          positions+0.5*bardist-dotspacex/2, unlist(vals[1,]+vals[2,]),
          by=dotspacex, by2=dotspacey)
  
  text(positions, rep(vmax*-0.05, length(positions)), labels=dat$per_player$name,
       cex=text.cex, srt=45, adj = c(1,0.5))
  text(positions, sumvals + vmax*0.03, labels=format(sumvals, big.mark=" ", scientific=FALSE),
       cex=text.cex, srt=45, adj=c(0, 0))
  
  #widths = sapply(format(sumvals, big.mark=" ", scientific=FALSE), strwidth)
  #print(widths)
  #text(positions+widths, sumvals + vmax*0.03, labels="test", srt=45, adj=c(0,0))
  
  if (! horiz)
  {
    x=par("usr")[2]
    y=vmax + vmax*0.3
  } else
  {
    x=vmax + vmax*0.3
    y=par("usr")[2]
  }
  iptype_legend(x, y, 1, 1)
  
  if (tofile != FALSE)
  {
    print(positions)
    dev.off()
  }
  return(invisible(positions))
}


contribution_plot = function(dat, tofile=FALSE, ...)
{
  contrib_matrix = matrix(rep(0, length(faction_colors)), ncol=1, byrow=FALSE)
  rownames(contrib_matrix) = names(faction_colors)
  top_faction_arr = c(NA_character_)
  
  for (idx in seq(length(dat$raw$user)))
  {
    event = dat$raw[idx,]
    
    supported = ifelse(event$action == "ATTACK", toupper(event$attacker), toupper(event$defender))
    supported_idx = match(supported, names(faction_colors))[1]
    
    if (! event$user %in% colnames(contrib_matrix))
    {
      contrib_matrix = cbind(contrib_matrix, rep(0, length(faction_colors)))
      colnames(contrib_matrix)[ncol(contrib_matrix)] = event$user
    }
    
    user_idx = match(event$user, colnames(contrib_matrix))
    contrib_matrix[supported_idx, user_idx] = contrib_matrix[supported_idx, user_idx] + event$sumUsed
  }
  contrib_matrix = contrib_matrix[,-1]
  
  top_faction_indexes = max.col(t(contrib_matrix))
  for (idx in top_faction_indexes)
  {
    top_faction_arr = c(top_faction_arr, names(faction_colors)[idx])
  }
  top_faction_arr = top_faction_arr[-1]
  
  faction_sum_scores = c()
  for (idx in seq(length(faction_colors)))
  {
    faction_name = names(faction_colors)[idx]
    faction_scores = contrib_matrix[idx, which(top_faction_arr == faction_name)]
    if (length(faction_scores) == 0 ||
        (length(faction_scores) == 1 && is.nan(faction_scores)) )
    {
      faction_sum_scores = c(faction_sum_scores, NA_integer_)
    }
    else faction_sum_scores = c(faction_sum_scores, sum(faction_scores))
  }
  names(faction_sum_scores) = names(faction_colors)
  faction_sum_scores = sort(faction_sum_scores, decreasing=TRUE)
  
  border_indexes = c(0)
  ordered_matrix = matrix(rep(0, length(faction_colors)), ncol=1, byrow=FALSE)
  for (idx in seq(length(faction_sum_scores)))
  {
    faction_name = names(faction_sum_scores)[idx]
    
    contribs = which(top_faction_arr == faction_name)
    backup = "" # Because R is stupid enough to automatically coerce matrices to arrays DISCARDING THEIR COLNAME VALUES
    if (length(contribs) == 1) backup = colnames(contrib_matrix)[contribs]
    
    faction_scores = as.matrix(contrib_matrix[, contribs])
    if (length(faction_scores) == 0) next
    
    faction_scores = as.matrix(faction_scores[,order(faction_scores[faction_name,], decreasing = TRUE)])
    
    ordered_matrix = cbind(ordered_matrix, faction_scores)
    if (length(contribs) == 1) colnames(ordered_matrix)[ncol(ordered_matrix)] = backup
    border_indexes = c(border_indexes, border_indexes[length(border_indexes)] + ncol(faction_scores))
  }
  ordered_matrix = ordered_matrix[,-1]
  border_indexes = border_indexes[-1]
  
  vmax = max(apply(contrib_matrix, 2, sum))
  
  ylim = c(vmax*-0.3, vmax + vmax*0.3)
  
  barcolors = unlist(faction_colors, use.names=FALSE)
  usernames = c()
  faction_cols = c()
  for (usr in colnames(ordered_matrix))
  {
    tmp = match(usr, dat$per_player$user)
    usernames = c(usernames, dat$per_player$name[tmp])
    faction_cols = c(faction_cols, dat$per_player$color[tmp])
  }
  
  width=2
  xlim = c(0, width * 1.2 * (ncol(ordered_matrix) + 1))
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  positions = barplot(ordered_matrix, col=barcolors,
                      ylim=ylim, xlim=xlim, width=width,
                      main=bquote(atop(paste("Players' support to factions in battle #", .(dat$raw$battle[1]), sep=""), scriptstyle("(IP spent, before bonuses)") )),
                      axes=FALSE, las=1, srt=35, axisnames=FALSE, ...)
  
  bardist = positions[2] - positions[1]
  tickdist = round(vmax*1.1/15, digits=-3)
  for (idx in seq(length(border_indexes)))
  {
    faction_names = names(faction_sum_scores)[seq(length(border_indexes))]
    lineposits = positions[border_indexes] + bardist/2
    
    segments(lineposits, 0,
             lineposits, ylim[2],
             col=unlist(faction_colors[faction_names]), lty="dashed")
    text(lineposits, vmax, labels=paste(faction_names, " ", sep=""), adj=c(1, 0), col=unlist(faction_colors[faction_names]))
  }
  
  ofst_x = bardist*0.005
  ofst_y = tickdist*0.005
  #exp_usernames = gsub("^ ", "", gsub("(.)", " \\1", usernames))
  #exp_usernames = format(usernames, width = strwidth(usernames)*1.5)
  for (i in seq(5))
  {
    X = positions
    Y = rep(vmax*-0.05, length(positions))
    COL = "black"
    if (i==1) X = X + ofst_x
    else if (i==2) X = X - ofst_x
    else if (i==3) Y = Y + ofst_y
    else if (i==4) Y = Y - ofst_y
    else if (i==5) COL = faction_cols
    
    text(X, Y, labels=usernames, family="mono",
         srt=45, adj = c(1,0.5), col=COL, cex=1.1)
  }
  
  axis(2, at=seq(0, vmax*1.1, by=tickdist),
       labels=format(seq(0, vmax*1.1, by=round(vmax*1.1/15, digits=-3)), scientific=FALSE, big.mark=","),
       lty="solid", las=2, line=-2, cex.axis=1)
  
  if (tofile != FALSE)
  {
    print(positions)
    dev.off()
  }
  
  return(invisible(positions))
}

bonusplot = function(allset, lim=NULL, horiz=FALSE, text.cex=1, tofile=FALSE, ...)
{
  # per player
  allset$per_player$avgbonus = (allset$per_player$ipApplied / allset$per_player$sumUsed)-1
  
  if (is.null(lim))
  {
    vmax = ceiling(max(allset$per_player$avgbonus))
    vmin = floor(min(allset$per_player$avgbonus))
  } else
  {
    vmin = lim[1]
    vmax = lim[2]
  }
  
  ylim = c(min(0, vmin) - (vmax-vmin)*0.2, vmax + (vmax-vmin)*0.2)
  xlim = NULL
  if (horiz)
  {
    xlim = ylim
    ylim = NULL
  }
  allset$per_player = allset$per_player[order(allset$per_player$avgbonus, decreasing=TRUE),]
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  positions = barplot(allset$per_player$avgbonus, names.arg=allset$per_player$name, horiz=horiz,
                      col=allset$per_player$color, ylim=ylim, xlim=xlim,
                      main=paste("Average IP bonus per player in battle #", allset$raw$battle[1], sep=""),
                      axes=FALSE, las=1, axisnames=FALSE, ...)
  
  namepos = allset$per_player$avgbonus
  for (i in seq(length(namepos))) namepos[i] = min(0, namepos[i]) - (vmax-vmin)*0.05
  text(positions, namepos,
       labels=allset$per_player$name, srt=45, adj = c(1,0.5))
  
  positive = ifelse(allset$per_player$avgbonus < 0, 0, 1)
  text(positions, (allset$per_player$avgbonus * positive)+(vmax-vmin)*0.05,
       cex = text.cex, srt=60, adj=c(0,0),
       labels=paste(format(allset$per_player$avgbonus*100, big.mark=" ", digits=2, scientific=FALSE),"%", sep=""))
  
  if (tofile != FALSE)
  {
    print(positions)
    dev.off()
  }
}

vsplot = function(teams, teamnames=list(NA_character_, NA_character_), tofile=FALSE, textcols=list(NA_character_, NA_character_), ...)
{
  teams = as.list(teams)
  if (length(teams) < 2)
  {
    stop("Function vsplot: Too few teams supplied - must be no less than 2.")
  }
  for (i in seq(length(teams)))
  {
    teams[[i]] = datasort(teams[[i]])
    if (length(teamnames) < i ||
        is.na(teamnames[[i]]))
    {
      teamnames[[i]] = paste("Team", i)
    }
    if (length(textcols) < i)
    {
      textcols[[i]] = NA_character_
    }
  }
  
  # Create data frame for summary team data
  teamsdf = data.frame(name=rep("", length(teams)), ipUsed=rep(0, length(teams)),
                       DPUsed=rep(0, length(teams)), bipUsed=rep(0, length(teams)),
                       sumUsed=rep(0, length(teams)), ipApplied=rep(0, length(teams)),
                       faction=rep(0, length(teams)), color=rep("", length(teams)))
  
  # Numerical columns
  numnames = c("ipUsed", "DPUsed", "bipUsed", "sumUsed", "ipApplied")
  for (i in seq(length(teams)))
  {
    teamsdf[i,numnames] = apply(teams[[i]][,numnames], 2, sum)
    teamsdf[i, c("name", "faction", "color")] = c(teamnames[[i]], "ZZ", "white")
  }
  
  vmax = max(teamsdf$sumUsed)
  vmin = 0
  ylim = c(vmin, vmax + (vmax-vmin)*0.3)
  
  # Prepare label positions for each bar
  labposlst = list(null=c())
  pnamelabs = list(null=list())
  for (i in seq(length(teams)))
  {
    cteam = teams[[i]]
    csum = cumsum(cteam$sumUsed[which(cteam$sumUsed >= max(teamsdf$sumUsed)*0.02)])
    if (length(csum) > 1)
    {
      labpos = rep(0, length(csum))
      
      for (j in seq(2, length(csum))) { labpos[j] = csum[j] - (csum[j] - csum[j-1])/2 }
      labpos[1] = csum[1] - (csum[1] - ylim[1])/2
    } else if (length(csum) == 1)
    {
      labpos = c(csum[1] - (csum[1] - ylim[1])/2)
    } else labpos = c()
    
    labposlst[[teamnames[i]]] = labpos
    pnamelabs[[teamnames[i]]] = cteam$name[which(cteam$sumUsed >= max(teamsdf$sumUsed)*0.02)]
  }
  labposlst = labposlst[-which(names(labposlst)=="null")]
  pnamelabs = pnamelabs[-which(names(pnamelabs)=="null")]
  
  # Prepare matrix of data for barplot
  vals = c()
  sizes = vapply(teams, nrow, c(0))
  for (i in seq(length(teams)))
  {
    vals = c(vals, rep(0, sum(sizes[0:(i-1)])), teams[[i]]$sumUsed, rep(0, sum(sizes[-(0:i)])))
  }
  stacked = matrix(vals, ncol=length(teams), byrow=FALSE)
  
  # Text coloring depending on each player's color, to assure good visibility
  for (i in seq(length(teams)))
  {
    if (is.na(textcols[[i]]))
    {
      textcols[[i]] = textcontrast(teams[[i]]$color)
    }
  }
  
  # Bar colours
  bcols = c()
  for (i in seq(length(teams)))
  {
    bcols = c(bcols, teams[[i]]$color)
  }
  
  # Plot title
  ttl = teamnames[1]
  for (name in teamnames[-1]) { ttl = paste(ttl, "vs", name) }
  
  # Auto writing to file utility
  if (tofile != FALSE) png(tofile, 1280, 720)
  
  # Bars
  positions = barplot(stacked, col=bcols, ylim=ylim, names.arg=teamsdf$name, axes=FALSE,
                      main=ttl, horiz=FALSE, ...)
  
  # Player name labels for each team
  for (i in seq(length(teams)))
  {
    if (length(pnamelabs[[i]]) > 0)
    {
      text(y=labposlst[[i]], x=positions[i], labels=pnamelabs[[i]],
           col=textcols[[i]], cex=0.9)
    }
  }
  
  text(x=positions, y=teamsdf$sumUsed+(vmax-vmin)*0.05,
       labels=format(teamsdf$sumUsed, big.mark=" ", scientific=FALSE))
  
  if (tofile != FALSE)
  {
    print(positions)
    dev.off()
  }
  return(invisible(list(matr=stacked, labels=pnamelabs, labelpos=labposlst, labelcol=textcols)))
}

# Nex vs all others
nexplot = function(dat, tofile=FALSE, ...)
{
  nexpos = which(dat$per_player$name == "Nex")
  if (length(nexpos) == 0) return(invisible(NULL))
  datanex = dat$per_player[nexpos,] # Nex
  dataothers = dat$per_player[-nexpos,] # Everyone except Nex
  
  vsplot(datanex, dataothers, c("One man army", "Others"), tofile=tofile, ...)
}

sourceplot = function(dat, tofile=FALSE, ...)
{
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
  
  if (tofile != FALSE) png(tofile, 1280, 720)
  
  data = vsplot(list(ip=ipdata, dp=dpdata, bip=bipdata), c("Influence IP", "Direct Play", "Discord BIP"), ...)
  positions = barplot(data$matr, add=TRUE, density=density, col=col,
                      axes=FALSE, main="")
  
  if (length(data$labels[[3]]) > 0)
  {
    text(y=data$labelpos[[3]], x=positions[3], labels=data$labels[[3]],
         col="white", cex=0.9)
  }
  
  if (tofile != FALSE)
  {
    print(positions)
    dev.off()
  }
}

scaleplot = function(dat, tofile=FALSE, ...)
{
  lower = dat$per_player[-1,]
  upper = dat$per_player[1,]
  
  while (sum(upper$sumUsed) < sum(lower$sumUsed))
  {
    upper = rbind(upper, lower[1,])
    lower = lower[-1,]
  }
  
  vsplot(list(top=upper, rest=lower), c(paste("Top scorers (", nrow(upper), ")", sep=""),
                                        paste("All others (", nrow(lower), ")", sep="")), tofile=tofile, ...)
}

timeline = function(sets, battleinfo, tofile=FALSE, ...)
{
  if (!(is.null(battleinfo$start) | is.na(battleinfo$start))) start = battleinfo$start
  else start = min(sets$ALL$raw$timestamp)
  
  max_val = 0
  timelines = list(none=list(name="", color="", per_hour=list()))
  for (faction in sets)
  {
    per_hour = as.list(rep(0, battleinfo$battle_length))
    names(per_hour) = seq(1, battleinfo$battle_length)
    
    for (i in seq(1, length(faction$raw$user)))
    {
      row = faction$raw[i,]
      hour = floor( (row$timestamp - start) / 3600.0) + 1
      per_hour[[hour]] = per_hour[[hour]] + row$sumUsed
    }
    
    if (max(unlist(per_hour)) > max_val) max_val = max(unlist(per_hour))
    timelines[[faction$faction]] = list(name=faction$faction, color=faction$color, per_hour=per_hour)
  }
  timelines = timelines[-1]
  
  top_lim = max_val*1.30
  
  if (tofile != FALSE) png(tofile, 1920, 1080)
  
  out = plot(names(per_hour), y=NULL, type="n", xlim=c(0, battleinfo$battle_length), ylim=c(0, top_lim),
             main=bquote(atop(paste("Timeline of battle #", .(sets$ALL$raw$battle[[1]]), sep=""), scriptstyle("(IP spent, before bonuses)"))),
             xlab="", ylab="IP", axes=FALSE)
  
  ax_names = c()
  for (i in seq(0, battleinfo$battle_length))
  {
    ax_names = c(ax_names, as.expression(bquote( scriptstyle(.(format(as.POSIXct(battleinfo$start+3600*i, tz="UTC", origin="1970-01-01"),
                                                                      format="%H:%m"))) )))
  }
  axis(1, at=c(0, names(per_hour)), lty="solid", labels=seq(0, battleinfo$battle_length), line=0)
  axis(1, at=c(0, names(per_hour)), lty="solid", labels=ax_names, line=1, tick=FALSE)
  axis(2, at=seq(0, max_val*1.1, by=round(max_val*1.1/15, digits=-3)), labels=format(seq(0, max_val*1.1, by=round(max_val*1.1/15, digits=-3)), scientific=FALSE),
       lty="solid", las=2, line=-2, cex.axis=1)
  
  title(xlab="Hours since battle start", line=par("mgp")[1])
  title(xlab=bquote(scriptstyle("(Hour in UTC)")), line=par("mgp")[1]+1)
  
  for (i in seq(1, length(timelines)))
  {
    model = loess(value ~ hour,
                  data.frame(value = unlist(timelines[[i]]$per_hour), hour = names(per_hour)), span=0.1)
    newdata = data.frame(hour=seq(1, battleinfo$battle_length, by=0.25))
    p = predict(model, newdata)
    
    for (j in seq(1, length(p)))
    {
      if (is.na(p[j]) || p[j] < 0)
      {
        p[j] = 0
      }
    }
    
    par(new=TRUE)
    plot(c(0, newdata$hour), c(0, p), type="l", xlim=c(0, battleinfo$battle_length), ylim=c(0, top_lim), main="",
         xlab="", ylab="", axes=FALSE, col=timelines[[i]]$color, lwd=3)
  }
  
  segments(seq(0, battleinfo$battle_length, by=8), 0,
           seq(0, battleinfo$battle_length, by=8), max_val*1.1,
           col="gray40", lty="dashed")
  
  #rect(-1, max_val*1.15, battleinfo$battle_length, max_val*1.4, col="white", border="white")
  cols = c()
  faction_names = c()
  for (faction in sets)
  {
    cols = c(cols, faction$color)
    faction_names = c(faction_names, faction$faction)
  }
  legend(battleinfo$battle_length, top_lim*1, faction_names, fill=cols, xjust=1, yjust=1, ncol=3, cex=1,
         x.intersp=0.6, y.intersp=0.8)
  
  if (tofile != FALSE)
  {
    print(out)
    dev.off()
  }
  
  return(invisible(timelines))
}
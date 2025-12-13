## Authors:
## -  GreenStickInsect aka Ogrodnik10


# A small troubleshooting note:
# Sometimes output devices randomly get messed up and you might see your plots not drawn at all,
# or stubbornly drawn into some file even when you don't tell them to.
# (I don't think this is an issue with my code, either something with RStudio or R itself)
# When this happens, run command 'dev.off()' a few times, until it says "null device".
# This should close all output devices and let a default one open on next plot.


# Set work dir to current script location.
# Careful - this might not work if ran otherwise than from within RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Source required functions ###

# data processing functions
source("data_functions.R")

# plotting functions
source("plotting_functions.R")

# If ggplot2 is available, source advanced plotting functions
ggplotgraphics = FALSE
if (require("ggplot2"))
{
  ggplotgraphics = TRUE
  library(ggplot2)
  
  source("plotting_ggplot.R")
} else
{
  cat("WARNING: Library ggplot2 is not installed.
      Some plots cannot be drawn without it and will be skipped or replaced with simpler version.\n")
}


### Manually changed variables ###

# Directory in which data files are and where plots will be generated
directory = "./battle_xxx"

# Names of files with data
filenames = c("ip_spend_battle_125.csv")
battleinfofile = "battleinfo.txt"

# Technically, if you know what you're doing, you can set the directory to whatever
# you like and paths to input files located completely elsewhere,
# thus allowing you to have separate folders for input and output


# Whether to export to files
#   FALSE - draw the plots within R
#   TRUE - instead, draw the plots as .png files
#
# Note: output file names are defined in the drawing code below, separately for each plot.
# You can change them if you wish.
export = FALSE


# RGB (in hex) colors of each team. This also serves as a declaration of known teams and their names.
# Be careful if you're editing that, wrong definition can easily make plots malfunction
faction_colors = data.frame(CI="#1aa44f", MT="#fc8c36", DC="#3f5fde",
                            PS="#beff64", BB="#ff496c", FG="#0099ff",
                            CO="#9c6aff", CR="#b20000", NO_TEAM="#303030",
                            XX="#303030")


### Generate datasets ###
# You probably don't want to touch this section, unless you REALLY know what you're doing
# Though, if you have some weird, unusual data, you might need to do that to make it work.
# Well, good luck.

# We'll be working in output directory from now on
setwd(directory)


## Try to obtain battle info
# Data from older battles does not have this, so code needs to be fault-proof
if (file.access(battleinfofile)[1] == 0)
{
  battleinfo = read_battle_data(battleinfofile) # see data_functions.R for details
} else
{
  battleinfo = list()
}


## If some of required data is missing, estimate it
# WARNING: battle length is then set arbitrarily. Old battles always lasted 48-49 hours,
#          but if you have data from battle which lasted different amount of time AND does not
#          have battle info file - you need to tweak this manually.
#          (or disable (comment out), since technically MOST plots should work without that)
if ( ! is.null(battleinfo$start) &
     ! is.null(battleinfo$end))
{
  battleinfo$battle_length = ceiling((battleinfo$end - battleinfo$start) / 60 / 60) # hours
} else
{
  battleinfo$battle_length = 49 # hours
}

## Create temporary list of data sets, generated from each input file.

# Dataset format is as follows:
#   List of:
#     raw - data frame with raw data (as read from CSV, every IP input event separately)
#     faction - name of team which this dataset belongs to
#     color - color of said team
#     per_player - (not yet here, we'll add that few lines down) data frame with total scores of each player
#
# For details, see inline documentation of prepare_someset() in data_functions.R

nonesets = list(null=list())
for (i in seq(length(filenames)))
{
  tmp = prepare_someset(filenames[i])
  nonesets[[paste("None",i,sep="")]] = tmp
}
rm(i, tmp) # clean up
nonesets = nonesets[-1] # remove temporary list


## Join the temporary data sets into one big
allset = list(raw=nonesets[[1]]$raw, faction="All", color="gray60")
for (i in seq(length(nonesets)))
{
  if (i == 1) next
  allset$raw = rbind(allset$raw, nonesets[[i]]$raw)
}

# Create a list of datasets, containing the "ALL" dataset created above
sets = list(ALL=allset)
rm(i, nonesets, allset) # clean up, we won't need those anymore

# Generate the missing per_player data.frame and add it to "ALL" dataset
sets$ALL$per_player = prepare_per_player(sets$ALL$raw) # see data_functions.R for details

# Generate separate dataset for every team and add those to list of datasets
# Note: those datasets are stored in the list under name corresponding to its team's (short) name
# e.g. sets$PS is dataset documenting performance of Protectores Silva
# Known faction names are in "faction_colors" data.frame defined above
sets = append(sets, prepare_factionsets(sets$ALL))


### Drawing the plots ###

# Draws the standard set of plots
# This can be edited to customize, though you might want to first have a look at
# inline plot documentation in plotting_functions.R and plotting_ggplot.R


## Per player plots

# This includes "All Players" plot, as well as "Per faction" plots
# Basically just plots total scores of every player in a dataset
for (s in sets)
{
  text.cex = 1
  if (s$faction == "All") text.cex = 0.8
  
  tofile = FALSE
  if (export) tofile = paste("Plot_players_", s$faction, ".png", sep="")
  player_plot(s, battleinfo, text.cex=text.cex, cex.names=0.8, tofile=tofile)
}
rm(s, text.cex) # cleanup


## Team plot

# Compare summary scores of each team.
#
# Internally utilizes the "vsplot" function.
if (!export) {teamplot(sets, battleinfo)
} else teamplot(sets, battleinfo, tofile="Teamplot.png")


## Contribution plot

# Which teams did every player help
if (! export) {contribution_plot(sets$ALL, battleinfo)
} else contribution_plot(sets$ALL, battleinfo, tofile="Contribplot.png")

# Which teams did every player oppose (fight against)
if (! export) {contribution_plot(sets$ALL, battleinfo, inverted=TRUE)
} else contribution_plot(sets$ALL, battleinfo, tofile="Oppositionplot.png", inverted=TRUE)


## Bonuses plot

# How high bonuses each player utilized.
# Be warned: this just divides "ipApplied" column by sum of
# "Used" columns (in fact, datasets already have a "sumUsed" column defined)
# It WILL stop being accurate if formula for ipApplied ever changes to include something besides the bonus
if (! export) {bonusplot(sets$ALL, battleinfo)
} else bonusplot(sets$ALL, battleinfo, tofile="Bonusplot.png")


## Scaleplot

# Divides players into 2 teams, where 1st is the minimal set of highest-scoring players,
# such that their scores sum up to same or higher score than this of all other players summed.
#
# Internally utilizes the "vsplot" function.
if (! export) {scaleplot(sets$ALL, battleinfo)
} else scaleplot(sets$ALL, battleinfo, tofile="Scaleplot.png")


## Sourceplot

# Compares IP, DirectPlay and Discord BIP usage in battle.
#
# Simple version utilizes the "vsplot" function internally.

if (ggplotgraphics)
{ # Pie chart (requires ggplot2)
  if (! export) {sourceplot_piechart(sets$ALL, battleinfo)
  } else sourceplot_piechart(sets$ALL, battleinfo, tofile="Sourceplot_pie.png")
  
} else
{ # Simple version (bar plot)
  if (! export) {sourceplot(sets$ALL, battleinfo)
  } else sourceplot(sets$ALL, battleinfo, tofile="Sourceplot.png")
}


## Hit density across a cycle
if (ggplotgraphics) # Requires ggplot2
{
  if (! export) {hitdensity(sets$ALL, battleinfo)
  } else hitdensity(sets$ALL, battleinfo, tofile="HitDensity.png")
}


## Timeline

# Visualizes performance of each team over time
if (! export) {timeline(sets, battleinfo)
} else timeline(sets, battleinfo, tofile="Timeline.png")


## Custom vsplot

# This is a manually composed plot comparing total scores of customized teams.
# For usage reference and examples, see inline documentation of vsplot() in plotting_functions.R

# if (! export) {vsfile = FALSE
# } else vsfile = "VSplot.png"
# vsplot(teams=list(deltans=rbind(sets$BB$per_player, sets$DC$per_player), protectors=sets$PS$per_player),
#        teamnames=c("Deltans", "Protectors"), tofile=vsfile)


export = FALSE # Make sure this remains false after execution, we don't want to generate files by accident
## Authors:
## -  GreenStickInsect aka Ogrodnik10


# A small troubleshooting note:
# Sometimes output devices randomly get messed up and you might see your plots not drawn at all,
# or stubbornly drawn into some file even when you don't tell them to.
# (I don't think this is an issue with my code, either something with RStudio or R itself)
# When this happens, run command 'dev.off()' a few times, until it says "null device".
# This should close all output devices and let a default one open for next plot.


##################################
### Manually changed variables ###
##################################
# This is the playground - the following variables are to be modified
# as fits, according to data and its location
# Just be warned that if some files with same names (as those requested to be created) already exist in
# the selected directory, they WILL be overwritten.
#
# NOTE: The following config options only let you draw the basic set of plots.
#       In order to customize, you'd need to scroll down to "Drawing the plots" section
#       and modify / add plot function calls. Alternatively, you can run plot functions manually.
#       This is a bit more advanced and might require digging through documentation in proper code files.


# File paths !!!
################

# Directory in which data files are and where plots will be generated
# Use absolute path or relative to repository root ("installation directory")
directory = "./battle_xxx"

filenames = c("ip_spend_battle_xxx.csv") # Main file with battle data
battleinfofile = "battleinfo.txt" # Supplementary general info file

# Technically, if you know what you're doing, you can set the directory to whatever
# you like and paths to input files located completely elsewhere,
# thus allowing you to have separate folders for input and output
# WARNING: if you do that, use absolute paths - this script may modify the workdir at many points

# It is assumed that the files are in output directory - if you specify full paths in
# 'filenames' and 'battleinfofile', remove/comment the following lines
filenames = file.path(directory, filenames)
battleinfofile = file.path(directory, battleinfofile)


# Whether to export to files
############################
#   FALSE - draw the plots within R
#   TRUE - instead, draw the plots as .png files
#
# Note: output file names for each plot are defined below.
# You can change them if you wish.
export = FALSE

# Set to FALSE to disable ggplot-drawn plots.
# If TRUE, requires package 'ggplot2' and won't apply without it.
ENABLE_GGPLOT_PLOTS = TRUE

# Set to TRUE to also draw animated plots. Requires library 'gganimate'.
# Implies ENABLE_GGPLOT_PLOTS, since animated plots are drawn using ggplot2.
# Note that generation of animated plots may be slow and heavy on the device.
ENABLE_ANIMATED_PLOTS = FALSE


# File names
############
# Well, you can also just run the plot generation functions manually, this is just a config
# for the predefined set of plots generated in the "Drawing the plots" section below

file_suffix = ".png" # Appended at the end of file name, except for animated plots which use '.gif'

prefix_player_plots = "Plot_players_" # Name for player plots is constructed like: prefix + faction name + suffix
name_team_plot = "Teamplot" # Total scores of each faction
name_contribution_plot = "Contribplot" # Player's support to factions
name_opposition_plot = "Oppositionplot" # Player's opposition to factions
name_bonuses_plot = "Bonusplot" # Average IP bonus per player
name_scaleplot = "Scaleplot" # Top scorers vs everyone else
name_sourceplot_classic = "Sourceplot" # Usage of playing methods
name_sourceplot_pie = "Sourceplot_pie" # Same as above but as piechart
name_hit_density_plot = "HitDensity" # Density of hits across a cycle
name_hit_density_animated_plot = "HitDensity_animation" # Density of hits on each cycle
name_timeline = "Timeline" # Timeline of faction activity
name_cr_plot = "CRPlot" # CR vs other teams
name_nexscale = "PlayersOnANexScale" # Copies of Nex vs other players. Only drawn if Nex participated



#########################
### Prepare Resources ###
#########################
# Only touch the following if you REALLY know what you're doing.

# This imports setup functions and creates the execution environment 'bplt' - your interface to
# all plotting and data manipulation functions, as well as constants created via "config.R".
# Functions and objects may be accessed as: bplt$some_object

# Direct modification and creation of objects within the environment is possible but not recommended.
# An advanced user wishing to modify the app is recommended to instead edit one of the
# relevant source files.
# Which files are imported into the environment is documented in the construction function
# 'eval_dependencies()' in "plots_cli.R"


# Set work dir to current script location.
# Assumption is that all scripts remain within the file structure of the original repository
# Careful - this line might not work if ran otherwise than from within RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# default setup functions #
source("plots_cli.R")


# For execution in RStudio, some extra steps need to be taken, so this simple function automates them
make_battle_plots = function(sourcedir)
{
  bplt = eval_dependencies(sourcedir, ggplot=ENABLE_GGPLOT_PLOTS, animated=ENABLE_ANIMATED_PLOTS)
  
  # This is IMPORTANT if you run through RStudio - if this env is missing in the 'bplt' environment's
  # search path, RStudio might trigger critical errors in some situations
  # (by "critical" I mean "you just fried your R session" kind of errors)
  if ("tools:rstudio" %in% search())
  {
    old_parent = parent.env(bplt)
    
    tmp = as.environment("tools:rstudio")
    rstudiotools = list2env(as.list(tmp, all.names = TRUE), parent=old_parent)
    
    parent.env(bplt) = rstudiotools
  }
  
  return(bplt)
}

bplt = make_battle_plots('.')

# Little helper function for constructing file paths for plots
mkpth = function(dir, filepart, ...)
{
  file.path(dir, paste0(filepart, ...))
}


#########################
### Generate datasets ###
#########################
# You probably don't want to touch this section, unless you know what you're doing.

# Advanced users may want to generate custom datasets - while you can do this from scratch,
# consider making use of/modifying the data manipulation functions in "data_functions.R".
# For simple changes it may be sufficient to modify the 'generate_datasets()' pipeline
# in "plots_cli.R"


## Try to obtain battle info
# Data from older battles does not have this, so code needs to be fault-proof
if (file.access(battleinfofile)[1] == 0)
{
  battleinfo = bplt$read_battle_data(battleinfofile) # see file "data_functions.R" for details
} else
{
  battleinfo = list()
}

## Create a list of datasets
# Each dataset in output list has a name, which is either a 2-letter faction code or 'ALL'.
# E.g.:
# sets$MT
#
# For details on how a dataset is structured, see 'prepare_someset()' in "data_functions.R"
sets = generate_datasets(bplt, filenames)


#########################
### Drawing the plots ###
#########################
# Draws the standard set of plots
# This can be edited to customize, though you might want to first have a look at
# inline plot documentation in plotting_functions.R and plotting_ggplot.R


## Player plots

# This includes "All Players" plot, as well as "Per faction" plots
# Basically just plots total scores of every player in a dataset
for (s in sets)
{
  if (s$faction == "CR") next # Makes no sense for CR - as a singular force, it is better seen on Team plot
  text.cex = 1
  if (s$faction == "All") text.cex = 0.8
  
  tofile = FALSE
  if (export) tofile = mkpth(directory, prefix_player_plots, s$faction, file_suffix)
  bplt$player_plot(s, battleinfo, text.cex=text.cex, cex.names=0.8, tofile=tofile)
}
rm(s, text.cex) # cleanup


## Team plot 

# Compare summary scores of each team.
#
# Internally utilizes the "vsplot" function.
if (!export) {bplt$teamplot(sets, battleinfo)
} else bplt$teamplot(sets, battleinfo, tofile=mkpth(directory, name_team_plot, file_suffix))


## Contribution plot

# Which teams did every player help
if (! export) {bplt$contribution_plot(sets$ALL, battleinfo)
} else bplt$contribution_plot(sets$ALL, battleinfo,
                              tofile=mkpth(directory, name_contribution_plot, file_suffix))

# Which teams did every player oppose (fight against)
if (! export) {bplt$contribution_plot(sets$ALL, battleinfo, inverted=TRUE)
} else bplt$contribution_plot(sets$ALL, battleinfo,
                         tofile=mkpth(directory, name_opposition_plot, file_suffix), inverted=TRUE)


## Bonuses plot

# How high bonuses each player utilized.
# Be warned: this just divides "ipApplied" column by sum of
# "Used" columns (in fact, datasets already have a "sumUsed" column defined)
# It WILL stop being accurate if formula for ipApplied ever changes to include something besides the bonus
if (! export) {bplt$bonusplot(sets$ALL, battleinfo)
} else bplt$bonusplot(sets$ALL, battleinfo, tofile=mkpth(directory, name_bonuses_plot, file_suffix))


## Scaleplot

# Divides players into 2 teams, where 1st is the minimal set of highest-scoring players,
# such that their scores sum up to same or higher score than this of all other players summed.
#
# Internally utilizes the "vsplot" function.
if (! export) {bplt$scaleplot(sets$ALL, battleinfo)
} else bplt$scaleplot(sets$ALL, battleinfo, tofile=mkpth(directory, name_scaleplot, file_suffix))


## Sourceplot

# Compares IP, DirectPlay and Discord BIP usage in battle.
#
# Simple version utilizes the "vsplot" function internally.

if (bplt$ggplotgraphics)
{ # Pie chart (requires ggplot2)
  if (! export) {bplt$sourceplot_piechart(sets$ALL, battleinfo)
  } else bplt$sourceplot_piechart(sets$ALL, battleinfo,
                                  tofile=mkpth(directory, name_sourceplot_pie, file_suffix))
  
} else
{ # Simple version (bar plot)
  if (! export) {bplt$sourceplot(sets$ALL, battleinfo)
  } else bplt$sourceplot(sets$ALL, battleinfo,
                         tofile=mkpth(directory, name_sourceplot_classic, file_suffix))
}


## Hit density across a cycle
if (bplt$ggplotgraphics) # Requires ggplot2
{
  if (bplt$ridgeplot)
  {
    if (! export) {bplt$hitdensity_ridge(sets$ALL, battleinfo)
    } else  bplt$hitdensity_ridge(sets$ALL, battleinfo,
                                  tofile=mkpth(directory, name_hit_density_plot, file_suffix))
  } else
  {
    if (! export) {bplt$hitdensity(sets$ALL, battleinfo)
    } else bplt$hitdensity(sets$ALL, battleinfo,
                           tofile=mkpth(directory, name_hit_density_plot, file_suffix))  
  }
}


## Hit density on each cycle (animated)
if (bplt$animatedgraphics)
{
  if (! export) {bplt$hitdensity_anim(sets$ALL, battleinfo)
  } else bplt$hitdensity_anim(sets$ALL, battleinfo,
                              tofile=mkpth(directory, name_hit_density_animated_plot, ".gif"))
}


## Timeline
# Visualizes performance of each team over time
if (! export) {bplt$timeline(sets, battleinfo)
} else bplt$timeline(sets, battleinfo, tofile=mkpth(directory, name_timeline, file_suffix))


## CR plot (if this is CR battle)
# Compares CR score to total score of all other players
if ("CR" %in% sets$ALL$raw$faction)
{
  if (! export) {bplt$nexplot(sets$ALL, "cr", labels=c("The Evil Force", "Stubborn Prey"))
  } else bplt$nexplot(sets$ALL, "cr", labels=c("The Evil Force", "Stubborn Prey"),
                      tofile=mkpth(directory, name_cr_plot, file_suffix))
}


## Nexscale
# Like a scale, but we're using Nexes instead of weights???
if ("Nex" %in% sets$ALL$per_player$name)
{
  if (! export) {bplt$nexscale(sets$ALL, battleinfo=battleinfo)
  } else bplt$nexscale(sets$ALL, battleinfo=battleinfo,
                       tofile=mkpth(directory, name_nexscale, file_suffix))
}


# Nexplot
# if ("Nex" %in% sets$ALL$per_player$name)
# {
#   if (! export) {bplt$nexplot(sets$ALL, exclude=c("cr"))
#   } else bplt$nexplot(sets$ALL, exclude=c("cr"), tofile=mkpth(directory, "Nexplot.png"))
# }


## Custom vsplot

# This is a manually composed plot comparing total scores of customized teams.
# For usage reference and examples, see inline documentation of vsplot() in plotting_functions.R

# if (! export) {vsfile = FALSE
# } else vsfile = mkpth(directory, "VSplot.png")
# bplt$vsplot(teams=list(deltans=rbind(sets$BB$per_player, sets$DC$per_player), protectors=sets$PS$per_player),
#             teamnames=c("Deltans", "Protectors"), tofile=vsfile)


export = FALSE # Make sure this remains false after execution, we don't want to generate files by accident
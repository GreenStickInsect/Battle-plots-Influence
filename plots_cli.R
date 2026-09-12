ARGUMENTS = list(c("file", NA_character_), c("directory", "."))
FLAGS = list(`--directory`="directory", `--dir`="directory", `-d`="directory")


args = commandArgs(TRUE)
argn = length(args)

add_package = function(envmt, pkg)
{
  ns = loadNamespace(pkg)
  
  for (name in getNamespaceExports(ns))
  {
    if (exists(name, envir=envmt, inherits=FALSE))
    {
      rm(list=name, envir=envmt)
    }
    
    makeActiveBinding(name, 
                      local({
                        n = name
                        function() getExportedValue(ns, n)
                      }),
                      envmt)
  }
  
  invisible(envmt)
}

# Create the execution environment of the app.
# As of last update to this documentation, the following files are added (source()-d)
# into the environment: "config.R", "data_functions.R", "plotting_functions.R", "plotting_ggplot.R"
#
# Arguments:
#   sourcedir - The root directory of repository (or the installation directory).
#   Env - (optional) Environment to use as execution environment. Note that its search path (parent)
#           will be modified. If not specified, a new environment (child to baseenv()) will be created.
#   bonus_packages - (optional) vector of names of packages that are to be made available to the
#                     environment. Note that these will be imported privately, as only accessible to
#                     functions within the environment and not from the outside.
#   ggplot - (optional) Whether 'ggplot2' is to be imported (privately). Should the
#             package be unavailable, a warning will be shown.
#             Sets the logical value 'ggplotgraphics' within the environment, noting whether the
#             library had been successfully imported. If possible, also imports 'ggridges' and sets
#             the value 'ridgeplot' to TRUE.
#   animated - (optional) Whether the package 'gganimate' should be imported. Should the package
#               be unavailable, a warning will be shown. Implies argument 'ggplot'.
#               Sets the logical value 'animatedgraphics' within the environment, noting whether the
#               library had been successfully imported.
#
# returns: An execution environment for the Battle Plots Influence app.
eval_dependencies = function(sourcedir, Env=NULL, bonus_packages=c(), ggplot=FALSE, animated=FALSE)
{
  # Create the destination environment if not supplied by user
  if (is.null(Env))
  {
    Env = new.env(parent=baseenv())
  }
  
  # Insert new environment "Deps" into Env's search path
  # (Deps is where libraries required by this app's functions, such as ggplot2, will go)
  old_parent = parent.env(Env)
  Deps = new.env(parent=old_parent)
  parent.env(Env) = Deps
  
  # Load some R's built-in packages
  # These should always be available in an R installation
  add_package(Deps, "tools")
  add_package(Deps, "utils")
  add_package(Deps, "stats")
  add_package(Deps, "methods")
  add_package(Deps, "grDevices")
  add_package(Deps, "graphics")
  
  # Add the additional requested packages
  for (pkg in bonus_packages)
  {
    if (! requireNamespace(pkg, quietly=T))
    {
      stop(paste0("Requested package \"", pkg, "\" is not available! Please make sure that this package is installed."))
    }
  }
  
  # Load the base R functions into Env
  source(file.path(sourcedir, "config.R"), local=Env)
  source(file.path(sourcedir, "data_functions.R"), local=Env)
  source(file.path(sourcedir, "plotting_functions.R"), local=Env)
  source(file.path(sourcedir, "plotting_ggplot.R"), local=Env)
  
  # Animated plots imply ggplot
  if (animated) ggplot = TRUE
  
  
  # Test whether ggplot2 and ggridges are to (and can) be loaded into Env
  Env$ggplotgraphics = FALSE
  Env$ridgeplot = FALSE
  if (ggplot)
  {
    if (requireNamespace("ggplot2", quietly=T))
    {
      add_package(Deps, "ggplot2")
      
      Env$ggplotgraphics = TRUE
      
      if (requireNamespace("ggridges", quietly=T))
      {
        add_package(Deps, "ggridges")
        
        Env$ridgeplot = TRUE
      }
    } else
    {
      cat("WARNING: 'ggplot' and/or 'animated' plots are set to TRUE, yet library 'ggplot2' is not installed.
        These plots cannot be drawn without it and will be skipped or replaced with simpler versions.\n")
      animated = FALSE
    }
  }
  
  # Test whether gganimate is to (and can) be loaded into Env
  Env$animatedgraphics = FALSE
  if (animated)
  {
    if (requireNamespace("gganimate", quietly=T))
    {
      add_package(Deps, "gganimate")
      
      Env$animatedgraphics = TRUE
    }
    else cat("WARNING: 'animated' plots are set to TRUE, yet library 'gganimate' is not installed.
           Animated plots cannot be drawn without it and will be skipped.")
  }
  
  return(Env)
}

# Create a list of basic per-faction datasets and one general 'ALL' dataset
# encompassing actions of all players. Each dataset is given name, which is either a 2-letter faction
# code or 'ALL'.
# For dataset format, see prepare_someset() in "data_functions.R".

# Arguments:
#   battleplots_env - (environment) An execution environment of this app, as created with eval_dependencies()
#   filenames - (character) Vector of paths to files from which data should be sourced. All data is joined together.
#
# returns: (list) A list of datasets.
generate_datasets = function(battleplots_env, filenames)
{
  bplt = battleplots_env
  
  # Prepare one temporary dataset from every input file
  nonesets = list(null=list())
  for (i in seq(length(filenames)))
  {
    # A "pipeline" function internally running several data reading and preparation functions.
    # Details can be found in "data_functions.R"
    tmp = bplt$read_and_reformat_dataset(filenames[i])
    
    nonesets[[paste("None",i,sep="")]] = tmp
  }

  nonesets = nonesets[-1] # remove temporary set
  
  ## Join all temporary datasets into one big
  allset = list(raw=nonesets[[1]]$raw, faction="ALL", color="gray60")
  for (i in seq(length(nonesets)))
  {
    if (i == 1) next
    allset$raw = rbind(allset$raw, nonesets[[i]]$raw)
  }
  
  # Create a list of datasets, containing the "ALL" dataset created above
  sets = list(ALL=allset)
  rm(i, nonesets, allset) # free memory, we won't need those anymore
  
  # Generate the missing 'per_player' data.frame and add it to "ALL" dataset
  sets$ALL$per_player = bplt$prepare_per_player(sets$ALL$raw) # see data_functions.R for details
  
  # Generate separate dataset for every faction and add those to list of datasets
  # Note: those datasets are stored in the list under name corresponding to its team's (short) name
  # e.g. sets$PS is dataset documenting performance of Protectores Silva
  # Known faction names should be present in 'bplt$faction_colors' data.frame
  sets = append(sets, bplt$prepare_factionsets(sets$ALL))
  
  return(sets)
}

if ("--run" %in% args)
{
  argnum = 1
  while (argnum <= argn)
  {
    arg = args[argnum]
    if (arg %in% FLAGS)
    {
      argkey = FLAGS[arg]
      ARGUMENTS[[argkey]] = args[argnum+1]
    }
  }
  
  bplt = eval_dependencies()
}
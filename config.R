# Teams/factions config
# This is very important, if it does not make it into the execution environment, many things may crash.
# Same goes if some teams are missing/defined wrone
#######################
# Specifies RGB (in hex) colors of each team.
# Also serves as a declaration of known teams and their names.
faction_colors = data.frame(CI="#1aa44f", MT="#fc8c36", DC="#3f5fde",
                            PS="#beff64", BB="#ff496c", FG="#0099ff",
                            CO="#9c6aff", RG = "#00bfa5", CR="#b20000",
                            AN="#cabfb0",
                            All="#dddddd", NO_TEAM="#303030", XX="#303030" # Technical
                            )

# This is a list of functions from the app execution environment which need to be imported into user's
# environment for optimal experience. The importing of tools is done via 'eval_dependencies()' in
# "plots_cli.R" and may optionally be disabled. Only use where necessary, filling user's environment with
# random functions is not a good practice.
global_imports = c("print.support_table")
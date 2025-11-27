# Battle plots Influence
An R programming language application for visualization of battle results from Influence mobile game.

Dependencies:
- R
- RStudio (for automated plotting with plots_via_rstudio.R, you don't have to use it)
- [ggplot2](<https://ggplot2.tidyverse.org/>) (this is optional, but some advanced/prettier graphs will not be available)

To get the app running:
- Install R
- Install RStudio - for now the only way to run the app is through it (unless you want to run things manually). I might add a command-line interface in future.
- [optional] Install [ggplot2](https://ggplot2.tidyverse.org/) with `install.packages("ggplot2")`.
- Clone the repo locally - best done via command-line with git, but as simple as this repo is, you can also just click download on the files you need.
- Create a folder with battle data somewhere on your machine.
  - battle data includes a CSV with ip spend data and file containing copy of general battle data posted on Discord with every data set. You might need to create the latter manually.
  - There is an example in folder "battle_xxx".
  - If you create the folder within repo, it is recommended to use names such as "battle_???" since these are included in .gitingore
- Open the source file "plots_via_rstudio.R" in RStudio.
- Set the control variables as follows:
  - directory - path to the directory with battle data (the one you created a moment ago).
  - filenames - vector with names of files with battle data (you will probably only have one, "ip_spend_battle_???.csv".
  - battleinfofile - name of the file containing general battle data.
  - export - (TRUE or FALSE) whether plots should be exported as files (these appear in your data folder)

- Run all the code (shortcut in RStudio - ctrl + a (select all), ctrl + enter (run selected))


If you need help, best contact @Ogrodnik10 on [Influence Discord server](<https://discord.gg/6yEf3cBq98>).
There is even a dedicated post for this [here](<https://canary.discord.com/channels/562910943848169472/1418601973543862303>) (make sure you join the server before following this link, else it won't work).

Alternatively, you can also send an email to gstickinsect@gmail.com

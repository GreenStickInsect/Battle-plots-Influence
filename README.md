# Battle plots Influence
An R programming language application for visualization of battle results from Influence mobile game.

To get the app running:
- Install R
- Install RStudio - for now the only way to run the app is through it. I might add a command-line interface in future.
- Clone the repo locally - best done via command-line with git, but as simple as this repo is, you can also just download everything.
- Create a folder with battle data somewhere on your machine.
  - battle data includes a CSV with ip spend data and file containing copy of general battle data posted on Discord with every data set.
  - There is an example in folder "battle_xxx".
  - it is recommended to use names such as "battle_???" since these are included in .gitingore
- Open the source file "plots_via_rstudio.R" in RStudio.
- Set the control variables as follows:
  - directory - path to the directory with batttle data.
  - filenames - vector with names of files with battle data (you will probably only have one, "ip_spend_battle_xxx.csv".
  - battleinfofile - name of the file containing general battle data.
  - export - (TRUE or FALSE) whether plots should be exported as files (these appear in your data folder)

- Run all the code (shortcut in RStudio - ctrl + a (select all), ctrl + enter (run selected))


If you need help, best contact @Ogrodnik10 on Influence Discord server.
Alternatively, you can also send an email to gstickinsect@gmail.com

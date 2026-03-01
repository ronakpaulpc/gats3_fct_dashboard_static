DEFINITIONS
Household Response Rate (HRR) 
Target: Min 92%
Formula: (Num of Int with Code 200 / Num of Total Int) * 100

Individual Response Rate (IRR) 
Target: Min 98%
Formula: (Num of Int with Code 400 / Num of Total Int) * 100

Total Response Rate (TRR)
Target: Min 92%
Formula: (Num of Int with Code 200 & Code 400 / Num of Total Int) * 100


GIT COMMANDS TO INITIALIZE VERSION CONTROL IN THE DASHBOARD FOLDER
git init
git add .
git commit -m "Initial commit for GATS-3 Static Dashboard"
git branch -M main
git remote add origin https://github.com/ronakpaulpc/gats3_fct_dashboard_static.git
git push -u origin main


COMMAND TO RENDER QUARTO DASHBOARD VIA THE GITHUB PAGES FROM THE TERMINAL
Once the folder is connected to GitHub, Quarto can take over and do all the heavy lifting. 
It will automatically render your document, create a special gh-pages branch, and push the HTML to the web.
quarto publish gh-pages dash_statewise_static_v00.qmd


GIT COMMANDS TO UPDATE QUARTO DASHBOARD VIA THE GITHUB PAGES FROM THE TERMINAL
quarto publish gh-pages dash_statewise_static_v00.qmd

GIT COMMANDS TO PUSH CHANGES TO THE DASHBOARD TO GITHUB
git add .
git commit -m "Update Dashboard"
git push 





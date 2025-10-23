# Flipped
Flipped shiny game

repo:
https://github.com/FlorisRoelofs2/Flipped

To merge f/add-app to main:

# Make sure you're on main
git checkout main

# Merge the feature branch
git merge origin f/add-app

# Delete local branch
git branch origin -d f/add-app
git branch -d f/add-app

# Delete remote branch
git push origin -d f/add-app

# Git token secret
check bestand 'tokens.rtf' op prive laptop
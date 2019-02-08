#!/bin/sh

echo -e "\033[0;32mDeploying updates to GitHub...\033[0m"

# Build the project
hugo

# Commit changes
git add docs
msg="rebuilding site `date`"
if [ $# -eq 1 ]
  then msg="$1"
fi
git commit -m "$msg"

# Push source and build repos.
git push origin master
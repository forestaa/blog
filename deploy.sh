#!/bin/sh

echo -e "\033[0;32mDeploying updates to GitHub...\033[0m"

msg="rebuilding site `date`"
hugo && git add docs && git commit -m "$msg" && git push origin master

#!/bin/bash

# Prepare updated reports
git checkout develop
Rscript scripts/render-site.R 
git add docs/*
git commit -m 'Update html reports and dependencies'

# Move changes to gh-pages branch
git checkout gh-pages 
git checkout develop docs/*
mv docs/* .

# Clean up
git rm --cached docs/*
rmdir docs
git add *

# Commit
git commit -m 'Update html reports and dependencies'

# Return to develop
git checkout develop

# Publish
git push origin gh-pages


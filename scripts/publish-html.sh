#!/bin/bash

# Prepare updated reports
git checkout develop
Rscript scripts/render-site.R 
git add docs/*.html
git commit -m 'Update html reports'

# Move changes to gh-pages branch
git checkout gh-pages 
git checkout develop docs/*
mv docs/* .

# Clean up
git rm --cached docs/*
rmdir docs
git add *.html

# Commit
git commit -m 'Update html reports'

# Return to develop
git checkout develop

# Publish
git push origin gh-pages


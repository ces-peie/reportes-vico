#!/bin/bash

# Prepare updated reports
git checkout feature/publish-html-reports docs
Rscript scripts/render-site.R 
git add docs/*.html
git commit -m 'Update html reports'

# Move changes to gh-pages branch
git checkout gh-pages 
git checkout feature/publish-html-reports docs/*
mv docs/* .

# Clean up
rmdir docs
git reset HEAD
git add *.html

# Commit
git commit -m 'Update html reports'


#!/bin/bash

# Prepare updated reports
git checkout develop
Rscript scripts/render-site.R 
git add docs/*
git commit -m 'Update html reports and dependencies'

# Move changes to gh-pages branch
git checkout gh-pages 
git checkout develop docs/*
cp -r docs/* . && rm -R docs/* && rmdir docs

# Clean up
git rm --cached docs/*
git add lib/* *_files/* *.html

# Commit
git commit -m 'Update html reports and dependencies'

# Return to develop
git checkout develop

# Publish
git push origin gh-pages


#!/bin/bash

# Prepare updated reports
git checkout develop
rm -R docs/*
Rscript scripts/render-site.R
git add -f docs/.
git commit -m 'Update html reports and dependencies'

# Move changes to gh-pages branch
git checkout gh-pages
rm -r `ls | grep -v "data"`
git checkout develop docs/*
cp -r docs/. . && rm -R docs/* && rmdir docs

# Clean up
git rm --cached docs/*
git add -f lib/* *_files/* *.html
git reset -- .Rproj.user/*

# Commit
git commit -m 'Update html reports and dependencies'

# Return to develop
git checkout develop

# Publish
git push origin gh-pages
git push origin develop


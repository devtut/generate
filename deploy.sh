#!/usr/bin/env sh

# abort on errors
set -e

# build
yarn docs:build

# navigate into the build output directory
cd dist

echo 'User-agent: *
Disallow:
Sitemap: https://devtut.github.io/sitemap-index.xml
Host: https://devtut.github.io' > robots.txt

git init
git add -A
git commit -m 'deploy'

git push -f git@github.com:devtut/devtut.github.io.git master

cd -
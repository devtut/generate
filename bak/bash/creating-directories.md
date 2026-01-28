---
metaTitle: "Bash - Creating directories"
description: "Move all files not already in a directory into a self named directory"
---

# Creating directories




## Move all files not already in a directory into a self named directory


ll | grep ^- | awk -F"." '{print $2 "." $3}' | awk -F":" '{print $2}' | awk '{$1=""; print $0}' | cut -c2- | awk -F"." '{print "mkdir ""$1"";mv ""$1"."$2"" ""$1"""}' > tmp;source tmp


---
metaTitle: "iOS - Create a Custom framework in iOS"
description: "Create Framework in Swift"
---

# Create a Custom framework in iOS



## Create Framework in Swift


follow these step to creating Custom Framework in Swift-IOS:

1. Create a new project. In Xcode
1. Choose iOS/Framework & Library/Cocoa Touch Framework to create a new framework
1. click next and set the productName
1. click next and choose directory to create Project there
1. add code and resources to created project

Framework created successfully

to add created framework to another project, first you should create a workspace<br />
add "target project" and "framework project" to workspace, then :

1. go to the general tab of target project
1. drag the "*.framework" file in product folder of framework project to "Embedded Binaries" section
1. to use in any ViewController or class just import framework at each file


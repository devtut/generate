---
metaTitle: "iOS - GameCenter Leaderboards"
description: "GameCenter Leaderboards"
---

# GameCenter Leaderboards




## GameCenter Leaderboards


**Prerequisites:**

1. Apple Developers Account
1. Setup GameCenter Leaderboards with iTunesConnect

**Setting up GameCenter Leaderboards:**

1. Sign in to **iTunesConnect**
1. Go to **My Apps**. Create an app for your project then go to **Features**.
1. Click on **Game Center**
1. Click the plus sign next to Leaderboards.
1. Choose **Single Leaderboard** for Leaderboard types.
1. Create a **Leaderboard Reference Name** for your reference.
1. Create a **Leaderboard ID** for your app to refer to when reporting scores.
1. Set score format to **Integer**
1. Score Submission will be **Best Score**
1. Click **Add language** and fill the entries.

Copy your `LeaderboardID` that you made and lets head over to Xcode.

Working with Xcode

There are 4 functions that we will be working with.

<li>
Importing the framework and setting up the protocols
</li>
<li>
Checking if the user is signed in to GameCenter
</li>
<li>
Reporting the scores to GameCenter
</li>
<li>
Viewing leaderboards
</li>
<li>
<p>Import GameKit `import GameKit`
Protocols `GKGameCenterControllerDelegate`</p>
</li>
<li>
Now we want to check if the user is signed in to GameCenter
</li>

```swift
func authenticateLocalPlayer() {
        
        let localPlayer = GKLocalPlayer.localPlayer()
        localPlayer.authenticateHandler = { (viewController, error) -> Void in
            
            if viewController != nil {
                //If the user is not signed in to GameCenter, we make them sign in
                let vc:UIViewController = self.view!.window!.rootViewController!
                vc.presentViewController(viewController!, animated: true, completion: nil)
                
            } else {
                
                //Do something here if you want
            }
        }
    }

```


1. Now the user is using the app and suddenly the user has a new high score, we report the high score by calling the function below.

The function below hols 2 parameters.

`Identifier` which is defined as a string and used to enter your leaderboardID that you made in iTunesConnect.

`score` which is defined as an Int which will be the users score to submit to iTunesConnect

```swift
func saveHighScore(identifier:String, score:Int) {
        
        if GKLocalPlayer.localPlayer().authenticated {
            
            let scoreReporter = GKScore(leaderboardIdentifier: identifier)
            
            scoreReporter.value = Int64(score)
            
            let scoreArray:[GKScore] = [scoreReporter]
            
            GKScore.reportScores(scoreArray, withCompletionHandler: {
                error -> Void in
                
                if error != nil {
                    print("Error")
                } else {
                    
                    
                }
            })
        }
    }

```


1. Now if the user wants to view leaderboards, call  the function below

```swift
//This function will show GameCenter leaderboards and Achievements if you call this function.
    func showGameCenter() {
        
        let gameCenterViewController = GKGameCenterViewController()
        gameCenterViewController.gameCenterDelegate = self
        
        let vc:UIViewController = self.view!.window!.rootViewController!
        vc.presentViewController(gameCenterViewController, animated: true, completion:nil)
        
    }
    
    //This function closes gameCenter after showing.
    func gameCenterViewControllerDidFinish(gameCenterViewController: GKGameCenterViewController) {
        
        gameCenterViewController.dismissViewControllerAnimated(true, completion: nil)
        self.gameCenterAchievements.removeAll()
        
    }

```


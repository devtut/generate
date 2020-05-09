---
metaTitle: "iOS - Creating an App ID"
description: "Creating In-App Purchase Products, Creating a Sandbox User"
---

# Creating an App ID




## Creating In-App Purchase Products


<li>When offering IAP within an an app, you must first add an entry for each individual purchase within iTunes Connect. If you’ve ever listed an app for sale in the store, it’s a similar process and includes things like choosing a pricing tier for the purchase. When the user makes a purchase, the App Store handles the complex process of charging the user’s iTunes account.
There are a whole bunch of different types of IAP you can add:
<ul>
- **Consumable**: These can be bought more than once and can be used up. These are things such as extra lives, in-game currency, temporary power-ups, and the like.
- **Non-Consumable**: Something that you buy once, and expect to have permanently such as extra levels and unlockable content.
- **Non-Renewing Subscription**: Content that’s available for a fixed period of time.
- **Auto-Renewing Subscription**: A repeating subscription such as a monthly raywenderlich.com subscription.

You can only offer In-App Purchases for digital items, and not for physical goods or services. For more information about all of this, check out Apple’s full documentation on Creating In-App Purchase Products.
Now, while viewing your app’s entry in iTunes Connect, click on the Features tab and then select In-App Purchases. To add a new IAP product, click the + to the right of In-App Purchases.

[<img src="https://i.stack.imgur.com/BqRZS.png" alt="enter image description here" />](https://i.stack.imgur.com/BqRZS.png)

You will see the following dialog appear:

[<img src="https://i.stack.imgur.com/6TtcO.png" alt="enter image description here" />](https://i.stack.imgur.com/6TtcO.png)

When a user purchases a rage comic in your app, you’ll want them to always have access to it, so select Non-Consumable, and click Create.
Next, fill out the details for the IAP as follows:

- **Reference Name**: A nickname identifying the IAP within iTunes Connect. This name does not appear anywhere in the app. The title of the comic you’ll be unlocking with this purchase is **“Girlfriend of Drummer“**, so enter that here.
- **Product ID**: This is a unique string identifying the IAP. Usually it’s best to start with the Bundle ID and then append a unique name specific to this purchasable item. For this tutorial, make sure you append “GirlfriendOfDrummerRage“, as this will be used later within the app to look up the comic to unlock. So, for example: com.theNameYouPickedEarlier.Rage.GirlFriendOfDrummerRage.
- **Cleared for Sale**: Enables or disables the sale of the IAP. You want to enable it!
- **Price Tier**: The cost of the IAP. Choose Tier 1.

Now scroll down to the Localizations section and note that there is a default entry for English (U.S.). Enter “Girlfriend of Drummer” for both the Display Name and the Description. Click Save. Great! You’ve created your first IAP product.

[<img src="https://i.stack.imgur.com/OJv4L.png" alt="enter image description here" />](https://i.stack.imgur.com/OJv4L.png)

There’s one more step required before you can delve into some code. When testing in-app purchases in a development build of an app, Apple provides a test environment which allows you to ‘purchase’ your IAP products without creating financial transactions.



## Creating a Sandbox User


In iTunes Connect, click iTunes Connect in the top left corner of the window to get back to the main menu. Select Users and Roles, then click the Sandbox Testers tab. Click + next to the “Tester” title.

[<img src="https://i.stack.imgur.com/8fVvB.png" alt="enter image description here" />](https://i.stack.imgur.com/8fVvB.png)

Fill out the information and click Save when you’re done. You can make up a first and last name for your test user, but the email address chosen must be a real email address as a verification will be sent to the address by Apple. Once you receive that email, be sure to click the link in it to verify your address.
The email address you enter should also NOT already be associated with an Apple ID account. Hint: if you have a gmail account, you can simply use an address alias instead of having to create a brand new account


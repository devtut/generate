---
metaTitle: "Xamarin - Push Notifications"
description: "iOS Example"
---

# Push Notifications



## iOS Example


1. You will need a development device
1. Go to your Apple Developer Account and create a provisioning profile with Push Notifications enabled
1. You will need some sort of way to notify your phone (AWS, Azure..etc) **We will use AWS here**

```cs
public override bool FinishedLaunching(UIApplication app, NSDictionary options)
{
    global::Xamarin.Forms.Forms.Init();

   //after typical Xamarin.Forms Init Stuff

   //variable to set-up the style of notifications you want, iOS supports 3 types

   var pushSettings = UIUserNotificationSettings.GetSettingsForTypes(
              UIUserNotificationType.Alert |
              UIUserNotificationType.Badge |
              UIUserNotificationType.Sound,
              null );  

       
        //both of these methods are in iOS, we have to override them and set them up
        //to allow push notifications

        app.RegisterUserNotificationSettings(pushSettings);  //pass the supported push notifications settings to register app in settings page

     
}

public override async void RegisteredForRemoteNotifications(UIApplication application, NSData token)
    {
        AmazonSimpleNotificationServiceClient snsClient = new AmazonSimpleNotificationServiceClient("your AWS credentials here");

        // This contains the registered push notification token stored on the phone. 
        var deviceToken = token.Description.Replace("<", "").Replace(">", "").Replace(" ", ""); 

        if (!string.IsNullOrEmpty(deviceToken))
        {
            //register with SNS to create an endpoint ARN, this means AWS can message your phone
            var response = await snsClient.CreatePlatformEndpointAsync(
            new CreatePlatformEndpointRequest
            {
                Token = deviceToken,
                PlatformApplicationArn = "yourARNwouldgohere" /* insert your platform application ARN here */
            });

            var endpoint = response.EndpointArn;

            //AWS lets you create topics, so use subscribe your app to a topic, so you can easily send out one push notification to all of your users
            var subscribeResponse = await snsClient.SubscribeAsync(new SubscribeRequest
            {
                TopicArn = "YourTopicARN here",
                Endpoint = endpoint,
                Protocol = "application"

            });

        }

    }

```



#### Remarks


### AWS Simple Notification Service Lingo:

**Endpoint** - The endpoint can be a phone, email address or whatever, it's what AWS SNS can hit back with a notification

**Topic** - Essentially a group that contains all of your endpoints

**Subscribe** - You sign up your phone/client to receive notifcations

### Generic Push Notification Lingo:

**APNS** - Apple Push Notification Service. Apple is the only one who can send push notifications. This is why we provision our app with the proper certificate. We provide AWS SNS the certificate that Apple provides us to authorize SNS to send a notification to APNS on our behalf.

**GCM** - Google Cloud Messaging is very similar to APNS. Google is the only one who can directly send push notifications. So we first register our App in GCM and hand over our token to AWS SNS. SNS handles all the complex stuff dealing with GCM and sending over the data.


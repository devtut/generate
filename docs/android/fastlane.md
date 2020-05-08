---
metaTitle: "Android - fastlane"
description: "Fastfile lane to build and install all flavors for given build type to a device, Fastfile to build and upload multiple flavors to Beta by Crashlytics"
---

# fastlane




## Fastfile lane to build and install all flavors for given build type to a device


Add this lane to your **Fastfile** and run `fastlane installAll type:{BUILD_TYPE}` in command line. Replace `BUILD_TYPE` with the build type you want to build.

For example: `fastlane installAll type:Debug`

This command will build all flavors of given type and install it to your device. Currently, it doesn't work if you have more than one device attached. Make sure you have only one. In the future I'm planning to add option to select target device.

```java
lane :installAll do |options|

    gradle(task: "clean")

    gradle(task: "assemble",
       build_type: options[:type])

    lane_context[SharedValues::GRADLE_ALL_APK_OUTPUT_PATHS].each do | apk |

        puts "Uploading APK to Device: " + apk

        begin
            adb(
                command: "install -r #{apk}"
            )
        rescue => ex
            puts ex
        end
    end
end

```



## Fastfile to build and upload multiple flavors to Beta by Crashlytics


This is a sample **Fastfile** setup for a multi-flavor app. It gives you an option to build and deploy all flavors or a single flavor. After the deployment, it reports to **Slack** the status of the deployment, and sends a notification to testers in Beta by Crashlytics testers group.

To build and deploy all flavors use:

```java
fastlane android beta

```

To build a single APK and deploy use:

```java
fastlane android beta app:flavorName

```

Using a single Fastlane file, you can manage iOS, Android, and Mac apps. If you are using this file just for one app `platform` is not required.

**How It Works**

1. `android` argument tells fastlane that we will use `:android` platform.
1. Inside `:android` platform you can have multiple lanes. Currently, I have only `:beta` lane. The second argument from the command above specifies the lane we want to use.
1. `options[:app]`
1. There are two **Gradle** tasks. First, it runs `gradle clean`. If you provided a flavor with `app` key, fastfile runs `gradle assembleReleaseFlavor`. Otherwise, it runs `gradle assembleRelease` to build all build flavors.
1. If we are building for all flavors, an array of generated APK file names is stored inside `SharedValues::GRADLE_ALL_APK_OUTPUT_PATHS`. We use this to loop through generated files and deploy them to **Beta by Crashlytics**. `notifications` and `groups` fields are optional. They are used to notify testers registered for the app on **Beta by Crashlytics**.
1. If you are familiar with Crashlytics, you might know that to activate an app in the portal, you have to run it on a device and use it first. Otherwise, Crashlytics will assume the app inactive and throw an error. In this scenario, I capture it and report to **Slack** as a failure, so you will know which app is inactive.
1. If deployment is successful, **fastlane** will send a success message to **Slack**.
1. `#{/([^\/]*)$/.match(apk)}` this regex is used to get flavor name from APK path. You can remove it if it does not work for you.
1. `get_version_name` and `get_version_code` are two **Fastlane** plugins to retrieve app version name and code. You have to install these gems if you want to use, or you can remove them. Read more about Plugins here.
1. The `else` statement will be executed if you are building and deploying a single APK. We don't have to provide `apk_path` to Crashlytics since we have only one app.
1. `error do` block at the end is used to get notified if anything else goes wrong during execution.

**Note**

Don't forget to replace `SLACK_URL`, `API_TOKEN`, `GROUP_NAME` and `BUILD_SECRET` with your own credentials.

```java
fastlane_version "1.46.1"

default_platform :android

platform :android do

    before_all do
        ENV["SLACK_URL"] = "https://hooks.slack.com/servic...."
    end
    
    lane :beta do |options|
        # Clean and build the Release version of the app.
        # Usage `fastlane android beta app:flavorName`
    
        gradle(task: "clean")
    
        gradle(task: "assemble",
               build_type: "Release",
               flavor: options[:app])
    
        # If user calls `fastlane android beta` command, it will build all projects and push them to Crashlytics
        if options[:app].nil?
            lane_context[SharedValues::GRADLE_ALL_APK_OUTPUT_PATHS].each do | apk |
    
                puts "Uploading APK to Crashlytics: " + apk
    
                begin
                    crashlytics(
                      api_token: "[API_TOKEN]",
                      build_secret: "[BUILD_SECRET]",
                      groups: "[GROUP_NAME]",
                      apk_path: apk,
                      notifications: "true"
                    )
    
                    slack(
                      message: "Successfully deployed new build for #{/([^\/]*)$/.match(apk)} #{get_version_name} - #{get_version_code}",
                      success: true,
                      default_payloads: [:git_branch, :lane, :test_result]
                    )
                rescue => ex
                    # If the app is inactive in Crashlytics, deployment will fail. Handle it here and report to slack
                    slack(
                        message: "Error uploading => #{/([^\/]*)$/.match(apk)} #{get_version_name} - #{get_version_code}: #{ex}",
                        success: false,
                        default_payloads: [:git_branch, :lane, :test_result]
                    )
                end
            end
    
            after_all do |lane|
                # This block is called, only if the executed lane was successful
                slack(
                    message: "Operation completed for #{lane_context[SharedValues::GRADLE_ALL_APK_OUTPUT_PATHS].size} app(s) for #{get_version_name} - #{get_version_code}",
                    default_payloads: [:git_branch, :lane, :test_result],
                    success: true
                )
            end
        else
            # Single APK upload to Beta by Crashlytics
            crashlytics(
                api_token: "[API_TOKEN]",
                build_secret: "[BUILD_SECRET]",
                groups: "[GROUP_NAME]",
                notifications: "true"
            )
    
            after_all do |lane|
                # This block is called, only if the executed lane was successful
                slack(
                    message: "Successfully deployed new build for #{options[:app]} #{get_version_name} - #{get_version_code}",
                    default_payloads: [:git_branch, :lane, :test_result],
                    success: true
                )
            end
        end
    
        error do |lane, exception|
            slack(
                message: exception.message,
                success: false,
                default_payloads: [:git_branch, :lane, :test_result]
            )
        end
    end
end

```



#### Remarks


**fastlane** is a tool for iOS, Mac, and Android developers to automate tedious tasks like generating screenshots, dealing with provisioning profiles, and releasing your application.

**Docs:** [https://docs.fastlane.tools/](https://docs.fastlane.tools/)

**Source Code:** [https://github.com/fastlane/fastlane](https://github.com/fastlane/fastlane)


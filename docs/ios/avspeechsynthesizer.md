---
metaTitle: "iOS - AVSpeechSynthesizer"
description: "Creating a basic text to speech"
---

# AVSpeechSynthesizer




## Creating a basic text to speech


Use the `speakUtterance:` method of `AVSpeechSynthesizer` to convert text to speech. You need to pass an `AVSpeechUtterance` object to this method, which contains the text that you want to be spoken.

**Objective C**

```swift
AVSpeechSynthesizer *speaker = [[AVSpeechSynthesizer alloc] init];
AVSpeechUtterance *speech    = [AVSpeechUtterance speechUtteranceWithString:@"Hello World"];
[speaker speakUtterance:speech];

```

**Swift**

```swift
let speaker = AVSpeechSynthesizer()
let speech = AVSpeechUtterance(string: "Hello World")
speaker.speakUtterance(speech)

```



#### Syntax


- AVSpeechSynthesizer() // Creates a speech synthesiser
- speaker.speakUtterance(speech) // Converts the text to speech



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|speaker|AVSpeechSynthesizer object
|speech|AVSpeechUtterance object


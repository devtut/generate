---
metaTitle: "iOS - iOS TTS"
description: "Text To Speech"
---

# iOS TTS


Find how to produce synthesized speech from text on an iOS device



## Text To Speech


### **Objective C**

```swift
AVSpeechSynthesizer *synthesizer = [[AVSpeechSynthesizer alloc]init];
AVSpeechUtterance *utterance = [AVSpeechUtterance speechUtteranceWithString:@"Some text"];
[utterance setRate:0.2f];
[synthesizer speakUtterance:utterance];

```

### **Swift**

```swift
let synthesizer = AVSpeechSynthesizer()
let utterance = AVSpeechUtterance(string: "Some text")
utterance.rate = 0.2

```

You can also change the voice like this :

```swift
utterance.voice = AVSpeechSynthesisVoice(language: "fr-FR")

```

And then speek

- In Swift 2 : `synthesizer.speakUtterance(utterance)`
- In Swift 3 : `synthesizer.speak(utterance)`

**Don't forget to import AVFoundation**

### Helpful methods

You can Stop or Pause all speech using these two methods :

```swift
- (BOOL)pauseSpeakingAtBoundary:(AVSpeechBoundary)boundary;
- (BOOL)stopSpeakingAtBoundary:(AVSpeechBoundary)boundary;

```

The AVSpeechBoundary indicates if the speech should pause or stop immediately (`AVSpeechBoundaryImmediate`) or it should pause or stop after the word currently being spoken (`AVSpeechBoundaryWord`).


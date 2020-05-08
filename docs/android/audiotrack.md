---
metaTitle: "Android - AudioTrack"
description: "Generate tone of a specific frequency"
---

# AudioTrack




## Generate tone of a specific frequency


To play a sound of with a specific tone,we first have to create a sine wave sound.This is done in the following way.

```java
final int duration = 10; // duration of sound
final int sampleRate = 22050; // Hz (maximum frequency is 7902.13Hz (B8))
final int numSamples = duration * sampleRate;
final double samples[] = new double[numSamples];
final short buffer[] = new short[numSamples];
for (int i = 0; i < numSamples; ++i) 
{
 samples[i] = Math.sin(2 * Math.PI * i / (sampleRate / note[0])); // Sine wave
 buffer[i] = (short) (samples[i] * Short.MAX_VALUE);  // Higher amplitude increases volume
}

```

Now we have to configure AudioTrack to play in accordance with the generated buffer . It is done in the following manner

```java
AudioTrack audioTrack = new AudioTrack(AudioManager.STREAM_MUSIC,
                    sampleRate, AudioFormat.CHANNEL_OUT_MONO,
                    AudioFormat.ENCODING_PCM_16BIT, buffer.length,
                    AudioTrack.MODE_STATIC);

```

Write the generated buffer and play the track

```java
audioTrack.write(buffer, 0, buffer.length);
audioTrack.play();

```

Hope this helps :)


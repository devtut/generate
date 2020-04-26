# Audio




## Working with WAV files


### winsound

- Windows environment

```
import winsound
winsound.PlaySound(=path_to_wav_file.wav=, winsound.SND_FILENAME)

```

### wave

- Support mono/stereo
- Doesn't support compression/decompression

```
import wave
with wave.open(=path_to_wav_file.wav=, =rb=) as wav_file:    # Open WAV file in read-only mode.
    # Get basic information.
    n_channels = wav_file.getnchannels()      # Number of channels. (1=Mono, 2=Stereo).
    sample_width = wav_file.getsampwidth()    # Sample width in bytes.
    framerate = wav_file.getframerate()       # Frame rate.
    n_frames = wav_file.getnframes()          # Number of frames.
    comp_type = wav_file.getcomptype()        # Compression type (only supports =NONE=).
    comp_name = wav_file.getcompname()        # Compression name.

    # Read audio data.
    frames = wav_file.readframes(n_frames)    # Read n_frames new frames.
    assert len(frames) == sample_width * n_frames

# Duplicate to a new WAV file.
with wave.open(=path_to_new_wav_file.wav=, =wb=) as wav_file:    # Open WAV file in write-only mode.
    # Write audio data.
    params = (n_channels, sample_width, framerate, n_frames, comp_type, comp_name)
    wav_file.setparams(params)
    wav_file.writeframes(frames)

```



## Convert any soundfile with python and ffmpeg


```
from subprocess import check_call

ok = check_call(['ffmpeg','-i','input.mp3','output.wav'])
if ok:
    with open('output.wav', 'rb') as f:
        wav_file = f.read()

```

note:

- [http://superuser.com/questions/507386/why-would-i-choose-libav-over-ffmpeg-or-is-there-even-a-difference](http://superuser.com/questions/507386/why-would-i-choose-libav-over-ffmpeg-or-is-there-even-a-difference)
- [What are the differences and similarities between ffmpeg, libav, and avconv?](http://stackoverflow.com/questions/9477115/what-are-the-differences-and-similarities-between-ffmpeg-libav-and-avconv)



## Playing Windows' beeps


Windows provides an explicit interface through which the `winsound` module allows you to play raw beeps at a given frequency and duration.

```
import winsound
freq = 2500 # Set frequency To 2500 Hertz
dur = 1000 # Set duration To 1000 ms == 1 second
winsound.Beep(freq, dur)

```



## Audio With Pyglet


```
import pyglet
audio = pyglet.media.load(=audio.wav=)
audio.play()

```

For further information, see [pyglet](https://pyglet.readthedocs.io/en/pyglet-1.2-maintenance/programming_guide/media.html)


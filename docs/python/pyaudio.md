# pyaudio


PyAudio provides Python bindings for PortAudio, the cross-platform audio I/O library. With PyAudio, you can easily use Python to play and record audio on a variety of platforms. PyAudio is inspired by:

1.pyPortAudio/fastaudio: Python bindings for PortAudio v18 API.

2.tkSnack: cross-platform sound toolkit for Tcl/Tk and Python.



## Callback Mode Audio I/O


```
===PyAudio Example: Play a wave file (callback version).===

import pyaudio
import wave
import time
import sys

if len(sys.argv) < 2:
    print(=Plays a wave file.\n\nUsage: %s filename.wav= % sys.argv[0])
    sys.exit(-1)

wf = wave.open(sys.argv[1], 'rb')

# instantiate PyAudio (1)
p = pyaudio.PyAudio()

# define callback (2)
def callback(in_data, frame_count, time_info, status):
    data = wf.readframes(frame_count)
    return (data, pyaudio.paContinue)

# open stream using callback (3)
stream = p.open(format=p.get_format_from_width(wf.getsampwidth()),
                channels=wf.getnchannels(),
                rate=wf.getframerate(),
                output=True,
                stream_callback=callback)

# start the stream (4)
stream.start_stream()

# wait for stream to finish (5)
while stream.is_active():
    time.sleep(0.1)

# stop stream (6)
stream.stop_stream()
stream.close()
wf.close()

# close PyAudio (7)
p.terminate()

```

In callback mode, PyAudio will call a specified callback function (2) whenever it needs new audio data (to play) and/or when there is new (recorded) audio data available. Note that PyAudio calls the callback function in a separate thread. The function has the following signature `callback(<input_data>, <frame_count>, <time_info>, <status_flag>)` and must return a tuple containing `frame_count` frames of audio data and a flag signifying whether there are more frames to play/record.

Start processing the audio stream using **pyaudio.Stream.start_stream()** (4), which will call the callback function repeatedly until that function returns **pyaudio.paComplete**.

To keep the stream active, the main thread must not terminate, e.g., by sleeping (5).



## Blocking Mode Audio I/O


===PyAudio Example: Play a wave file.===

```
import pyaudio
import wave
import sys

CHUNK = 1024

if len(sys.argv) < 2:
    print(=Plays a wave file.\n\nUsage: %s filename.wav= % sys.argv[0])
    sys.exit(-1)

wf = wave.open(sys.argv[1], 'rb')

# instantiate PyAudio (1)
p = pyaudio.PyAudio()

# open stream (2)
stream = p.open(format=p.get_format_from_width(wf.getsampwidth()),
                channels=wf.getnchannels(),
                rate=wf.getframerate(),
                output=True)

# read data
data = wf.readframes(CHUNK)

# play stream (3)
while len(data) > 0:
    stream.write(data)
    data = wf.readframes(CHUNK)

# stop stream (4)
stream.stop_stream()
stream.close()

# close PyAudio (5)
p.terminate()

```

To use PyAudio, first instantiate PyAudio using **pyaudio.PyAudio()** (1), which sets up the portaudio system.

To record or play audio, open a stream on the desired device with the desired audio parameters using **pyaudio.PyAudio.open()** (2). This sets up a **pyaudio.Stream** to play or record audio.

Play audio by writing audio data to the stream using **pyaudio.Stream.write()**, or read audio data from the stream using **pyaudio.Stream.read()**. (3)

Note that in “**blocking mode**”, each **pyaudio.Stream.write()** or **pyaudio.Stream.read()** blocks until all the given/requested frames have been played/recorded. Alternatively, to generate audio data on the fly or immediately process recorded audio data, use the “callback mode”(**refer the example on call back mode**)

Use pyaudio.Stream.stop_stream() to pause playing/recording, and **pyaudio.Stream.close()** to terminate the stream. (4)

Finally, terminate the portaudio session using **pyaudio.PyAudio.terminate()** (5)



#### Remarks


**Note:** stream_callback is called in a separate thread (from the main thread). Exceptions that occur in the stream_callback will:<br>
**1**.print a traceback on standard error to aid debugging,<br>
**2**.queue the exception to be thrown (at some point) in the main thread, and<br>
**3**.return paAbort to PortAudio to stop the stream.<br>
**Note:** Do not call Stream.read() or Stream.write() if using non-blocking operation.<br>
See: PortAudio’s callback signature for additional details :<br> [http://portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a8a60fb2a5ec9cbade3f54a9c978e2710](http://portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a8a60fb2a5ec9cbade3f54a9c978e2710)


---
metaTitle: "Android - MediaPlayer"
description: "Basic creation and playing, Media Player with Buffer progress and play position, Asynchronous prepare, Getting system ringtones, Getting and setting system volume, Import  audio into androidstudio and play it"
---

# MediaPlayer




## Basic creation and playing


MediaPlayer class can be used to control playback of audio/video files and streams.

Creation of MediaPlayer object can be of three types:

<li>
Media from local resource

```java
MediaPlayer mediaPlayer = MediaPlayer.create(context, R.raw.resource);
mediaPlayer.start(); // no need to call prepare(); create() does that for you

```


</li>
<li>
From local URI (obtained from ContentResolver)

```java
Uri myUri = ....; // initialize Uri here
MediaPlayer mediaPlayer = new MediaPlayer();
mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
mediaPlayer.setDataSource(getApplicationContext(), myUri);
mediaPlayer.prepare();
mediaPlayer.start();

```


</li>
<li>
From external URL

```java
String url = "http://........"; // your URL here
MediaPlayer mediaPlayer = new MediaPlayer();
mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
mediaPlayer.setDataSource(url);
mediaPlayer.prepare(); // might take long! (for buffering, etc)
mediaPlayer.start();

```


</li>



## Media Player with Buffer progress and play position


```java
public class SoundActivity extends Activity  {
 
    private MediaPlayer mediaPlayer;
    ProgressBar progress_bar;
    
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_tool_sound);
        mediaPlayer = new MediaPlayer();
        mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
        progress_bar = (ProgressBar) findViewById(R.id.progress_bar);
   
        btn_play_stop.setEnabled(false);
        btn_play_stop.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if(mediaPlayer.isPlaying()) {
                    mediaPlayer.pause();
                    btn_play_stop.setImageResource(R.drawable.ic_pause_black_24dp);
                } else {
                    mediaPlayer.start();
                    btn_play_stop.setImageResource(R.drawable.ic_play_arrow_black_24px);
                }
            }
        });

        mediaPlayer.setDataSource(proxyUrl);
        mediaPlayer.setOnCompletionListener(new MediaPlayer.OnCompletionListener() {
            @Override
            public void onCompletion(MediaPlayer mp) {
                observer.stop();
                progress_bar.setProgress(mp.getCurrentPosition());
                // TODO Auto-generated method stub
                mediaPlayer.stop();
                mediaPlayer.reset();
            }
        });
        mediaPlayer.setOnBufferingUpdateListener(new MediaPlayer.OnBufferingUpdateListener() {
            @Override
            public void onBufferingUpdate(MediaPlayer mp, int percent) {
                progress_bar.setSecondaryProgress(percent);
            }
        });
        mediaPlayer.setOnPreparedListener(new MediaPlayer.OnPreparedListener() {
            @Override
            public void onPrepared(MediaPlayer mediaPlayer) {
                btn_play_stop.setEnabled(true);

            }
        });
        observer = new MediaObserver();
        mediaPlayer.prepare();
        mediaPlayer.start();
        new Thread(observer).start();
    }

   
    private MediaObserver observer = null;

    private class MediaObserver implements Runnable {
        private AtomicBoolean stop = new AtomicBoolean(false);

        public void stop() {
            stop.set(true);
        }

        @Override
        public void run() {
            while (!stop.get()) {
                progress_bar.setProgress((int)((double)mediaPlayer.getCurrentPosition() / (double)mediaPlayer.getDuration()*100));
                try {
                    Thread.sleep(200);
                } catch (Exception ex) {
                    Logger.log(ToolSoundActivity.this, ex);
                }

            }
        }
    }
 
    @Override
    protected void onDestroy() {
        super.onDestroy();
        mediaPlayer.stop();
    }
}

```



```java
<LinearLayout
    android:gravity="bottom"
    android:layout_gravity="bottom"
    android:orientation="vertical"
    android:layout_width="match_parent"
    android:layout_height="0dp"
    android:layout_weight="1"
    android:weightSum="1">

    <LinearLayout
        android:orientation="horizontal"
        android:layout_width="match_parent"
        android:layout_height="wrap_content">

        <ImageButton
            app:srcCompat="@drawable/ic_play_arrow_black_24px"
            android:layout_width="48dp"
            android:layout_height="48dp"
            android:id="@+id/btn_play_stop" />

        <ProgressBar
            android:padding="8dp"
            android:progress="0"
            android:id="@+id/progress_bar"
            style="@style/Widget.AppCompat.ProgressBar.Horizontal"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:layout_gravity="center" />

    </LinearLayout>

</LinearLayout>

```



## Asynchronous prepare


The `MediaPlayer$prepare()` is a blocking call and will freeze the UI till execution completes. To solve this problem, `MediaPlayer$prepareAsync()` can be used.

```java
mMediaPlayer = ... // Initialize it here
mMediaPlayer.setOnPreparedListener(new MediaPlayer.OnPreparedListener(){
    @Override
    public void onPrepared(MediaPlayer player) {
        // Called when the MediaPlayer is ready to play
        mMediaPlayer.start();
    }
}); // Set callback for when prepareAsync() finishes
mMediaPlayer.prepareAsync(); // Prepare asynchronously to not block the Main Thread

```

On synchronous operations, errors would normally be signaled with an exception or an error code, but whenever you use asynchronous resources, you should make sure your application is notified of errors appropriately. For MediaPlayer,

```java
mMediaPlayer.setOnErrorListener(new MediaPlayer.OnErrorListener(){
        @Override
        public boolean onError(MediaPlayer mp, int what, int extra) {
            // ... react appropriately ...
            // The MediaPlayer has moved to the Error state, must be reset!
            // Then return true if the error has been handled
        }
});

```



## Getting system ringtones


This example demonstrates how to fetch the URI's of system ringtones (`RingtoneManager.TYPE_RINGTONE`):

```java
private List<Uri> loadLocalRingtonesUris() {
        List<Uri> alarms = new ArrayList<>();
        try {
            RingtoneManager ringtoneMgr = new RingtoneManager(getActivity());
            ringtoneMgr.setType(RingtoneManager.TYPE_RINGTONE);

            Cursor alarmsCursor = ringtoneMgr.getCursor();
            int alarmsCount = alarmsCursor.getCount();
            if (alarmsCount == 0 && !alarmsCursor.moveToFirst()) {
                alarmsCursor.close();
                return null;
            }

            while (!alarmsCursor.isAfterLast() && alarmsCursor.moveToNext()) {
                int currentPosition = alarmsCursor.getPosition();
                alarms.add(ringtoneMgr.getRingtoneUri(currentPosition));
            }

        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return alarms;
    }

```

The list depends on the types of requested ringtones. The possibilities are:

- `RingtoneManager.TYPE_RINGTONE`
- `RingtoneManager.TYPE_NOTIFICATION`
- `RingtoneManager.TYPE_ALARM`
- `RingtoneManager.TYPE_ALL = TYPE_RINGTONE | TYPE_NOTIFICATION | TYPE_ALARM`<br>

<br>In order to get the Ringtones as `android.media.Ringtone` every `Uri` must be resolved by the `RingtoneManager`:

```java
android.media.Ringtone osRingtone = RingtoneManager.getRingtone(context, uri);

```

To play the sound, use the method:

```java
public void setDataSource(Context context, Uri uri)

```

from `android.media.MediaPlayer`. `MediaPlayer` must be initialised and prepared according to the [State diagram](https://developer.android.com/reference/android/media/MediaPlayer.html#StateDiagram)



## Getting and setting system volume


### Audio stream types

There are different profiles of ringtone streams. Each one of them has it's different volume.

Every example here is written for `AudioManager.STREAM_RING` stream type. However this is not the only one. The available stream types are:

- `STREAM_ALARM`
- `STREAM_DTMF`
- `STREAM_MUSIC`
- `STREAM_NOTIFICATION`
- `STREAM_RING`
- `STREAM_SYSTEM`
- `STREAM_VOICE_CALL`

### Setting volume

To get the volume of specific profile, call:

```java
AudioManager audio = (AudioManager) getActivity().getSystemService(Context.AUDIO_SERVICE);
int currentVolume = audioManager.getStreamVolume(AudioManager.STREAM_RING);

```

This value is very little useful, when the maximum value for the stream is not known:

```java
AudioManager audio = (AudioManager) getActivity().getSystemService(Context.AUDIO_SERVICE);
int streamMaxVolume = audioManager.getStreamMaxVolume(AudioManager.STREAM_RING);

```

The ratio of those two value will give a relative volume (0 < volume < 1):

```java
float volume = ((float) currentVolume) / streamMaxVolume

```

### Adjusting volume by one step

To make the volume for the stream higher by one step, call:

```java
AudioManager audio = (AudioManager) getActivity().getSystemService(Context.AUDIO_SERVICE);
audio.adjustStreamVolume(AudioManager.STREAM_RING, AudioManager.ADJUST_RAISE, 0);

```

To make the volume for the stream lower by one step, call:

```java
AudioManager audio = (AudioManager) getActivity().getSystemService(Context.AUDIO_SERVICE);
audio.adjustStreamVolume(AudioManager.STREAM_RING, AudioManager.ADJUST_LOWER, 0);

```

### Setting MediaPlayer to use specific stream type

There is a helper function from `MediaPlayer` class to do this.<br>Just call `void setAudioStreamType(int streamtype)`:

```java
MediaPlayer mMedia = new MediaPlayer();
mMedia.setAudioStreamType(AudioManager.STREAM_RING);

```



## Import  audio into androidstudio and play it


This is an example how to get the play an audio file which you already have on your pc/laptop .First create a new directory under res and name it as raw like this

[<img src="http://i.stack.imgur.com/PZVCc.png" alt="this" />](http://i.stack.imgur.com/PZVCc.png)

copy the audio which you want to play into this folder .It may be a .mp3 or .wav file.

Now for example on button click you want to play this sound ,here is how it is done

```java
public class MainActivity extends AppCompatActivity {
@Override
public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.aboutapp_activity);

    MediaPlayer song=MediaPlayer.create(this, R.raw.song);

    Button button=(Button)findViewById(R.id.button);
    button.setOnClickListener(new View.OnClickListener() {
        @Override
        public void onClick(View view) {
           song.start();
        }
    });
}
}

```

This will play the song only once when the button is clicked,if you want to replay the song on every button click write code like this

```

public class MainActivity extends AppCompatActivity {
 @Override
 public void onCreate(Bundle savedInstanceState) {
   super.onCreate(savedInstanceState);
   setContentView(R.layout.aboutapp_activity);

   MediaPlayer song=MediaPlayer.create(this, R.raw.song);

   Button button=(Button)findViewById(R.id.button);
   button.setOnClickListener(new View.OnClickListener() {
    @Override
    public void onClick(View view) {
        if (song.isPlaying()) {
            song.reset();
            song= MediaPlayer.create(getApplicationContext(), R.raw.song);
       }
         song.start();
     }
   });
 }
}

```



#### Syntax


- void    setAudioStreamType(int streamtype)
- void    setDataSource(Context context, Uri uri)
- void    prepare()
- void    prepareAsync()
- void    start()
- void    stop()



#### Remarks


The `MediaPlayer` usage is mainly based on the State diagram:

[<img src="https://i.stack.imgur.com/a5JqU.gif" alt="enter image description here" />](https://i.stack.imgur.com/a5JqU.gif)

That means that in order to play audio/video a defined sequence of action must occur is specific order. It also states what [actions can be made in which state](https://developer.android.com/reference/android/media/MediaPlayer.html#Valid_and_Invalid_States).

The MediaPlayer API lacks flexibility (adding custom decoder and rendering logic) and lacks sSupport for Dynamic Adaptive Streaming over HTTP (DASH) and SmoothStreaming. For these, look into [ExoPlayer](https://developer.android.com/guide/topics/media/exoplayer.html).


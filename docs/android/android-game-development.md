---
metaTitle: "Android - Android game development"
description: "Game using Canvas and SurfaceView"
---

# Android game development


A short introduction to creating a game on the Android platform using Java



## Game using Canvas and SurfaceView


This covers how you can create a basic 2D game using SurfaceView.

First, we need an activity:

```java
public class GameLauncher extends AppCompatActivity {

    private Game game;
    @Override
    public void onCreate(Bundle sis){
        super.onCreate(sis);
        game = new Game(GameLauncher.this);//Initialize the game instance
        setContentView(game);//setContentView to the game surfaceview
        //Custom XML files can also be used, and then retrieve the game instance using findViewById.
    }

}

```

The activity also has to be declared in the Android Manifest.

Now for the game itself. First, we start by implementing a game thread:

```java
public class Game extends SurfaceView implements SurfaceHolder.Callback, Runnable{

    /**
     * Holds the surface frame
     */
    private SurfaceHolder holder;

    /**
     * Draw thread
     */
    private Thread drawThread;

    /**
     * True when the surface is ready to draw
     */
    private boolean surfaceReady = false;


    /**
     * Drawing thread flag
     */

    private boolean drawingActive = false;

    /**
     * Time per frame for 60 FPS
     */
    private static final int MAX_FRAME_TIME = (int) (1000.0 / 60.0);

    private static final String LOGTAG = "surface";    

    /*
     * All the constructors are overridden to ensure functionality if one of the different constructors are used through an XML file or programmatically
     */
    public Game(Context context) {
        super(context);
        init();
    }
    public Game(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }
    public Game(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }
    @TargetApi(21)
    public Game(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init();
    }

    public void init(Context c) {
        this.c = c;
        
        SurfaceHolder holder = getHolder();
        holder.addCallback(this);
        setFocusable(true);
        //Initialize other stuff here later
    }

    public void render(Canvas c){
        //Game rendering here
    }

    public void tick(){
        //Game logic here
    }

    @Override
    public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
    {
        if (width == 0 || height == 0){
            return;
        }

        // resize your UI
    }

    @Override
    public void surfaceCreated(SurfaceHolder holder){
        this.holder = holder;

        if (drawThread != null){
            Log.d(LOGTAG, "draw thread still active..");
            drawingActive = false;
            try{
                drawThread.join();
            } catch (InterruptedException e){}
        }

        surfaceReady = true;
        startDrawThread();
        Log.d(LOGTAG, "Created");
    }

    @Override
    public void surfaceDestroyed(SurfaceHolder holder){
        // Surface is not used anymore - stop the drawing thread
        stopDrawThread();
        // and release the surface
        holder.getSurface().release();

        this.holder = null;
        surfaceReady = false;
        Log.d(LOGTAG, "Destroyed");
    }

    @Override
    public boolean onTouchEvent(MotionEvent event){
        // Handle touch events
        return true;
    }

    /**
     * Stops the drawing thread
     */
    public void stopDrawThread(){
        if (drawThread == null){
            Log.d(LOGTAG, "DrawThread is null");
            return;
        }
        drawingActive = false;
        while (true){
            try{
                Log.d(LOGTAG, "Request last frame");
                drawThread.join(5000);
                break;
            } catch (Exception e) {
                Log.e(LOGTAG, "Could not join with draw thread");
            }
        }
        drawThread = null;
    }

    /**
     * Creates a new draw thread and starts it.
     */
    public void startDrawThread(){
        if (surfaceReady && drawThread == null){
            drawThread = new Thread(this, "Draw thread");
            drawingActive = true;
            drawThread.start();
        }
    }

    @Override
    public void run() {
        Log.d(LOGTAG, "Draw thread started");
        long frameStartTime;
        long frameTime;

        /*
         * In order to work reliable on Nexus 7, we place ~500ms delay at the start of drawing thread
         * (AOSP - Issue 58385)
         */
        if (android.os.Build.BRAND.equalsIgnoreCase("google") && android.os.Build.MANUFACTURER.equalsIgnoreCase("asus") && android.os.Build.MODEL.equalsIgnoreCase("Nexus 7")) {
            Log.w(LOGTAG, "Sleep 500ms (Device: Asus Nexus 7)");
            try {
                Thread.sleep(500);
            } catch (InterruptedException ignored) {}
        }

        while (drawing) {
            if (sf == null) {
                return;
            }

            frameStartTime = System.nanoTime();
            Canvas canvas = sf.lockCanvas();
            if (canvas != null) {
                try {
                    synchronized (sf) {
                        tick();
                        render(canvas);
                    }
                } finally {

                    sf.unlockCanvasAndPost(canvas);
                }
            }

            // calculate the time required to draw the frame in ms
            frameTime = (System.nanoTime() - frameStartTime) / 1000000;

            if (frameTime < MAX_FRAME_TIME){
                try {
                    Thread.sleep(MAX_FRAME_TIME - frameTime);
                } catch (InterruptedException e) {
                    // ignore
                }
            }

        }
        Log.d(LOGTAG, "Draw thread finished");
    }
}

```

That is the basic part. Now you have the ability to draw onto the screen.

Now, let's start by adding to integers:

```java
public final int x = 100;//The reason for this being static will be shown when the game is runnable
public int y;
public int velY;

```

For this next part, you are going to need an image. It should be about 100x100 but it can be bigger or smaller. For learning, a Rect can also be used(but that requires change in code a little bit down)

Now, we declare a Bitmap:

```java
private Bitmap PLAYER_BMP = BitmapFactory.decodeResource(getResources(), R.drawable.my_player_drawable);

```

In render, we need to draw this bitmap.

```java
...
c.drawBitmap(PLAYER_BMP, x, y, null);
...

```

**BEFORE LAUNCHING** there are still some things to be done

We need a boolean first:

```java
boolean up = false;

```

in onTouchEvent, we add:

```java
if(ev.getAction() == MotionEvent.ACTION_DOWN){
    up = true;
}else if(ev.getAction() == MotionEvent.ACTION_UP){
    up = false;
}

```

And in tick we need this to move the player:

```java
if(up){
    velY -=1;
}
else{
    velY +=1;
}
if(velY >14)velY = 14;
if(velY <-14)velY = -14;
y += velY *2;

```

and now we need this in init:

```java
WindowManager wm = (WindowManager) c.getSystemService(Context.WINDOW_SERVICE);
Display display = wm.getDefaultDisplay();
Point size = new Point();
display.getSize(size);
WIDTH = size.x;
HEIGHT = size.y;
y = HEIGHT/ 2 - PLAYER_BMP.getHeight();

```

And we need these to variables:

```java
public static int WIDTH, HEIGHT;

```

At this point, the game is runnable. Meaning you can launch it and test it.

Now you should have a player image or rect going up and down the screen. The player can be created as a custom class if needed. Then all the player-related things can be moved into that class, and use an instance of that class to move, render and do other logic.

Now, as you probably saw under testing it flies off the screen. So we need to limit it.

First, we need to declare the Rect:

```java
private Rect screen;

```

In init, after initializing width and height, we create a new rect that is the screen.

```java
screen = new Rect(0,0,WIDTH,HEIGHT);

```

Now we need another rect in the form of a method:

```java
private Rect getPlayerBound(){
    return new Rect(x, y, x + PLAYER_BMP.getWidth(), y + PLAYER_BMP.getHeight();
}

```

and in tick:

```java
if(!getPlayerBound().intersects(screen){
    gameOver = true;
}

```

The implementation of gameOVer can also be used to show the start of a game.

Other aspects of a game worth noting:

Saving(currently missing in documentation)



#### Remarks


- The first example covers the basics: There are no objectives, but it shows you how you create a basic part of a 2D game using SurfaceView.
- Make sure to save any important data when you create a game; everything else will be lost


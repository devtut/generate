---
metaTitle: "Android - Getting started with OpenGL ES 2.0+"
description: "Setting up GLSurfaceView and OpenGL ES 2.0+, Compiling and Linking GLSL-ES Shaders from asset file"
---

# Getting started with OpenGL ES 2.0+


This topic is about setting up and using **OpenGL ES 2.0+** on Android. OpenGL ES is the standard for 2D and 3D accelerated graphics on embedded systems - including consoles, smartphones, appliances and vehicles.



## Setting up GLSurfaceView and OpenGL ES 2.0+


To use OpenGL ES in your application you must add this to the manifest:

```java
<uses-feature android:glEsVersion="0x00020000" android:required="true"/>

```

Create your extended GLSurfaceView:

```java
import static android.opengl.GLES20.*; // To use all OpenGL ES 2.0 methods and constants statically

public class MyGLSurfaceView extends GLSurfaceView {
    
    public MyGLSurfaceView(Context context, AttributeSet attrs) {
        super(context, attrs);

        setEGLContextClientVersion(2); // OpenGL ES version 2.0
        setRenderer(new MyRenderer());
        setRenderMode(GLSurfaceView.RENDERMODE_CONTINUOUSLY);
    }

    public final class MyRenderer implements GLSurfaceView.Renderer{
        public final void onSurfaceCreated(GL10 unused, EGLConfig config) {
            // Your OpenGL ES init methods
            glClearColor(1f, 0f, 0f, 1f);
        }
        public final void onSurfaceChanged(GL10 unused, int width, int height) {
            glViewport(0, 0, width, height);
        }

        public final void onDrawFrame(GL10 unused) {
            // Your OpenGL ES draw methods
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        }
    }
}

```

Add `MyGLSurfaceView` to your layout:

```java
<com.example.app.MyGLSurfaceView
    android:id="@+id/gles_renderer"
    android:layout_width="match_parent"
    android:layout_height="match_parent"/>

```

To use newer [version of OpenGL ES](http://stackoverflow.com/documentation/opengl-es/5831/getting-started-with-opengl-es#t=20161229142910274369&a=versions) just change the version number in your manifest, in the static import and change `setEGLContextClientVersion`.



## Compiling and Linking GLSL-ES Shaders from asset file


The [Assets](http://stackoverflow.com/a/18302624/6300197) folder is the most common place to store your GLSL-ES shader files. To use them in your OpenGL ES application you need to load them to a string in the first place. This functions creates a string from the asset file:

```java
private String loadStringFromAssetFile(Context myContext, String filePath){
    StringBuilder shaderSource = new StringBuilder();
    try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(myContext.getAssets().open(filePath)));
        String line;
        while((line = reader.readLine()) != null){
            shaderSource.append(line).append("\n");
        }
        reader.close();
        return shaderSource.toString();
    } catch (IOException e) {
        e.printStackTrace();
        Log.e(TAG, "Could not load shader file");
        return null;
    }
}

```

Now you need to create a function that compiles a shader stored in a sting:

```java
private int compileShader(int shader_type, String shaderString){
    
    // This compiles the shader from the string
    int shader = glCreateShader(shader_type);
    glShaderSource(shader, shaderString); 
    glCompileShader(shader);

    // This checks for for compilation errors
    int[] compiled = new int[1];
    glGetShaderiv(shader, GL_COMPILE_STATUS, compiled, 0);
    if (compiled[0] == 0) {
        String log = glGetShaderInfoLog(shader);

        Log.e(TAG, "Shader compilation error: ");
        Log.e(TAG, log);
    }
    return shader;
}

```

Now you can load, compile and link your shaders:

```java
// Load shaders from file
String vertexShaderString = loadStringFromAssetFile(context, "your_vertex_shader.glsl");
String fragmentShaderString = loadStringFromAssetFile(context, "your_fragment_shader.glsl");

// Compile shaders
int vertexShader = compileShader(GL_VERTEX_SHADER, vertexShaderString);
int fragmentShader = compileShader(GL_FRAGMENT_SHADER, fragmentShaderString);

// Link shaders and create shader program
int shaderProgram = glCreateProgram();
glAttachShader(shaderProgram , vertexShader);
glAttachShader(shaderProgram , fragmentShader);
glLinkProgram(shaderProgram);

// Check for linking errors:
int linkStatus[] = new int[1];
glGetProgramiv(shaderProgram, GL_LINK_STATUS, linkStatus, 0);
if (linkStatus[0] != GL_TRUE) {
    String log = glGetProgramInfoLog(shaderProgram);

    Log.e(TAG,"Could not link shader program: ");
    Log.e(TAG, log);
}

```

If there are no errors, your shader program is ready to use:

```java
glUseProgram(shaderProgram);

```


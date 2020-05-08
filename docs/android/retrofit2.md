---
metaTitle: "Android - Retrofit2"
description: "A Simple GET Request, Add logging to Retrofit2, Debugging with Stetho, A simple POST request with GSON, Upload multiple file using Retrofit as multipart, Download a file from Server using Retrofit2, Retrofit with OkHttp interceptor, Header and Body: an Authentication Example, Uploading a file via Multipart, Retrofit 2 Custom Xml Converter, Reading XML form URL with Retrofit 2"
---

# Retrofit2


The official Retrofit page describes itself as

****A type-safe REST client for Android and Java.****

Retrofit turns your REST API into a Java interface. It uses annotations to describe HTTP requests, URL parameter replacement and query parameter support is integrated by default. Additionally, it provides functionality for multipart request body and file uploads.



## A Simple GET Request


We are going to be showing how to make a `GET` request to an API that responds with a `JSON` object or a `JSON` array. The first thing we need to do is add the Retrofit and `GSON` Converter dependencies to our module's gradle file.

**Add the dependencies for retrofit library as described in the Remarks section.**

**Example of expected JSON object:**

```java
{
    "deviceId": "56V56C14SF5B4SF",
    "name": "Steven",
    "eventCount": 0
}

```

**Example of JSON array:**

```java
[
    {
        "deviceId": "56V56C14SF5B4SF",
        "name": "Steven",
        "eventCount": 0
    },
    {
        "deviceId": "35A80SF3QDV7M9F",
        "name": "John",
        "eventCount": 2
    }
]

```

**Example of corresponding model class:**

```java
public class Device
{
    @SerializedName("deviceId")
    public String id;

    @SerializedName("name")
    public String name;

    @SerializedName("eventCount")
    public int eventCount;
}

```

**The `@SerializedName` annotations** here are from the `GSON` library and allows us to `serialize` and `deserialize` this class to `JSON` using the serialized name as the keys. Now we can build the interface for the API that will actually fetch the data from the server.

```java
public interface DeviceAPI
{
    @GET("device/{deviceId}")
    Call<Device> getDevice (@Path("deviceId") String deviceID);

    @GET("devices")
    Call<List<Device>> getDevices();
}

```

**There's a lot going on here in a pretty compact space so let's break it down:**

<li>The `@GET` annotation comes from Retrofit and tells the library that
we're defining a GET request.</li>
<li>The path in the parentheses is the endpoint that our GET request
should hit (we'll set the base url a little later).</li>
<li>The curly-brackets allow us to replace parts of the path at run time
so we can pass arguments.</li>
<li>The function we're defining is called `getDevice` and takes the
device id we want as an argument.</li>
<li>The `@PATH` annotation tells Retrofit that this argument should
replace the "deviceId" placeholder in the path.</li>
- The function returns a `Call` object of type `Device`.

**Creating a wrapper class:**

Now we will make a little wrapper class for our API to keep the Retrofit initialization code wrapped up nicely.

```java
public class DeviceAPIHelper
{
    public final DeviceAPI api;

    private DeviceAPIHelper ()
    {

        Retrofit retrofit = new Retrofit.Builder()
                .baseUrl("http://example.com/")
                .addConverterFactory(GsonConverterFactory.create())
                .build();

        api = retrofit.create(DeviceAPI.class);
    }
}

```

This class creates a GSON instance to be able to parse the JSON response, creates a Retrofit instance with our base url and a GSONConverter and then creates an instance of our API.

**Calling the API:**

```java
// Getting a JSON object
Call<Device> callObject = api.getDevice(deviceID);
callObject.enqueue(new Callback<Response<Device>>()
{
    @Override
    public void onResponse (Call<Device> call, Response<Device> response)
    {
        if (response.isSuccessful()) 
        {
           Device device = response.body();
        }
    }

    @Override
    public void onFailure (Call<Device> call, Throwable t)
    {
        Log.e(TAG, t.getLocalizedMessage());
    }
});

// Getting a JSON array
Call<List<Device>> callArray = api.getDevices();
callArray.enqueue(new Callback<Response<List<Device>>()
{
    @Override
    public void onResponse (Call<List<Device>> call, Response<List<Device>> response)
    {
        if (response.isSuccessful()) 
        {
           List<Device> devices = response.body();
        }
    }

    @Override
    public void onFailure (Call<List<Device>> call, Throwable t)
    {
        Log.e(TAG, t.getLocalizedMessage());
    }
});

```

This uses our API interface to create a `Call<Device>` object and to create a `Call<List<Device>>` respectively. Calling `enqueue` tells Retrofit to make that call on a background thread and return the result to the callback that we're creating here.

**Note:** Parsing a JSON array of primitive objects (like **String, Integer, Boolean**, and **Double**) is similar to parsing a JSON array. However, you don't need your own model class. You can get the array of Strings for example by having the return type of the call as `Call<List<String>>`.



## Add logging to Retrofit2


Retrofit requests can be logged using an intercepter. There are several levels of detail available: NONE, BASIC, HEADERS, BODY. See [Github project here](https://github.com/square/okhttp/tree/master/okhttp-logging-interceptor).

1. Add dependency to build.gradle:

```java
compile 'com.squareup.okhttp3:logging-interceptor:3.8.1'

```


1. Add logging interceptor when creating Retrofit:

```

 HttpLoggingInterceptor loggingInterceptor = new HttpLoggingInterceptor();
  loggingInterceptor.setLevel(LoggingInterceptor.Level.BODY);
  OkHttpClient okHttpClient = new OkHttpClient().newBuilder()
          .addInterceptor(loggingInterceptor)
          .build();
  Retrofit retrofit = new Retrofit.Builder()
          .baseUrl("http://example.com/")
          .client(okHttpClient)
          .addConverterFactory(GsonConverterFactory.create(gson))
          .build();

```

Exposing the logs in the Terminal(Android Monitor) is something that should be avoided in the release version as it may lead to unwanted exposing of critical information such as Auth Tokens etc.

To avoid the logs being exposed in the run time, check the following condition

```java
if(BuildConfig.DEBUG){
     //your interfector code here   
    }

```

For example:

```java
HttpLoggingInterceptor loggingInterceptor = new HttpLoggingInterceptor();
if(BuildConfig.DEBUG){
     //print the logs in this case   
    loggingInterceptor.setLevel(LoggingInterceptor.Level.BODY);
}else{
    loggingInterceptor.setLevel(LoggingInterceptor.Level.NONE);
}

OkHttpClient okHttpClient = new OkHttpClient().newBuilder()
      .addInterceptor(loggingInterceptor)
      .build();

Retrofit retrofit = new Retrofit.Builder()
      .baseUrl("http://example.com/")
      .client(okHttpClient)
      .addConverterFactory(GsonConverterFactory.create(gson))
      .build();

```



## Debugging with Stetho


Add the following dependencies to your application.

```java
compile 'com.facebook.stetho:stetho:1.5.0' 
compile 'com.facebook.stetho:stetho-okhttp3:1.5.0' 

```

In your Application class' `onCreate` method, call the following.

```java
Stetho.initializeWithDefaults(this);

```

When creating your `Retrofit` instance, create a custom OkHttp instance.

```java
OkHttpClient.Builder clientBuilder = new OkHttpClient.Builder();
clientBuilder.addNetworkInterceptor(new StethoInterceptor());

```

Then set this custom OkHttp instance in the Retrofit instance.

```java
Retrofit retrofit = new Retrofit.Builder()
    // ...
    .client(clientBuilder.build())
    .build();

```

Now connect your phone to your computer, launch the app, and type `chrome://inspect` into your Chrome browser. Retrofit network calls should now show up for you to inspect.



## A simple POST request with GSON


Sample JSON:

```java
{ 
    "id": "12345",
    "type": "android"
}

```

Define your request:

```java
public class GetDeviceRequest {

    @SerializedName("deviceId")
    private String mDeviceId;

    public GetDeviceRequest(String deviceId) {
        this.mDeviceId = deviceId;
    }

    public String getDeviceId() {
        return mDeviceId;
    }

}

```

Define your service (endpoints to hit):

```java
public interface Service {

    @POST("device")
    Call<Device> getDevice(@Body GetDeviceRequest getDeviceRequest);

}

```

Define your singleton instance of the network client:

```java
public class RestClient {

    private static Service REST_CLIENT;

    static {
        setupRestClient();
    }

    private static void setupRestClient() {
        
        // Define gson 
        Gson gson = new Gson();

        // Define our client 
        Retrofit retrofit = new Retrofit.Builder()
                .baseUrl("http://example.com/")
                .addConverterFactory(GsonConverterFactory.create(gson))
                .build();

        REST_CLIENT = retrofit.create(Service.class);
    }

    public static Retrofit getRestClient() {
        return REST_CLIENT;
    }

} 

```

Define a simple model object for the device:

```java
public class Device {

    @SerializedName("id")
    private String mId;

    @SerializedName("type")
    private String mType;

    public String getId() {
        return mId;
    }

    public String getType() {
        return mType;
    }

}

```

Define controller to handle the requests for the device

```java
public class DeviceController {

    // Other initialization code here...

    public void getDeviceFromAPI() {

        // Define our request and enqueue 
        Call<Device> call = RestClient.getRestClient().getDevice(new GetDeviceRequest("12345"));

        // Go ahead and enqueue the request 
        call.enqueue(new Callback<Device>() {
            @Override
            public void onSuccess(Response<Device> deviceResponse) {
                // Take care of your device here 
                if (deviceResponse.isSuccess()) {
                    // Handle success
                    //delegate.passDeviceObject();
                }
            }

            @Override
            public void onFailure(Throwable t) {
                // Go ahead and handle the error here 
            } 

        });

```



## Upload multiple file using Retrofit as multipart


Once you have setup the Retrofit environment in your project, you can use the following example that demonstrates how to upload multiple files using Retrofit:

```java
private void mulipleFileUploadFile(Uri[] fileUri) {
    OkHttpClient okHttpClient = new OkHttpClient();
    OkHttpClient clientWith30sTimeout = okHttpClient.newBuilder()
            .readTimeout(30, TimeUnit.SECONDS)
            .build();

    Retrofit retrofit = new Retrofit.Builder()
            .baseUrl(API_URL_BASE)
            .addConverterFactory(new MultiPartConverter())
            .client(clientWith30sTimeout)
            .build();

    WebAPIService service = retrofit.create(WebAPIService.class); //here is the interface which you have created for the call service
    Map<String, okhttp3.RequestBody> maps = new HashMap<>();

    if (fileUri!=null && fileUri.length>0) {
        for (int i = 0; i < fileUri.length; i++) {
            String filePath = getRealPathFromUri(fileUri[i]);
            File file1 = new File(filePath);

            if (filePath != null && filePath.length() > 0) {
                if (file1.exists()) {
                    okhttp3.RequestBody requestFile = okhttp3.RequestBody.create(okhttp3.MediaType.parse("multipart/form-data"), file1);
                    String filename = "imagePath" + i; //key for upload file like : imagePath0
                    maps.put(filename + "\"; filename=\"" + file1.getName(), requestFile);
                }
            }
        }
    }

    String descriptionString = " string request";//
    //hear is the your json request
    Call<String> call = service.postFile(maps, descriptionString);
    call.enqueue(new Callback<String>() {
        @Override
        public void onResponse(Call<String> call,
                               Response<String> response) {
            Log.i(LOG_TAG, "success");
            Log.d("body==>", response.body().toString() + "");
            Log.d("isSuccessful==>", response.isSuccessful() + "");
            Log.d("message==>", response.message() + "");
            Log.d("raw==>", response.raw().toString() + "");
            Log.d("raw().networkResponse()", response.raw().networkResponse().toString() + "");
        }

        @Override
        public void onFailure(Call<String> call, Throwable t) {
            Log.e(LOG_TAG, t.getMessage());
        }
    });
}
    
public String getRealPathFromUri(final Uri uri) { // function for file path from uri,
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT && DocumentsContract.isDocumentUri(mContext, uri)) {
        // ExternalStorageProvider
        if (isExternalStorageDocument(uri)) {
            final String docId = DocumentsContract.getDocumentId(uri);
            final String[] split = docId.split(":");
            final String type = split[0];

            if ("primary".equalsIgnoreCase(type)) {
                return Environment.getExternalStorageDirectory() + "/" + split[1];
            }
        }
        // DownloadsProvider
        else if (isDownloadsDocument(uri)) {

            final String id = DocumentsContract.getDocumentId(uri);
            final Uri contentUri = ContentUris.withAppendedId(
                    Uri.parse("content://downloads/public_downloads"), Long.valueOf(id));

            return getDataColumn(mContext, contentUri, null, null);
        }
        // MediaProvider
        else if (isMediaDocument(uri)) {
            final String docId = DocumentsContract.getDocumentId(uri);
            final String[] split = docId.split(":");
            final String type = split[0];

            Uri contentUri = null;
            if ("image".equals(type)) {
                contentUri = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
            } else if ("video".equals(type)) {
                contentUri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
            } else if ("audio".equals(type)) {
                contentUri = MediaStore.Audio.Media.EXTERNAL_CONTENT_URI;
            }

            final String selection = "_id=?";
            final String[] selectionArgs = new String[]{
                    split[1]
            };

            return getDataColumn(mContext, contentUri, selection, selectionArgs);
        }
    }
    // MediaStore (and general)
    else if ("content".equalsIgnoreCase(uri.getScheme())) {

        // Return the remote address
        if (isGooglePhotosUri(uri))
            return uri.getLastPathSegment();

        return getDataColumn(mContext, uri, null, null);
    }
    // File
    else if ("file".equalsIgnoreCase(uri.getScheme())) {
        return uri.getPath();
    }

    return null;
}

```

Following is the interface

```java
public interface WebAPIService {
    @Multipart
    @POST("main.php")
    Call<String> postFile(@PartMap Map<String,RequestBody> Files, @Part("json") String description);
}

```



## Download a file from Server using Retrofit2


Interface declaration for downloading a file

```java
public interface ApiInterface {
    @GET("movie/now_playing")
    Call<MovieResponse> getNowPlayingMovies(@Query("api_key") String apiKey, @Query("page") int page);

    // option 1: a resource relative to your base URL
    @GET("resource/example.zip")
    Call<ResponseBody> downloadFileWithFixedUrl();

    // option 2: using a dynamic URL
    @GET
    Call<ResponseBody> downloadFileWithDynamicUrl(@Url String fileUrl);
}

```

The option 1 is used for downloading a file from Server which is having fixed URL. and option 2 is used to pass a dynamic value as full URL to request call. This can be helpful when downloading files, which are dependent of parameters like user or time.

Setup retrofit for making api calls

```java
public class ServiceGenerator {

    public static final String API_BASE_URL = "http://your.api-base.url/";

    private static OkHttpClient.Builder httpClient = new OkHttpClient.Builder();

    private static Retrofit.Builder builder =
            new Retrofit.Builder()
            .baseUrl(API_BASE_URL)
            .addConverterFactory(GsonConverterFactory.create());

    public static <S> S createService(Class<S> serviceClass){
        Retrofit retrofit = builder.client(httpClient.build()).build();
        return retrofit.create(serviceClass);
    }

}

```

Now, make implementation of api for downloading file from server

```java
private void downloadFile(){
        ApiInterface apiInterface = ServiceGenerator.createService(ApiInterface.class);

        Call<ResponseBody> call = apiInterface.downloadFileWithFixedUrl();

        call.enqueue(new Callback<ResponseBody>() {
            @Override
            public void onResponse(Call<ResponseBody> call, Response<ResponseBody> response) {
                if (response.isSuccessful()){
                    boolean writtenToDisk = writeResponseBodyToDisk(response.body());

                    Log.d("File download was a success? ", String.valueOf(writtenToDisk));
                }
            }

            @Override
            public void onFailure(Call<ResponseBody> call, Throwable t) {

            }
        });
    }

```

And after getting response in the callback, code some standard IO for saving file to disk. Here is the code:

```java
private boolean writeResponseBodyToDisk(ResponseBody body) {
        try {
            // todo change the file location/name according to your needs
            File futureStudioIconFile = new File(getExternalFilesDir(null) + File.separator + "Future Studio Icon.png");

            InputStream inputStream = null;
            OutputStream outputStream = null;

            try {
                byte[] fileReader = new byte[4096];

                long fileSize = body.contentLength();
                long fileSizeDownloaded = 0;

                inputStream = body.byteStream();
                outputStream = new FileOutputStream(futureStudioIconFile);

                while (true) {
                    int read = inputStream.read(fileReader);

                    if (read == -1) {
                        break;
                    }

                    outputStream.write(fileReader, 0, read);

                    fileSizeDownloaded += read;

                    Log.d("File Download: " , fileSizeDownloaded + " of " + fileSize);
                }

                outputStream.flush();

                return true;
            } catch (IOException e) {
                return false;
            } finally {
                if (inputStream != null) {
                    inputStream.close();
                }

                if (outputStream != null) {
                    outputStream.close();
                }
            }
        } catch (IOException e) {
            return false;
        }
    }

```

Note we have specified **ResponseBody** as return type, otherwise Retrofit will try to parse and convert it, which doesn't make sense when you are downloading file.

If you want more on Retrofit stuffs, got to this link as it is very useful.
[1]: [https://futurestud.io/blog/retrofit-getting-started-and-android-client](https://futurestud.io/blog/retrofit-getting-started-and-android-client)



## Retrofit with OkHttp interceptor


This example shows how to use a request interceptor with OkHttp. This has numerous use cases such as:

- Adding universal `header` to the request. E.g. authenticating a request
- Debugging networked applications
- Retrieving raw `response`
- Logging network transaction etc.
- Set custom user agent

```java
Retrofit.Builder builder = new Retrofit.Builder()
        .addCallAdapterFactory(RxJavaCallAdapterFactory.create())
        .addConverterFactory(GsonConverterFactory.create())
        .baseUrl("https://api.github.com/");

if (!TextUtils.isEmpty(githubToken)) {
    // `githubToken`: Access token for GitHub
    OkHttpClient client = new OkHttpClient.Builder().addInterceptor(new Interceptor() {
        @Override public Response intercept(Chain chain) throws IOException {
            Request request = chain.request();
            Request newReq = request.newBuilder()
                    .addHeader("Authorization", format("token %s", githubToken))
                    .build();
            return chain.proceed(newReq);
        }
    }).build();

    builder.client(client);
}

return builder.build().create(GithubApi.class);

```

See [OkHttp topic](http://stackoverflow.com/documentation/android/3625/okhttp) for more details.



## Header and Body: an Authentication Example


The `@Header` and `@Body` annotations can be placed into the method signatures and Retrofit will automatically create them based on your models.

```java
public interface MyService {
     @POST("authentication/user")
     Call<AuthenticationResponse> authenticateUser(@Body AuthenticationRequest request, @Header("Authorization") String basicToken);
}

```

AuthenticaionRequest is our model, a POJO, containing the information the server requires. For this example, our server wants the client key and secret.

```java
public class AuthenticationRequest {
     String clientKey;
     String clientSecret;
}

```

Notice that in `@Header("Authorization")` we are specifying we are populating the Authorization header. The other headers will be populated automatically since Retrofit can infer what they are based on the type of objects we are sending and expecting in return.

We create our Retrofit service somewhere. We make sure to use HTTPS.

```java
Retrofit retrofit = new Retrofit.Builder()
            .baseUrl("https:// some example site")
            .client(client)
            .build();
MyService myService = retrofit.create(MyService.class)

```

Then we can use our service.

```java
AuthenticationRequest request = new AuthenticationRequest();
request.setClientKey(getClientKey());
request.setClientSecret(getClientSecret());
String basicToken = "Basic " + token;
myService.authenticateUser(request, basicToken);

```



## Uploading a file via Multipart


Declare your interface with Retrofit2 annotations:

```java
public interface BackendApiClient {
    @Multipart
    @POST("/uploadFile")
    Call<RestApiDefaultResponse> uploadPhoto(@Part("file\"; filename=\"photo.jpg\" ") RequestBody photo);
}

```

Where `RestApiDefaultResponse` is a custom class containing the response.

Building the implementation of your API and enqueue the call:

```java
Retrofit retrofit = new Retrofit.Builder()
                                .addConverterFactory(GsonConverterFactory.create())
                                .baseUrl("http://<yourhost>/")
                                .client(okHttpClient)
                                .build();

BackendApiClient apiClient = retrofit.create(BackendApiClient.class);
RequestBody reqBody = RequestBody.create(MediaType.parse("image/jpeg"), photoFile);
Call<RestApiDefaultResponse> call = apiClient.uploadPhoto(reqBody);
call.enqueue(<your callback function>);

```



## Retrofit 2 Custom Xml Converter


Adding dependencies into the build.gradle file.

```java
dependencies {
    ....
    compile 'com.squareup.retrofit2:retrofit:2.1.0'
    compile ('com.thoughtworks.xstream:xstream:1.4.7') {
        exclude group: 'xmlpull', module: 'xmlpull'
    }
    ....
}

```

Then create Converter Factory

```java
public class XStreamXmlConverterFactory extends Converter.Factory {

    /** Create an instance using a default {@link com.thoughtworks.xstream.XStream} instance for conversion. */
    public static XStreamXmlConverterFactory create() {
        return create(new XStream());
    }

    /** Create an instance using {@code xStream} for conversion. */
    public static XStreamXmlConverterFactory create(XStream xStream) {
        return new XStreamXmlConverterFactory(xStream);
    }

    private final XStream xStream;

    private XStreamXmlConverterFactory(XStream xStream) {
        if (xStream == null) throw new NullPointerException("xStream == null");
        this.xStream = xStream;
    }

    @Override
    public Converter<ResponseBody, ?> responseBodyConverter(Type type, Annotation[] annotations, Retrofit retrofit) {

        if (!(type instanceof Class)) {
            return null;
        }

        Class<?> cls = (Class<?>) type;

        return new XStreamXmlResponseBodyConverter<>(cls, xStream);
    }

    @Override
    public Converter<?, RequestBody> requestBodyConverter(Type type,
          Annotation[] parameterAnnotations, Annotation[] methodAnnotations, Retrofit retrofit) {

        if (!(type instanceof Class)) {
            return null;
        }

        return new XStreamXmlRequestBodyConverter<>(xStream);
    }
}

```

create a class to handle the body request.

```java
final class XStreamXmlResponseBodyConverter <T> implements Converter<ResponseBody, T> {

    private final Class<T> cls;
    private final XStream xStream;

    XStreamXmlResponseBodyConverter(Class<T> cls, XStream xStream) {
        this.cls = cls;
        this.xStream = xStream;
    }

    @Override
    public T convert(ResponseBody value) throws IOException {

        try {

            this.xStream.processAnnotations(cls);
            Object object =  this.xStream.fromXML(value.byteStream());
            return (T) object;

        }finally {
            value.close();
        }
    }
}

```

create a class to handle the body response.

```java
final class XStreamXmlRequestBodyConverter<T> implements Converter<T, RequestBody> {

    private static final MediaType MEDIA_TYPE = MediaType.parse("application/xml; charset=UTF-8");
    private static final String CHARSET = "UTF-8";

    private final XStream xStream;

    XStreamXmlRequestBodyConverter(XStream xStream) {
        this.xStream = xStream;
    }

    @Override
    public RequestBody convert(T value) throws IOException {

        Buffer buffer = new Buffer();

        try {
            OutputStreamWriter osw = new OutputStreamWriter(buffer.outputStream(), CHARSET);
            xStream.toXML(value, osw);
            osw.flush();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        return RequestBody.create(MEDIA_TYPE, buffer.readByteString());
    }
}

```

So, this point we can send and receive any XML , We just need create XStream Annotations for the entities.

Then create a Retrofit instance:

```java
XStream xs = new XStream(new DomDriver());
xs.autodetectAnnotations(true);

Retrofit retrofit = new Retrofit.Builder()
    .baseUrl("http://example.com/")
    .addConverterFactory(XStreamXmlConverterFactory.create(xs))
    .client(client)
    .build();

```



## Reading XML form URL with Retrofit 2


We will use retrofit 2 and SimpleXmlConverter to get xml data from url and parse to Java class.

Add dependency to Gradle script:

```java
compile 'com.squareup.retrofit2:retrofit:2.1.0'
compile 'com.squareup.retrofit2:converter-simplexml:2.1.0'

```

Create interface

Also create xml class wrapper in our case Rss class

```

   public interface ApiDataInterface{
    
        // path to xml link on web site
    
        @GET (data/read.xml)
    
        Call<Rss> getData();

}

```

Xml read function

```java
private void readXmlFeed() {
        try {

            // base url - url of web site
            Retrofit retrofit = new Retrofit.Builder()
                    .baseUrl(http://www.google.com/)
                    .client(new OkHttpClient())
                    .addConverterFactory(SimpleXmlConverterFactory.create())
                    .build();

            ApiDataInterface apiService = retrofit.create(ApiDataInterface.class);

            Call<Rss> call = apiService.getData();
            call.enqueue(new Callback<Rss>() {

                @Override
                public void onResponse(Call<Rss> call, Response<Rss> response) {

                    Log.e("Response success", response.message());
                    
                }

                @Override
                public void onFailure(Call<Rss> call, Throwable t) {
                    Log.e("Response fail", t.getMessage());
                }
            });

        } catch (Exception e) {
            Log.e("Exception", e.getMessage());
        }


    }

```

This is example of Java class with SimpleXML annotations

More about annotations [SimpleXmlDocumentation](http://simple.sourceforge.net/download/stream/doc/tutorial/tutorial.php)

```java
@Root (name = "rss")

public class Rss
{


    public Rss() {

    }



    public Rss(String title, String description, String link, List<Item> item, String language) {

        this.title = title;
        this.description = description;
        this.link = link;
        this.item = item;
        this.language = language;

    }

    @Element (name = "title")
    private String title;

    @Element(name = "description")
    private String description;

    @Element(name = "link")
    private String link;

    @ElementList (entry="item", inline=true)
    private List<Item> item;

    @Element(name = "language")
    private String language;

```



#### Remarks


**Dependencies for the retrofit library:**

From the [official documentation](http://square.github.io/retrofit/):

**Gradle:**

```java
dependencies {
    ...
    compile 'com.squareup.retrofit2:converter-gson:2.3.0'    
    compile 'com.squareup.retrofit2:retrofit:2.3.0'
    ...
}

```

**Maven:**

```java
<dependency>
  <groupId>com.squareup.retrofit2</groupId>
  <artifactId>retrofit</artifactId>
  <version>2.3.0</version>
</dependency>

```


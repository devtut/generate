---
metaTitle: "Android - Retrofit2 with RxJava"
description: "Retrofit2 with RxJava, Retrofit with RxJava to fetch data asyncronously, Nested requests example: multiple requests, combine results"
---

# Retrofit2 with RxJava



## Retrofit2 with RxJava


First, add relevant dependencies into the build.gradle file.

```java
dependencies {
    ....
    compile 'com.squareup.retrofit2:retrofit:2.3.0'
    compile 'com.squareup.retrofit2:converter-gson:2.3.0'
    compile 'com.squareup.retrofit2:adapter-rxjava:2.3.0'
    ....
}

```

Then create the model you would like to receive:

```java
public class Server {
    public String name;
    public String url;
    public String apikey;
    public List<Site> siteList;
}

```

Create an interface containing methods used to exchange data with remote server:

```java
public interface ApiServerRequests {

    @GET("api/get-servers")
    public Observable<List<Server>> getServers();
}

```

Then create a `Retrofit` instance:

```java
public ApiRequests DeviceAPIHelper ()
{
    Gson gson = new GsonBuilder().create();

    Retrofit retrofit = new Retrofit.Builder()
            .baseUrl("http://example.com/")
            .addConverterFactory(GsonConverterFactory.create(gson))
            .addCallAdapterFactory(RxJavaCallAdapterFactory.create())
            .build();

    api = retrofit.create(ApiServerRequests.class);
    return api;
}

```

Then, anywhere from the code, call the method:

```java
apiRequests.getServers()
   .subscribeOn(Schedulers.io()) // the observable is emitted on io thread
   .observerOn(AndroidSchedulers.mainThread()) // Methods needed to handle request in background thread
   .subscribe(new Subscriber<List<Server>>() {
       @Override
       public void onCompleted() {
                    
       }

       @Override
       public void onError(Throwable e) {

       }

       @Override
       public void onNext(List<Server> servers) {
           //A list of servers is fetched successfully
       }
   });

```



## Retrofit with RxJava to fetch data asyncronously


From the [GitHub repo](https://github.com/ReactiveX/RxJava) of RxJava, **RxJava is a Java VM implementation of Reactive Extensions: a library for composing asynchronous and event-based programs by using observable sequences. It extends the observer pattern to support sequences of data/events and adds operators that allow you to compose sequences together declaratively while abstracting away concerns about things like low-level threading, synchronisation, thread-safety and concurrent data structures.**

[Retrofit](http://square.github.io/retrofit/) is a type-safe HTTP client for Android and Java, using this, developers can make all network stuff much more easier. As an example, we are going to download some JSON and show it in RecyclerView as a list.

**Getting started:**

Add RxJava, RxAndroid and Retrofit dependencies in your app level build.gradle file:
`compile "io.reactivex:rxjava:1.1.6"`<br />
`compile "io.reactivex:rxandroid:1.2.1"`<br />
`compile "com.squareup.retrofit2:adapter-rxjava:2.0.2"`<br />
`compile "com.google.code.gson:gson:2.6.2"`<br />
`compile "com.squareup.retrofit2:retrofit:2.0.2"`<br />
`compile "com.squareup.retrofit2:converter-gson:2.0.2"`

**Define ApiClient and ApiInterface to exchange data from server**

```java
public class ApiClient {

private static Retrofit retrofitInstance = null;
private static final String BASE_URL = "https://api.github.com/";

public static Retrofit getInstance() {
    if (retrofitInstance == null) {
        retrofitInstance = new Retrofit.Builder()
                .baseUrl(BASE_URL)
                .addCallAdapterFactory(RxJavaCallAdapterFactory.create())
                .addConverterFactory(GsonConverterFactory.create())
                .build();
    }
    return retrofitInstance;
}

public static <T> T createRetrofitService(final Class<T> clazz, final String endPoint) {
    final Retrofit restAdapter = new Retrofit.Builder()
            .baseUrl(endPoint)
            .build();

    return restAdapter.create(clazz);
}

public static String getBaseUrl() {
    return BASE_URL;
}}

```

public interface ApiInterface {

```java
@GET("repos/{org}/{repo}/issues")
Observable<List<Issue>> getIssues(@Path("org") String organisation,
                                  @Path("repo") String repositoryName,
                                  @Query("page") int pageNumber);}

```

Note the getRepos() is returning an Observable and not just a list of issues.

**Define the models**

An example for this is shown. You can use free services like [JsonSchema2Pojo](http://www.jsonschema2pojo.org/) or this.

```java
public class Comment {

@SerializedName("url")
@Expose
private String url;
@SerializedName("html_url")
@Expose
private String htmlUrl;

//Getters and Setters
}

```

**Create Retrofit instance**

`ApiInterface apiService = ApiClient.getInstance().create(ApiInterface.class);`

**Then, Use this instance to fetch data from server**

```java
Observable<List<Issue>> issueObservable = apiService.getIssues(org, repo,                 pageNumber);
    issueObservable.subscribeOn(Schedulers.newThread())
            .observeOn(AndroidSchedulers.mainThread())
            .map(issues -> issues)    //get issues and map to issues list
            .subscribe(new Subscriber<List<Issue>>() {
                @Override
                public void onCompleted() {
                    Log.i(TAG, "onCompleted: COMPLETED!");
                }

                @Override
                public void onError(Throwable e) {
                    Log.e(TAG, "onError: ", e);
                }

                @Override
                public void onNext(List<Issue> issues) {
                    recyclerView.setAdapter(new IssueAdapter(MainActivity.this, issues, apiService));
                }
            });

```

Now, you have successfully fetched data from a server using Retrofit and RxJava.



## Nested requests example: multiple requests, combine results


Suppose we have an API which allows us to get object metadata in single request (`getAllPets`), and other request which have full data of single resource (`getSinglePet`). How we can query all of them in a single chain?

```java
public class PetsFetcher {


static class PetRepository {
    List<Integer> ids;
}

static class Pet {
    int id;
    String name;
    int weight;
    int height;
}

interface PetApi {

    @GET("pets") Observable<PetRepository> getAllPets();

    @GET("pet/{id}") Observable<Pet> getSinglePet(@Path("id") int id);

}

PetApi petApi;

Disposable petsDisposable;

public void requestAllPets() {

    petApi.getAllPets()
            .doOnSubscribe(new Consumer<Disposable>() {
                @Override public void accept(Disposable disposable) throws Exception {
                    petsDisposable = disposable;
                }
            })
            .flatMap(new Function<PetRepository, ObservableSource<Integer>>() {
                @Override
                public ObservableSource<Integer> apply(PetRepository petRepository) throws Exception {
                    List<Integer> petIds = petRepository.ids;
                    return Observable.fromIterable(petIds);
                }
            })
            .flatMap(new Function<Integer, ObservableSource<Pet>>() {
                @Override public ObservableSource<Pet> apply(Integer id) throws Exception {
                    return petApi.getSinglePet(id);
                }
            })
            .toList()
            .toObservable()
            .subscribeOn(Schedulers.io())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe(new Consumer<List<Pet>>() {
                @Override public void accept(List<Pet> pets) throws Exception {
                    //use your pets here
                }
            }, new Consumer<Throwable>() {
                @Override public void accept(Throwable throwable) throws Exception {
                    //show user something goes wrong
                }
            });

}

void cancelRequests(){
    if (petsDisposable!=null){
        petsDisposable.dispose();
        petsDisposable = null;
    }
}

```

}


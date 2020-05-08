---
metaTitle: "Angular 2 - Angular RXJS Subjects and Observables with API requests"
description: "Wait for multiple requests, Basic request, Encapsulating API requests"
---

# Angular RXJS Subjects and Observables with API requests




## Wait for multiple requests


One common scenario is to wait for a number of requests to finish before continuing. This can be accomplished using the [`forkJoin` method](http://reactivex.io/rxjs/class/es6/Observable.js%7EObservable.html#static-method-forkJoin).

In the following example, `forkJoin` is used to call two methods that return `Observables`. The callback specified in the `.subscribe` method will be called when both Observables complete. The parameters supplied by `.subscribe` match the order given in the call to `.forkJoin`. In this case, first `posts` then `tags`.

```js
loadData() : void {
    Observable.forkJoin(
        this.blogApi.getPosts(),
        this.blogApi.getTags()
    ).subscribe((([posts, tags]: [Post[], Tag[]]) => {
        this.posts = posts;
        this.tags = tags;
    }));
}

```



## Basic request


The following example demonstrates a simple HTTP GET request. `http.get()` returns an `Observable` which has the method `subscribe`. This one appends the returned data to the `posts` array.

```js
var posts = []

getPosts(http: Http):void {
    this.http.get(`https://jsonplaceholder.typicode.com/posts`)
        .map(response => response.json())
        .subscribe(post => posts.push(post));
}

```



## Encapsulating API requests


It may be a good idea to encapsulate the HTTP handling logic in its own class. The following class exposes a method for getting Posts. It calls the `http.get()` method and calls [`.map`](http://reactivex.io/rxjs/class/es6/Observable.js%7EObservable.html#instance-method-map) on the returned `Observable` to convert the `Response` object to a `Post` object.

```js
import {Injectable} from "@angular/core";
import {Http, Response} from "@angular/http";

@Injectable()
export class BlogApi {

  constructor(private http: Http) {
  }

  getPost(id: number): Observable<Post> {
    return this.http.get(`https://jsonplaceholder.typicode.com/posts/${id}`)
      .map((response: Response) => {
        const srcData = response.json();
        return new Post(srcData)
      });
  }
}

```

The previous example uses a `Post` class to hold the returned data, which could look as follows:

```js
export class Post { 
  userId: number;
  id: number;
  title: string;
  body: string;

  constructor(src: any) {
      this.userId = src && src.userId;
      this.id = src && src.id;
      this.title = src && src.title;
      this.body = src && src.body;
  }
}

```

A component now can use the `BlogApi` class to easily retrieve `Post` data without concerning itself with the workings of the `Http` class.



#### Remarks


Making API requests with Angular 2 Http service and RxJS is very similar to working with promises in Angular 1.x.

Use the [Http](https://angular.io/docs/ts/latest/api/http/index/Http-class.html) class to make requests. The Http class exposes the methods for issuing HTTP requests `GET`, `POST`, `PUT`, `DELETE`, `PATCH`, `HEAD` requests via corresponding methods. It also exposes a generic `request` method for issuing any kind of HTTP request.

All methods of the `Http` class return an `Observable<Response>`, to which you can apply [RxJS operations](http://reactivex.io/rxjs/class/es6/Observable.js%7EObservable.html). You call the `.subscribe()` method and pass in a function to be called when data is returned in the Observable stream.

The Observable stream for a request contains only one value - the `Response`, and completes/settles when the HTTP request is completed succesfully, or errors/faults if an error is thrown.

Note, the observables returned by the `Http` module are **cold**, which means if you subscribe to the observable multiple times, the originating request will be **executed** once for each subscription. This can happen if you want to consume the result in multiple components of your application. For GET requests this might just cause some extra requests, but this can create unexpected results if subscribe more than once to PUT or POST requests.


---
metaTitle: "Android - Design Patterns"
description: "Observer pattern, Singleton Class Example"
---

# Design Patterns


Design patterns are formalized best practices that the programmer can use to solve common problems when designing an application or system.

Design patterns can speed up the development process by providing tested, proven development paradigms.

Reusing design patterns helps to prevent subtle issues that can cause major problems, and it also improves code readability for coders and architects who are familiar with the patterns.



## Observer pattern


The observer pattern is a common pattern, which is widely used in many contexts. A real example can be taken from YouTube: When you like a channel and want to get all news and watch new videos from this channel, you have to subscribe to that channel. Then, whenever this channel publishes any news, you (and all other subscribers) will receive a notification.

An observer will have two components. One is a broadcaster (channel) and the other is a receiver (you or any other subscriber). The broadcaster will handle all receiver instances that subscribed to it. When the broadcaster fires a new event, it will announce this to all receiver instances. When the receiver receives an event, it will have to react to that event, for example, by turning on YouTube and playing the new video.

### Implementing the observer pattern

<li>
The broadcaster has to provide methods that permit receivers to subscribe and unsubscribe to it. When the broadcaster fires an event, subscribers need to be notified that an event has occurred:

```java
class Channel{
    private List<Subscriber> subscribers;
    public void subscribe(Subscriber sub) {
        // Add new subscriber.
    }
    public void unsubscribe(Subscriber sub) {
        // Remove subscriber.
    }
    public void newEvent() {
        // Notification event for all subscribers.
    }
}

```


</li>
<li>
The receiver needs to implement a method that handles the event from the broadcaster:

```java
interface Subscriber {
    void doSubscribe(Channel channel);
    void doUnsubscribe(Channel channel);
    void handleEvent();  // Process the new event.
}

```


</li>



## Singleton Class Example


**Java Singleton Pattern**

To implement Singleton pattern, we have different approaches but all of them have following common concepts.

- Private constructor to restrict instantiation of the class from other classes.
- Private static variable of the same class that is the only instance of the class.
- Public static method that returns the instance of the class, this is the global access
- point for outer world to get the instance of the singleton class.

```java
/**
 * Singleton class.
 */
public final class Singleton {

  /**
   * Private constructor so nobody can instantiate the class.
   */
  private Singleton() {}

  /**
   * Static to class instance of the class.
   */
  private static final Singleton INSTANCE = new Singleton();

  /**
   * To be called by user to obtain instance of the class.
   *
   * @return instance of the singleton.
   */
  public static Singleton getInstance() {
    return INSTANCE;
  }
}

```


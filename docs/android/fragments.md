---
metaTitle: "Android - Fragments"
description: "Pass data from Activity to Fragment using Bundle, The newInstance() pattern, Navigation between fragments using backstack and static fabric pattern, Sending events back to an activity with callback interface, Animate the transition between fragments, Communication between Fragments"
---

# Fragments


Introduction about Fragments and their intercommunication mechanism



## Pass data from Activity to Fragment using Bundle


All fragments should have an empty constructor (i.e. a constructor method having no input arguments). Therefore, in order to pass your data to the Fragment being created, you should use the `setArguments()` method. This methods gets a bundle, which you store your data in, and stores the Bundle in the arguments. Subsequently, this Bundle can then be retrieved in `onCreate()` and `onCreateView()` call backs of the Fragment.

**Activity:**

```

Bundle bundle = new Bundle();
 String myMessage = "Stack Overflow is cool!";
 bundle.putString("message", myMessage );
 FragmentClass fragInfo = new FragmentClass();
 fragInfo.setArguments(bundle);
 transaction.replace(R.id.fragment_single, fragInfo);
 transaction.commit();

```

**Fragment:**

```

@Override
 public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
    String myValue = this.getArguments().getString("message");
    ...
 }

```



## The newInstance() pattern


Although it is possible to create a fragment constructor with parameters, Android internally calls the zero-argument constructor when recreating fragments (for example, if they are being restored after being killed for Android's own reasons). For this reason, it is not advisable to rely on a constructor that has parameters.

To ensure that your expected fragment arguments are always present you can use a static `newInstance()` method to create the fragment, and put whatever parameters you want in to a bundle that will be available when creating a new instance.

```java
import android.os.Bundle;
import android.support.v4.app.Fragment;

public class MyFragment extends Fragment
{
  // Our identifier for obtaining the name from arguments
  private static final String NAME_ARG = "name";

  private String mName;

  // Required
  public MyFragment(){}

  // The static constructor.  This is the only way that you should instantiate
  // the fragment yourself
  public static MyFragment newInstance(final String name) {
    final MyFragment myFragment = new MyFragment();
    // The 1 below is an optimization, being the number of arguments that will
    // be added to this bundle.  If you know the number of arguments you will add
    // to the bundle it stops additional allocations of the backing map.  If
    // unsure, you can construct Bundle without any arguments
    final Bundle args = new Bundle(1);

    // This stores the argument as an argument in the bundle.  Note that even if
    // the 'name' parameter is NULL then this will work, so you should consider
    // at this point if the parameter is mandatory and if so check for NULL and
    // throw an appropriate error if so
    args.putString(NAME_ARG, name);

    myFragment.setArguments(args);
    return myFragment;
  }

  @Override
  public void onCreate(final Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Bundle arguments = getArguments();
    if (arguments == null || !arguments.containsKey(NAME_ARG)) {
      // Set a default or error as you see fit
    } else {
      mName = arguments.getString(NAME_ARG);
    }
  }
}

```

Now, in the Activity:

```java
FragmentTransaction ft = getSupportFragmentManager().beginTransaction();
MyFragment mFragment = MyFragment.newInstance("my name");
ft.replace(R.id.placeholder, mFragment);
//R.id.placeholder is where we want to load our fragment
ft.commit();

```

This pattern is a best practice to ensure that all the needed arguments will be passed to fragments on creation.
Note that when the system destroys the fragment and re-creates it later, it will automatically restore its state - but you must provide it with an [`onSaveInstanceState(Bundle)`](https://developer.android.com/reference/android/app/Fragment.html#onSaveInstanceState(android.os.Bundle)) implementation.



## Navigation between fragments using backstack and static fabric pattern


First of all, we need to add our first `Fragment` at the beginning, we should do it in the `onCreate()` method of our Activity:

```java
if (null == savedInstanceState) {
    getSupportFragmentManager().beginTransaction()
      .addToBackStack("fragmentA")
      .replace(R.id.container, FragmentA.newInstance(), "fragmentA")
      .commit();
}

```

Next, we need to manage our backstack. The easiest way is using a function added in our activity that is used for all FragmentTransactions.

```java
public void replaceFragment(Fragment fragment, String tag) {
    //Get current fragment placed in container
    Fragment currentFragment = getSupportFragmentManager().findFragmentById(R.id.container);

    //Prevent adding same fragment on top
    if (currentFragment.getClass() == fragment.getClass()) {
        return;
    }

    //If fragment is already on stack, we can pop back stack to prevent stack infinite growth
    if (getSupportFragmentManager().findFragmentByTag(tag) != null) {
        getSupportFragmentManager().popBackStack(tag, FragmentManager.POP_BACK_STACK_INCLUSIVE);
    }

    //Otherwise, just replace fragment
    getSupportFragmentManager()
            .beginTransaction()
            .addToBackStack(tag)
            .replace(R.id.container, fragment, tag)
            .commit();
}

```

Finally, we should override `onBackPressed()` to exit the application when going back from the last Fragment available in the backstack.

```java
@Override
public void onBackPressed() {
    int fragmentsInStack = getSupportFragmentManager().getBackStackEntryCount();
    if (fragmentsInStack > 1) { // If we have more than one fragment, pop back stack
        getSupportFragmentManager().popBackStack();
    } else if (fragmentsInStack == 1) { // Finish activity, if only one fragment left, to prevent leaving empty screen
        finish();
    } else {
        super.onBackPressed();
    }
}

```

Execution in activity:

```java
replaceFragment(FragmentB.newInstance(), "fragmentB");

```

Execution outside activity (assuming `MainActivity` is our activity):

```java
((MainActivity) getActivity()).replaceFragment(FragmentB.newInstance(), "fragmentB");

```



## Sending events back to an activity with callback interface


If you need to send events from fragment to activity, one of the possible solutions is to define callback interface and require that the host activity implement it.

### Example

### Send callback to an activity, when fragment's button clicked

```java
public interface SampleCallback {
    void onButtonClicked();
}

```

Next step is to assign this callback in fragment:

```java
public final class SampleFragment extends Fragment {

    private SampleCallback callback;

    @Override
    public void onAttach(Context context) {
        super.onAttach(context);
        if (context instanceof SampleCallback) {
            callback = (SampleCallback) context;
        } else {
            throw new RuntimeException(context.toString()
                    + " must implement SampleCallback");
        }
    }

    @Override
    public void onDetach() {
        super.onDetach();
        callback = null;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        final View view = inflater.inflate(R.layout.sample, container, false);
        // Add button's click listener
        view.findViewById(R.id.actionButton).setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                callback.onButtonClicked(); // Invoke callback here
            }
        });
        return view;
    }
}

```

And finally, implement callback in activity:

```java
public final class SampleActivity extends Activity implements SampleCallback {

    // ... Skipped code with settings content view and presenting the fragment

    @Override
    public void onButtonClicked() {
        // Invoked when fragment's button has been clicked
    }
}

```



## Animate the transition between fragments


To animate the transition between fragments, or to animate the process of showing or hiding a fragment you use the `FragmentManager` to create a `FragmentTransaction`.

For a single `FragmentTransaction`, there are two different ways to perform animations: you can use a standard animation or you can supply your own custom animations.

Standard animations are specified by calling `FragmentTransaction.setTransition(int transit)`, and using one of the pre-defined constants available in the `FragmentTransaction` class. At the time of writing, these constants are:

```java
FragmentTransaction.TRANSIT_NONE
FragmentTransaction.TRANSIT_FRAGMENT_OPEN
FragmentTransaction.TRANSIT_FRAGMENT_CLOSE
FragmentTransaction.TRANSIT_FRAGMENT_FADE

```

The complete transaction might look something like this:

```java
getSupportFragmentManager()
    .beginTransaction()
    .setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE)
    .replace(R.id.contents, new MyFragment(), "MyFragmentTag")
    .commit();

```

Custom animations are specified by calling either `FragmentTransaction.setCustomAnimations(int enter, int exit)` or `FragmentTransaction.setCustomAnimations(int enter, int exit, int popEnter, int popExit)`.

The `enter` and `exit` animations will be played for `FragmentTransaction`s that do not involve popping fragments off of the back stack. The `popEnter` and `popExit` animations will be played when popping a fragment off of the back stack.

The following code shows how you would replace a fragment by sliding out one fragment and sliding the other one in it's place.

```java
getSupportFragmentManager()
    .beginTransaction()
    .setCustomAnimations(R.anim.slide_in_left, R.anim.slide_out_right)
    .replace(R.id.contents, new MyFragment(), "MyFragmentTag")
    .commit();

```

The XML animation definitions would use the `objectAnimator` tag. An example of **slide_in_left.xml** might look something like this:

```java
<?xml version="1.0" encoding="utf-8"?>
<set>
  <objectAnimator xmlns:android="http://schemas.android.com/apk/res/android"
    android:propertyName="x" 
    android:valueType="floatType"
    android:valueFrom="-1280"
    android:valueTo="0" 
    android:duration="500"/>
</set>

```



## Communication between Fragments


All communications between Fragments must go via an Activity. Fragments **CANNOT** communicate with each other without an Activity.

**Additional Resources**

- [How to implement OnFragmentInteractionListener](https://stackoverflow.com/questions/24777985/how-to-implement-onfragmentinteractionlistener)
- [Android | Communicating With Other Fragments](https://developer.android.com/training/basics/fragments/communicating.html)

In this sample, we have a `MainActivity` that hosts two fragments, `SenderFragment` and `ReceiverFragment`, for sending and receiving a `message` (a simple String in this case) respectively.

A Button in `SenderFragment` initiates the process of sending the message. A TextView in the `ReceiverFragment` is updated when the message is received by it.

Following is the snippet for the MainActivity with comments explaining the important lines of code:

```java
// Our MainActivity implements the interface defined by the SenderFragment. This enables
// communication from the fragment to the activity
public class MainActivity extends AppCompatActivity implements SenderFragment.SendMessageListener {

@Override
protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);
}

/**
 * This method is called when we click on the button in the SenderFragment
 * @param message The message sent by the SenderFragment
 */
@Override
public void onSendMessage(String message) {
    // Find our ReceiverFragment using the SupportFragmentManager and the fragment's id
    ReceiverFragment receiverFragment = (ReceiverFragment)
            getSupportFragmentManager().findFragmentById(R.id.fragment_receiver);

    // Make sure that such a fragment exists
    if (receiverFragment != null) {
        // Send this message to the ReceiverFragment by calling its public method
        receiverFragment.showMessage(message);
    }
}
}

```

The layout file for the `MainActivity` hosts two fragments inside a LinearLayout :

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:id="@+id/activity_main"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:orientation="vertical"
    android:paddingBottom="@dimen/activity_vertical_margin"
    android:paddingLeft="@dimen/activity_horizontal_margin"
    android:paddingRight="@dimen/activity_horizontal_margin"
    android:paddingTop="@dimen/activity_vertical_margin"
    tools:context="com.naru.fragmentcommunication.MainActivity">

<fragment
    android:id="@+id/fragment_sender"
    android:name="com.naru.fragmentcommunication.SenderFragment"
    android:layout_width="match_parent"
    android:layout_height="0dp"
    android:layout_weight="1"
    tools:layout="@layout/fragment_sender" />

<fragment
    android:id="@+id/fragment_receiver"
    android:name="com.naru.fragmentcommunication.ReceiverFragment"
    android:layout_width="match_parent"
    android:layout_height="0dp"
    android:layout_weight="1"
    tools:layout="@layout/fragment_receiver" />
</LinearLayout>

```

The `SenderFragment` exposes an interface `SendMessageListener` that helps the `MainActivity` know when Button in the `SenderFragment` was clicked.

Following is the code snippet for the `SenderFragment` explaining the important lines of code:

```java
public class SenderFragment extends Fragment {

private SendMessageListener commander;

/**
 * This interface is created to communicate between the activity and the fragment. Any activity
 * which implements this interface will be able to receive the message that is sent by this
 * fragment.
 */
public interface SendMessageListener {
    void onSendMessage(String message);
}

/**
 * API LEVEL >= 23
 * <p>
 * This method is called when the fragment is attached to the activity. This method here will
 * help us to initialize our reference variable, 'commander' , for our interface
 * 'SendMessageListener'
 *
 * @param context
 */
@Override
public void onAttach(Context context) {
    super.onAttach(context);
    // Try to cast the context to our interface SendMessageListener i.e. check whether the
    // activity implements the SendMessageListener. If not a ClassCastException is thrown.
    try {
        commander = (SendMessageListener) context;
    } catch (ClassCastException e) {
        throw new ClassCastException(context.toString()
                + "must implement the SendMessageListener interface");
    }
}

/**
 * API LEVEL < 23
 * <p>
 * This method is called when the fragment is attached to the activity. This method here will
 * help us to initialize our reference variable, 'commander' , for our interface
 * 'SendMessageListener'
 *
 * @param activity
 */
@Override
public void onAttach(Activity activity) {
    super.onAttach(activity);
    // Try to cast the context to our interface SendMessageListener i.e. check whether the
    // activity implements the SendMessageListener. If not a ClassCastException is thrown.
    try {
        commander = (SendMessageListener) activity;
    } catch (ClassCastException e) {
        throw new ClassCastException(activity.toString()
                + "must implement the SendMessageListener interface");
    }
}

@Nullable
@Override
public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container,
                         @Nullable Bundle savedInstanceState) {
    // Inflate view for the sender fragment.
    View view = inflater.inflate(R.layout.fragment_receiver, container, false);

    // Initialize button and a click listener on it
    Button send = (Button) view.findViewById(R.id.bSend);
    send.setOnClickListener(new View.OnClickListener() {
        @Override
        public void onClick(View v) {

            // Sanity check whether we were able to properly initialize our interface reference
            if (commander != null) {

                // Call our interface method. This enables us to call the implemented method
                // in the activity, from where we can send the message to the ReceiverFragment.
                commander.onSendMessage("HELLO FROM SENDER FRAGMENT!");
            }
        }
    });

    return view;
}
}

```

The layout file for the `SenderFragment`:

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:gravity="center"
    android:orientation="vertical">

<Button
    android:id="@+id/bSend"
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:text="SEND"
    android:layout_gravity="center_horizontal" />
</LinearLayout>

```

The `ReceiverFragment` is simple and exposes a simple public method to updates its TextView. When the `MainActivity` receives the message from the `SenderFragment` it calls this public method of the `ReceiverFragment`

Following is the code snippet for the `ReceiverFragment` with comments explaining the important lines of code :

```java
public class ReceiverFragment extends Fragment {
TextView tvMessage;

@Nullable
@Override
public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container,
                         @Nullable Bundle savedInstanceState) {
    // Inflate view for the sender fragment.
    View view = inflater.inflate(R.layout.fragment_receiver, container, false);

    // Initialize the TextView
    tvMessage = (TextView) view.findViewById(R.id.tvReceivedMessage);

    return view;
}


/**
 * Method that is called by the MainActivity when it receives a message from the SenderFragment.
 * This method helps update the text in the TextView to the message sent by the SenderFragment.
 * @param message Message sent by the SenderFragment via the MainActivity.
 */
public void showMessage(String message) {
    tvMessage.setText(message);
}
}

```

The layout file for the `ReceiverFragment` :

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:gravity="center"
    android:orientation="vertical">
<TextView
    android:id="@+id/tvReceivedMessage"
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:text="Waiting for message!" />
</LinearLayout>

```



#### Syntax


<li>
void onActivityCreated(Bundle savedInstanceState) // Called when the fragment's activity has been created and this fragment's view hierarchy instantiated.
</li>
<li>
void onActivityResult(int requestCode, int resultCode, Intent data) // Receive the result from a previous call to startActivityForResult(Intent, int).
</li>
<li>
void onAttach(Activity activity) // This method was deprecated in API level 23. Use onAttach(Context) instead.
</li>
<li>
void onAttach(Context context) // Called when a fragment is first attached to its context.
</li>
<li>
void onAttachFragment(Fragment childFragment) // Called when a fragment is attached as a child of this fragment.
</li>
<li>
void onConfigurationChanged(Configuration newConfig) // Called by the system when the device configuration changes while your component is running.
</li>
<li>
void onCreate(Bundle savedInstanceState) // Called to do initial creation of a fragment.
</li>
<li>
View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) // Called to have the fragment instantiate its user interface view.
</li>
<li>
void onDestroy() // Called when the fragment is no longer in use.
</li>
<li>
void onDestroyView() // Called when the view previously created by onCreateView(LayoutInflater, ViewGroup, Bundle) has been detached from the fragment.
</li>
<li>
void onDetach() // Called when the fragment is no longer attached to its activity.
</li>
<li>
void onInflate(Activity activity, AttributeSet attrs, Bundle savedInstanceState) // This method was deprecated in API level 23. Use onInflate(Context, AttributeSet, Bundle) instead.
</li>
<li>
void onInflate(Context context, AttributeSet attrs, Bundle savedInstanceState) // Called when a fragment is being created as part of a view layout inflation, typically from setting the content view of an activity.
</li>
<li>
void onPause() // Called when the Fragment is no longer resumed.
</li>
<li>
void onResume() // Called when the fragment is visible to the user and actively running.
</li>
<li>
void onSaveInstanceState(Bundle outState) // Called to ask the fragment to save its current dynamic state, so it can later be reconstructed in a new instance of its process is restarted.
</li>
<li>
void onStart() // Called when the Fragment is visible to the user.
</li>
<li>
void onStop() // Called when the Fragment is no longer started.
</li>
<li>
void onViewStateRestored(Bundle savedInstanceState) // Called when all saved state has been restored into the view hierarchy of the fragment.
</li>



#### Remarks


A Fragment represents a behavior or a portion of user interface in an Activity. You can combine multiple fragments in a single activity to build a multi-pane UI and reuse a fragment in multiple activities. You can think of a fragment as a modular section of an activity, which has its own lifecycle, receives its own input events, and which you can add or remove while the activity is running (sort of like a "sub activity" that you can reuse in different activities).

### Constructor


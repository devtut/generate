---
metaTitle: "Android - EditText"
description: "Working with EditTexts, Customizing the InputType, Hiding SoftKeyboard, Icon or button inside Custom Edit Text and its action and click listeners., `inputype` attribute"
---

# EditText




## Working with EditTexts


The EditText is the standard text entry widget in Android apps. If the user needs to enter text into an app, this is the primary way for them to do that.

**EditText**

There are many important properties that can be set to customize the behavior of an EditText. Several of these are listed below. Check out the official text fields guide for even more input field details.

**Usage**

An EditText is added to a layout with all default behaviors with the following XML:

```java
<EditText
    android:id="@+id/et_simple"
    android:layout_height="wrap_content"
    android:layout_width="match_parent">
</EditText>

```

**Note that an EditText is simply a thin extension of the TextView and inherits all of the same properties.**

**Retrieving the Value**

**Getting the value of the text entered into an EditText is as follows:**

```java
EditText simpleEditText = (EditText) findViewById(R.id.et_simple);
String strValue = simpleEditText.getText().toString();

```

Further Entry Customization

**We might want to limit the entry to a single-line of text (avoid newlines):**

```java
<EditText
  android:singleLine="true"
  android:lines="1"
/>

```

**You can limit the characters that can be entered into a field using the digits attribute:**

```java
<EditText
  android:inputType="number"
  android:digits="01"
/>

```

**This would restrict the digits entered to just "0" and "1". We might want to limit the total number of characters with:**

```java
<EditText
  android:maxLength="5"
/>

```

**Using these properties we can define the expected input behavior for text fields.**

**Adjusting Colors**

You can adjust the highlight background color of selected text within an EditText with the `android:textColorHighlight` property:

```java
<EditText
    android:textColorHighlight="#7cff88"
/>

```

Displaying Placeholder Hints

**You may want to set the hint for the EditText control to prompt a user for specific input with:**

```java
<EditText
    ...
    android:hint="@string/my_hint">
</EditText>

```

**Hints**

**Changing the bottom line color**

Assuming you are using the AppCompat library, you can override the styles colorControlNormal, colorControlActivated, and colorControlHighlight:

```java
<style name="Theme.App.Base" parent="Theme.AppCompat.Light.DarkActionBar">
    <item name="colorControlNormal">#d32f2f</item>
    <item name="colorControlActivated">#ff5722</item>
    <item name="colorControlHighlight">#f44336</item>
</style>

```

If you do not see these styles applied within a DialogFragment, there is a known bug when using the LayoutInflater passed into the onCreateView() method.

The issue has already been fixed in the AppCompat v23 library. See this guide about how to upgrade. Another temporary workaround is to use the Activity's layout inflater instead of the one passed into the onCreateView() method:

```java
public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = getActivity().getLayoutInflater().inflate(R.layout.dialog_fragment, container);
  }

```

**Listening for EditText Input**

Check out the basic event listeners cliffnotes for a look at how to listen for changes to an EditText and perform an action when those changes occur.

**Displaying Floating Label Feedback**

Traditionally, the EditText hides the hint message (explained above) after the user starts typing. In addition, any validation error messages had to be managed manually by the developer.

With the `TextInputLayout` you can setup a floating label to display hints and error messages. You can find more [details here](http://stackoverflow.com/documentation/android/5652/textinputlayout#t=201609101109491325049).



## Customizing the InputType


Text fields can have different input types, such as number, date, password, or email address. The type determines what kind of characters are allowed inside the field, and may prompt the virtual keyboard to optimize its layout for frequently used characters.

By default, any text contents within an `EditText` control is displayed as plain text. By setting the [`inputType`](https://developer.android.com/reference/android/widget/TextView.html#attr_android:inputType) attribute, we can facilitate input of different types of information, like phone numbers and passwords:

```java
<EditText
    ...
    android:inputType="phone">
</EditText>

```

Most common input types include:

|Type|Description
|---|---|---|---|---|---|---|---|---|---
|textUri|Text that will be used as a URI
|textEmailAddress|Text that will be used as an e-mail address
|textPersonName|Text that is the name of a person
|textPassword|Text that is a password that should be obscured
|number|A numeric only field
|phone|For entering a phone number
|date|For entering a date
|time|For entering a time
|textMultiLine|Allow multiple lines of text in the field

The `android:inputType` also allows you to specify certain keyboard behaviors, such as whether to capitalize all new words or use features like auto-complete and spelling suggestions.<br />
Here are some of the common input type values that define keyboard behaviors:

|Type|Description
|---|---|---|---|---|---|---|---|---|---
|textCapSentences|Normal text keyboard that capitalizes the first letter for each new sentence
|textCapWords|Normal text keyboard that capitalizes every word. Good for titles or person names
|textAutoCorrect|Normal text keyboard that corrects commonly misspelled words

You can set multiple `inputType` attributes if needed (separated by '|').<br />
Example:

```java
<EditText
    android:id="@+id/postal_address"
    android:layout_width="fill_parent"
    android:layout_height="wrap_content"
    android:hint="@string/postal_address_hint"
    android:inputType="textPostalAddress|
                       textCapWords|
                       textNoSuggestions" />

```

You can see a list of all available input types [here](https://developer.android.com/reference/android/widget/TextView.html#attr_android:inputType).



## Hiding SoftKeyboard


Hiding Softkeyboard is a **basic requirement** usually when working with EditText. The softkeyboard by **default** can only be closed by pressing back button and so most developers use [InputMethodManager](https://developer.android.com/reference/android/view/inputmethod/InputMethodManager.html) to force Android to hide the virtual keyboard calling [hideSoftInputFromWindow](http://developer.android.com/reference/android/view/inputmethod/InputMethodManager.html#hideSoftInputFromWindow%28android.os.IBinder,%20int%29) and passing in the token of the window containing your focused view. The code to do the following:

```java
public void hideSoftKeyboard()    
{
        InputMethodManager inputMethodManager = (InputMethodManager) getSystemService(Activity.INPUT_METHOD_SERVICE);
        inputMethodManager.hideSoftInputFromWindow(getCurrentFocus().getWindowToken(), 0);    
}

```

The code is direct, but another major problems that arises is that the hide function needs to be called when some event occurs. What to do when you need the Softkeyboard hidden upon pressing anywhere other than your EditText? The following code gives a neat function that needs to be called in your onCreate() method just once.

```java
public void setupUI(View view) 
{
        String s = "inside";
        //Set up touch listener for non-text box views to hide keyboard.
        if (!(view instanceof EditText)) {

            view.setOnTouchListener(new View.OnTouchListener() {

                public boolean onTouch(View v, MotionEvent event) {
                    hideSoftKeyboard();
                    return false;
                }

            });
        }

        //If a layout container, iterate over children and seed recursion.
        if (view instanceof ViewGroup) {

            for (int i = 0; i < ((ViewGroup) view).getChildCount(); i++) {

                View innerView = ((ViewGroup) view).getChildAt(i);

                setupUI(innerView);
            }
        }    
}

```



## Icon or button inside Custom Edit Text and its action and click listeners.


This example will help to have the Edit text with the icon at the right side.

> 
<p>Note: In this just I am using setCompoundDrawablesWithIntrinsicBounds,
So if you want to change the icon position you can achieve that using
setCompoundDrawablesWithIntrinsicBounds in setIcon.</p>


```java
public class MKEditText extends AppCompatEditText {

    public interface IconClickListener {
        public void onClick();
    }

    private IconClickListener mIconClickListener;

    private static final String TAG = MKEditText.class.getSimpleName();

    private final int EXTRA_TOUCH_AREA = 50;
    private Drawable mDrawable;
    private boolean touchDown;

    public MKEditText(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    public MKEditText(Context context) {
        super(context);
    }

    public MKEditText(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public void showRightIcon() {
        mDrawable = ContextCompat.getDrawable(getContext(), R.drawable.ic_android_black_24dp);

        setIcon();
    }

    public void setIconClickListener(IconClickListener iconClickListener) {
        mIconClickListener = iconClickListener;
    }

    private void setIcon() {
        Drawable[] drawables = getCompoundDrawables();

        setCompoundDrawablesWithIntrinsicBounds(drawables[0], drawables[1], mDrawable, drawables[3]);

        setInputType(InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD);
        setSelection(getText().length());
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        final int right = getRight();
        final int drawableSize = getCompoundPaddingRight();
        final int x = (int) event.getX();
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                if (x + EXTRA_TOUCH_AREA >= right - drawableSize && x <= right + EXTRA_TOUCH_AREA) {
                    touchDown = true;
                    return true;
                }
                break;
            case MotionEvent.ACTION_UP:
                if (x + EXTRA_TOUCH_AREA >= right - drawableSize && x <= right + EXTRA_TOUCH_AREA && touchDown) {
                    touchDown = false;
                    if (mIconClickListener != null) {
                        mIconClickListener.onClick();
                    }
                    return true;
                }
                touchDown = false;
                break;

        }
        return super.onTouchEvent(event);
    }
}

```

> 
<p>If you want to change the touch area you can change the
EXTRA_TOUCH_AREA values default I gave as 50.</p>


And for Enable the button and click listener you can call from your Activity or Fragment like this,

```java
MKEditText mkEditText = (MKEditText) findViewById(R.id.password);
mkEditText.showRightIcon();
mkEditText.setIconClickListener(new MKEditText.IconClickListener() {
            @Override
            public void onClick() {
              // You can do action here for the icon.
               
            }
        });

```



## `inputype` attribute


`inputype` attribute in `EditText` widget: **(tested on Android 4.4.3 and 2.3.3)**

```java
<EditText android:id="@+id/et_test" android:inputType="?????"/>

```

**textLongMessage=** Keyboard: alphabet/default. Enter button: Send/Next. Emotion: yes. Case: lowercase. Suggestion: yes. Add. chars: **,** and **.** and everything

**textFilter=** Keyboard: alphabet/default. Enter button: Send/Next. Emotion: yes. Case: lowercase. **Suggestion: no**. Add. chars: **,** and **.** and everything

**textCapWords=** Keyboard: alphabet/default. Enter button: Send/Next. Emotion: yes. **Case: Camel Case**. Suggestion: yes. Add. chars: **,** and **.** and everything

**textCapSentences=** Keyboard: alphabet/default. Enter button: Send/Next. Emotion: yes. **Case: Sentence case**. Suggestion: yes. Add. chars: **,** and **.** and everything

**time=** Keyboard: numeric. Enter button: Send/Next. Emotion: no. Case: -. **Suggestion: no**. Add. chars: :

**textMultiLine=** Keyboard: alphabet/default. **Enter button: nextline**. Emotion: yes. Case: lowercase. Suggestion: yes. Add. chars: **,** and **.** and everything

**number=** **Keyboard: numeric**. Enter button: Send/Next. Emotion: no. Case: -. Suggestion: no. **Add. chars: nothing**

**textEmailAddress=** Keyboard: alphabet/default. Enter button: Send/Next. **Emotion: no**. Case: lowercase. **Suggestion: no**. Add. chars: **@** and **.** and everything

**(No type)=** Keyboard: alphabet/default. **Enter button: nextline**. Emotion: yes. Case: lowercase. Suggestion: yes. Add. chars: **,** and **.** and everything

**textPassword=** Keyboard: alphabet/default. Enter button: Send/Next. Emotion: no. Case: lowercase. **Suggestion: no**. Add. chars: **,** and **.** and everything

**text=** Keyboard: Keyboard: alphabet/default. Enter button: Send/Next. Emotion: yes. Case: lowercase. Suggestion: yes. Add. chars: **,** and **.** and everything

**textShortMessage=** Keyboard: alphabet/default. **Enter button: emotion**. Emotion: yes. Case: lowercase. Suggestion: yes. Add. chars: **,** and **.** and everything

**textUri=** Keyboard: alphabet/default. Enter button: Send/Next. Emotion: no. Case: lowercase. **Suggestion: no**. Add. chars: **/** and **.** and everything

**textCapCharacters=** Keyboard: alphabet/default. Enter button: Send/Next. Emotion: yes. **Case: UPPERCASE**. Suggestion: yes. Add. chars: **,** and **.** and everything

**phone=** **Keyboard: numeric**. Enter button: Send/Next. Emotion: no. Case: -. **Suggestion: no**. Add. chars: *** # . -  / () W P N , +**

**textPersonName=** Keyboard: alphabet/default. Enter button: Send/Next. Emotion: yes. Case: lowercase. Suggestion: yes. Add. chars: **,** and **.** and everything<br/>
<br/>
<br/>
Note: `Auto-capitalization` setting will change the default behavior.

Note 2: In the `Numeric keyboard`, ALL numbers are English 1234567890.

Note 3: `Correction/Suggestion` setting will change the default behavior.


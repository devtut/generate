---
metaTitle: "Android - In-app Billing"
description: "Consumable In-app Purchases, (Third party) In-App v3 Library"
---

# In-app Billing



## Consumable In-app Purchases


Consumable Managed Products are products that can be bought multiple times such as in-game currency, game lives, power-ups, etc.

In this example, we are going to implement 4 different consumable **managed products** `"item1", "item2", "item3", "item4"`.

> 
<h3>Steps in summary:</h3>
<ol>
- Add the In-app Billing library to your project (AIDL File).
- Add the required permission in `AndroidManifest.xml` file.
- Deploy a signed apk to Google Developers Console.
- Define your products.
- Implement the code.
- Test In-app Billing (optional).
</ol>


### Step 1:

First of all, we will need to add the AIDL file to your project as clearly explained in Google Documentation [here](https://developer.android.com/google/play/billing/billing_integrate.html#billing-add-aidl).

`IInAppBillingService.aidl` is an Android Interface Definition Language (AIDL) file that defines the interface to the In-app Billing Version 3 service. You will use this interface to make billing requests by invoking IPC method calls.

### Step 2:

After adding the AIDL file, add BILLING permission in `AndroidManifest.xml`:

```java
<!-- Required permission for implementing In-app Billing -->
<uses-permission android:name="com.android.vending.BILLING" />

```

### Step 3:

Generate a signed apk, and upload it to Google Developers Console. This is required so that we can start defining our in-app products there.

### Step 4:

Define all your products with different productID, and set a price to each one of them. There are 2 types of products (Managed Products and Subscriptions). As we already said, we are going to implement 4 different consumable **managed products** `"item1", "item2", "item3", "item4"`.

### Step 5:

After doing all the steps above, you are now ready to start implementing the code itself in your own activity.

**MainActivity:**

```java
public class MainActivity extends Activity {

    IInAppBillingService inAppBillingService;
    ServiceConnection serviceConnection;

    // productID for each item. You should define them in the Google Developers Console.
    final String item1 = "item1";
    final String item2 = "item2";
    final String item3 = "item3";
    final String item4 = "item4";

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Instantiate the views according to your layout file.
        final Button buy1 = (Button) findViewById(R.id.buy1);
        final Button buy2 = (Button) findViewById(R.id.buy2);
        final Button buy3 = (Button) findViewById(R.id.buy3);
        final Button buy4 = (Button) findViewById(R.id.buy4);

        // setOnClickListener() for each button.
        // buyItem() here is the method that we will implement to launch the PurchaseFlow.
        buy1.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                buyItem(item1);
            }
        });

        buy2.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                buyItem(item2);
            }
        });

        buy3.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                buyItem(item3);
            }
        });

        buy4.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                buyItem(item4);
            }
        });

        // Attach the service connection.
        serviceConnection = new ServiceConnection() {
            @Override
            public void onServiceDisconnected(ComponentName name) {
                inAppBillingService = null;
            }

            @Override
            public void onServiceConnected(ComponentName name, IBinder service) {
                inAppBillingService = IInAppBillingService.Stub.asInterface(service);
            }
        };

        // Bind the service.
        Intent serviceIntent = new Intent("com.android.vending.billing.InAppBillingService.BIND");
        serviceIntent.setPackage("com.android.vending");
        bindService(serviceIntent, serviceConnection, BIND_AUTO_CREATE);

        // Get the price of each product, and set the price as text to
        // each button so that the user knows the price of each item.
        if (inAppBillingService != null) {
            // Attention: You need to create a new thread here because
            // getSkuDetails() triggers a network request, which can
            // cause lag to your app if it was called from the main thread.
            Thread thread = new Thread(new Runnable() {
                @Override
                public void run() {
                    ArrayList<String> skuList = new ArrayList<>();
                    skuList.add(item1);
                    skuList.add(item2);
                    skuList.add(item3);
                    skuList.add(item4);
                    Bundle querySkus = new Bundle();
                    querySkus.putStringArrayList("ITEM_ID_LIST", skuList);

                    try {
                        Bundle skuDetails = inAppBillingService.getSkuDetails(3, getPackageName(), "inapp", querySkus);
                        int response = skuDetails.getInt("RESPONSE_CODE");

                        if (response == 0) {
                            ArrayList<String> responseList = skuDetails.getStringArrayList("DETAILS_LIST");

                            for (String thisResponse : responseList) {
                                JSONObject object = new JSONObject(thisResponse);
                                String sku = object.getString("productId");
                                String price = object.getString("price");

                                switch (sku) {
                                    case item1:
                                        buy1.setText(price);
                                        break;
                                    case item2:
                                        buy2.setText(price);
                                        break;
                                    case item3:
                                        buy3.setText(price);
                                        break;
                                    case item4:
                                        buy4.setText(price);
                                        break;
                                }
                            }
                        }
                    } catch (RemoteException | JSONException e) {
                        e.printStackTrace();
                    }
                }
            });
            thread.start();
        }
    }

    // Launch the PurchaseFlow passing the productID of the item the user wants to buy as a parameter.
    private void buyItem(String productID) {
        if (inAppBillingService != null) {
            try {
                Bundle buyIntentBundle = inAppBillingService.getBuyIntent(3, getPackageName(), productID, "inapp", "bGoa+V7g/yqDXvKRqq+JTFn4uQZbPiQJo4pf9RzJ");
                PendingIntent pendingIntent = buyIntentBundle.getParcelable("BUY_INTENT");
                startIntentSenderForResult(pendingIntent.getIntentSender(), 1003, new Intent(), 0, 0, 0);
            } catch (RemoteException | IntentSender.SendIntentException e) {
                e.printStackTrace();
            }
        }
    }

    // Unbind the service in onDestroy(). If you don’t unbind, the open
    // service connection could cause your device’s performance to degrade.
    @Override
    public void onDestroy() {
        super.onDestroy();
        if (inAppBillingService != null) {
            unbindService(serviceConnection);
        }
    }

    // Check here if the in-app purchase was successful or not. If it was successful,
    // then consume the product, and let the app make the required changes.
    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        if (requestCode == 1003 && resultCode == RESULT_OK) {

            final String purchaseData = data.getStringExtra("INAPP_PURCHASE_DATA");

            // Attention: You need to create a new thread here because
            // consumePurchase() triggers a network request, which can
            // cause lag to your app if it was called from the main thread.
            Thread thread = new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        JSONObject jo = new JSONObject(purchaseData);
                        // Get the productID of the purchased item.
                        String sku = jo.getString("productId");
                        String productName = null;

                        // increaseCoins() here is a method used as an example in a game to
                        // increase the in-game currency if the purchase was successful.
                        // You should implement your own code here, and let the app apply
                        // the required changes after the purchase was successful.
                        switch (sku) {
                            case item1:
                                productName = "Item 1";
                                increaseCoins(2000);
                                break;
                            case item2:
                                productName = "Item 2";
                                increaseCoins(8000);
                                break;
                            case item3:
                                productName = "Item 3";
                                increaseCoins(18000);
                                break;
                            case item4:
                                productName = "Item 4";
                                increaseCoins(30000);
                                break;
                        }

                        // Consume the purchase so that the user is able to purchase the same product again.
                        inAppBillingService.consumePurchase(3, getPackageName(), jo.getString("purchaseToken"));
                        Toast.makeText(MainActivity.this, productName + " is successfully purchased. Excellent choice, master!", Toast.LENGTH_LONG).show();
                    } catch (JSONException | RemoteException e) {
                        Toast.makeText(MainActivity.this, "Failed to parse purchase data.", Toast.LENGTH_LONG).show();
                        e.printStackTrace();
                    }
                }
            });
            thread.start();
        }
    }
}

```

### Step 6:

After implementing the code, you can test it by deploying your apk to beta/alpha channel, and let other users test the code for you. However, real in-app purchases can't be made while in testing mode. You have to publish your app/game first to Play Store so that all the products are fully activated.

More info on testing In-app Billing can be found [here](https://developer.android.com/google/play/billing/billing_testing.html).



## (Third party) In-App v3 Library


**Step 1:** First of all follow these two steps to add in app functionality :

**1. Add the library using :**

```

repositories {
            mavenCentral()
        }
        dependencies {
           compile 'com.anjlab.android.iab.v3:library:1.0.+'
        }

```

**2. Add permission in manifest file.**

```java
<uses-permission android:name="com.android.vending.BILLING" />

```

**Step 2: Initialise your billing processor:**

```java
BillingProcessor bp = new BillingProcessor(this, "YOUR LICENSE KEY FROM GOOGLE PLAY CONSOLE HERE", this);

```

and implement Billing Handler : BillingProcessor.IBillingHandler
which contains 4 methods :
a. onBillingInitialized();
b. onProductPurchased(String productId, TransactionDetails details) : This is where you need to handle actions to be performed after successful purchase
c. onBillingError(int errorCode, Throwable error) : Handle any error occurred during purchase process
d. onPurchaseHistoryRestored() : For restoring in app purchases

**Step 3: How to purchase a product.**

To purchase a managed product :

```java
bp.purchase(YOUR_ACTIVITY, "YOUR PRODUCT ID FROM GOOGLE PLAY CONSOLE HERE");

```

And to Purchase a subscription :

```java
bp.subscribe(YOUR_ACTIVITY, "YOUR SUBSCRIPTION ID FROM GOOGLE PLAY CONSOLE HERE");

```

**Step 4 : Consuming a product.**

To consume a product simply call consumePurchase method.

bp.consumePurchase("YOUR PRODUCT ID FROM GOOGLE PLAY CONSOLE HERE");

**For other methods related to in app visit [github](https://github.com/anjlab/android-inapp-billing-v3/releases)**


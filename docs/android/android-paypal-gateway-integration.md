---
metaTitle: "Android - Android Paypal Gateway Integration"
description: "Setup paypal in your android code"
---

# Android Paypal Gateway Integration



## Setup paypal in your android code


1)First go through  Paypal Developer web site and create an application.

2)Now open your manifest file and give the below permissions

```java
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />

```

3)And some required Activity and Services-

```

<service
        android:name="com.paypal.android.sdk.payments.PayPalService"
        android:exported="false" />
    <activity android:name="com.paypal.android.sdk.payments.PaymentActivity" />
    <activity android:name="com.paypal.android.sdk.payments.LoginActivity" />
    <activity android:name="com.paypal.android.sdk.payments.PaymentMethodActivity" />
    <activity android:name="com.paypal.android.sdk.payments.PaymentConfirmActivity" />
    <activity android:name="com.paypal.android.sdk.payments.PayPalFuturePaymentActivity" />
    <activity android:name="com.paypal.android.sdk.payments.FuturePaymentConsentActivity" />
    <activity android:name="com.paypal.android.sdk.payments.FuturePaymentInfoActivity" />
    <activity
        android:name="io.card.payment.CardIOActivity"
        android:configChanges="keyboardHidden|orientation" />
    <activity android:name="io.card.payment.DataEntryActivity" />

```

4)Open your Activity class and set Configuration for your app-

```java
//set the environment for production/sandbox/no netowrk
 private static final String CONFIG_ENVIRONMENT = PayPalConfiguration.ENVIRONMENT_PRODUCTION;

```

5)Now set client id from the Paypal developer account-
private static final String CONFIG_CLIENT_ID = "PUT YOUR CLIENT ID";
6)Inside onCreate method call the Paypal service-
Intent intent = new Intent(this, PayPalService.class);
intent.putExtra(PayPalService.EXTRA_PAYPAL_CONFIGURATION, config);
startService(intent);

7)Now you are ready to make a payment just on button press call the Payment Activity-

```java
PayPalPayment thingToBuy = new PayPalPayment(new BigDecimal(1),"USD", "androidhub4you.com",
                                 PayPalPayment.PAYMENT_INTENT_SALE);
                             Intent intent = new Intent(MainActivity.this, PaymentActivity.class);
                             intent.putExtra(PaymentActivity.EXTRA_PAYMENT, thingToBuy);

                             startActivityForResult(intent, REQUEST_PAYPAL_PAYMENT);    

```

8)And finally from the onActivityResult get the payment response-

```java
@Override
protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    if (requestCode == REQUEST_PAYPAL_PAYMENT) {
        if (resultCode == Activity.RESULT_OK) {
            PaymentConfirmation confirm = data
                    .getParcelableExtra(PaymentActivity.EXTRA_RESULT_CONFIRMATION);
            if (confirm != null) {
                try {
                System.out.println("Responseeee"+confirm);
                    Log.i("paymentExample", confirm.toJSONObject().toString());
                
                    JSONObject jsonObj=new JSONObject(confirm.toJSONObject().toString());
                    
                    String paymentId=jsonObj.getJSONObject("response").getString("id");
                    System.out.println("payment id:-=="+paymentId);
                    Toast.makeText(getApplicationContext(), paymentId, Toast.LENGTH_LONG).show(); 
                } catch (JSONException e) {
                    Log.e("paymentExample", "an extremely unlikely failure occurred: ", e);
                }
            }
        } else if (resultCode == Activity.RESULT_CANCELED) {
            Log.i("paymentExample", "The user canceled.");
        } else if (resultCode == PaymentActivity.RESULT_EXTRAS_INVALID) {
            Log.i("paymentExample", "An invalid Payment was submitted. Please see the docs.");
        }
    }
    
    
}

```



#### Remarks


Paypal provide us their own library for payment so it is now much secure and easy to implement in our application. Below are the important step to do.


---
metaTitle: "Android - Security"
description: "Verifying App Signature - Tamper Detection"
---

# Security



## Verifying App Signature - Tamper Detection


This technique details how to ensure that your .apk has been signed with your developer certificate, and leverages the fact that the certificate remains consistent and that only you have access to it.
We can break this technique into 3 simple steps:

- Find your developer certificate signature.
- Embed your signature in a String constant in your app.
<li>Check that the signature at runtime
matches our embedded developer signature.</li>

Here's the code snippet:

```java
private static final int VALID = 0;
private static final int INVALID = 1;

public static int checkAppSignature(Context context) {

try {
      PackageInfo packageInfo = context.getPackageManager().getPackageInfo(context.getPackageName(), PackageManager.GET_SIGNATURES);

      for (Signature signature : packageInfo.signatures) {

        byte[] signatureBytes = signature.toByteArray();

        MessageDigest md = MessageDigest.getInstance("SHA");

        md.update(signature.toByteArray());

        final String currentSignature = Base64.encodeToString(md.digest(), Base64.DEFAULT);

        Log.d("REMOVE_ME", "Include this string as a value for SIGNATURE:" + currentSignature);

        //compare signatures
        if (SIGNATURE.equals(currentSignature)){
          return VALID;
        };
      }
    } catch (Exception e) {
        //assumes an issue in checking signature., but we let the caller decide on what to do.
    }

    return INVALID;

}

```


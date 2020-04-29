---
metaTitle: "Java Print Service"
description: "Building the Doc that will be printed, Discovering the available print services, Defining print request attributes, Discovering the default print service, Creating a print job from a print service, Listening print job request status change"
---

# Java Print Service


The [Java Print Service API](https://docs.oracle.com/javase/8/docs/technotes/guides/jps/spec/JPSTOC.fm.html) provides functionalities to discover print services and send print requests for them.

It includes extensible print attributes based on the standard attributes specified in the [Internet Printing Protocol (IPP) 1.1](https://en.wikipedia.org/wiki/Internet_Printing_Protocol) from the IETF Specification, [RFC 2911](https://tools.ietf.org/html/rfc2911).



## Building the Doc that will be printed


`Doc` is an interface and the Java Print Service API provide a simple implementation called `SimpleDoc`.

Every `Doc` instance is basically made of two aspects:

- the print data content itself (an E-mail, an image, a document etc)
- the print data format, called `DocFlavor` (MIME type + Representation class).

Before creating the `Doc` object, we need to load our document from somewhere. In the example, we will load an specific file from the disk:

```java
FileInputStream pdfFileInputStream = new FileInputStream("something.pdf");

```

So now, we have to choose a `DocFlavor` that matches our content. The `DocFlavor` class has a bunch of constants to represent the most usual types of data. Let's pick the `INPUT_STREAM.PDF` one:

```java
DocFlavor pdfDocFlavor = DocFlavor.INPUT_STREAM.PDF;

```

Now, we can create a new instance of `SimpleDoc`:

```java
Doc doc = new SimpleDoc(pdfFileInputStream, pdfDocFlavor , null);

```

The `doc` object now can be sent to the print job request (see [Creating a print job from a print service](https://stackoverflow.com/documentation/java/10178/java-print-service/31198/creating-a-print-job-from-a-print-service)).



## Discovering the available print services


To discovery all the available print services, we can use the `PrintServiceLookup` class. Let's see how:

```java
import javax.print.PrintService;
import javax.print.PrintServiceLookup;

public class DiscoveringAvailablePrintServices {

    public static void main(String[] args) {
        discoverPrintServices();
    }

    public static void discoverPrintServices() {
        PrintService[] allPrintServices = PrintServiceLookup.lookupPrintServices(null, null);
        
        for (Printservice printService : allPrintServices) {
            System.out.println("Print service name: " + printService.getName());
        }
    }

}

```

This program, when executed on a Windows environment, will print something like this:

```java
Print service name: Fax
Print service name: Microsoft Print to PDF
Print service name: Microsoft XPS Document Viewer

```



## Defining print request attributes


Sometimes we need to determine some aspects of the print request. We will call them **attribute**.

Are examples of print request attributes:

- amount of copies (1, 2 etc),
- orientation (portrait or landscape)
- chromacity (monochrome, color)
- quality (draft, normal, high)
- sides (one-sided, two-sided etc)
- and so on...

Before choosing one of them and which value each one will have, first we need to build a set of attributes:

```java
PrintRequestAttributeSet pras = new HashPrintRequestAttributeSet();

```

Now we can add them. Some examples are:

```java
pras.add(new Copies(5));
pras.add(MediaSize.ISO_A4);
pras.add(OrientationRequested.PORTRAIT);
pras.add(PrintQuality.NORMAL);

```

The `pras` object now can be sent to the print job request (see [Creating a print job from a print service](https://stackoverflow.com/documentation/java/10178/java-print-service/31198/creating-a-print-job-from-a-print-service)).



## Discovering the default print service


To discovery the default print service, we can use the `PrintServiceLookup` class. Let's see how::

```java
import javax.print.PrintService;
import javax.print.PrintServiceLookup;

public class DiscoveringDefaultPrintService {

    public static void main(String[] args) {
        discoverDefaultPrintService();
    }

    public static void discoverDefaultPrintService() {
        PrintService defaultPrintService = PrintServiceLookup.lookupDefaultPrintService();
        System.out.println("Default print service name: " + defaultPrintService.getName());
    }

}

```



## Creating a print job from a print service


A print job is a request of printing something in a specific print service.
It consists, basically, by:

- the data that will be printed  (see [Building the Doc that will be printed](https://stackoverflow.com/documentation/java/10178/java-print-service/31199/building-the-doc-that-will-be-printed))
- a set of attributes

After picking-up the right print service instance, we can request the creation of a print job:

```java
DocPrintJob printJob = printService.createPrintJob();

```

The `DocPrintJob` interface provide us the `print` method:

```java
printJob.print(doc, pras);

```

The `doc` argument is a `Doc`: the data that will be printed.

And the `pras` argument is a `PrintRequestAttributeSet` interface: a set of `PrintRequestAttribute`. Are examples of print request attributes:

- amount of copies (1, 2 etc),
- orientation (portrait or landscape)
- chromacity (monochrome, color)
- quality (draft, normal, high)
- sides (one-sided, two-sided etc)
- and so on...

The print method may throw a `PrintException`.



## Listening print job request status change


For the most printing clients, is extremely useful to know if a print job has finished or failed.

The Java Print Service API provide some functionalities to get informed about these scenarios.
All we have to do is:

- provide an implementation for `PrintJobListener` interface and
- register this implementation at the print job.

When the print job state changes, we will be notified.
We can do anything is needed, for example:

- update a user interface,
- start another business process,
- record something in the database,
- or simply log it.

In the example bellow, we will log every print job status change:

```java
import javax.print.event.PrintJobEvent;
import javax.print.event.PrintJobListener;

public class LoggerPrintJobListener implements PrintJobListener {

    // Your favorite Logger class goes here!
    private static final Logger LOG = Logger.getLogger(LoggerPrintJobListener.class);


    public void printDataTransferCompleted(PrintJobEvent pje) {
        LOG.info("Print data transfer completed ;) ");
    }

    public void printJobCompleted(PrintJobEvent pje) {
        LOG.info("Print job completed =) ");
    }

    public void printJobFailed(PrintJobEvent pje) {
        LOG.info("Print job failed =( ");
    }

    public void printJobCanceled(PrintJobEvent pje) {
        LOG.info("Print job canceled :| ");
    }

    public void printJobNoMoreEvents(PrintJobEvent pje) {
        LOG.info("No more events to the job ");
    }

    public void printJobRequiresAttention(PrintJobEvent pje) {
        LOG.info("Print job requires attention :O ");
    }
}

```

Finally, we can add our print job listener implementation on the print job before the print request itself, as follows:

```java
DocPrintJob printJob = printService.createPrintJob();

printJob.addPrintJobListener(new LoggerPrintJobListener());

printJob.print(doc, pras);

```

### The **PrintJobEvent pje** argument

Notice that every method has a `PrintJobEvent pje` argument.
We don't use it in this example for simplicity purposes, but you can use it to explore the status.
For example:

```java
pje.getPrintJob().getAttributes();

```

Will return a `PrintJobAttributeSet` object instance and you can run them in a for-each way.

### Another way to achieve the same goal

Another option to achieve the same goal is extending the `PrintJobAdapter` class, as the name says, is an adapter for `PrintJobListener`.
Implementing the interface we compulsorily have to implement all of them.
The advantage of this way it's we need to override only the methods we want. Let's see how it works:

```java
import javax.print.event.PrintJobEvent;
import javax.print.event.PrintJobAdapter;

public class LoggerPrintJobAdapter extends PrintJobAdapter {

    // Your favorite Logger class goes here!
    private static final Logger LOG = Logger.getLogger(LoggerPrintJobAdapter.class);

    public void printJobCompleted(PrintJobEvent pje) {
        LOG.info("Print job completed =) ");
    }

    public void printJobFailed(PrintJobEvent pje) {
        LOG.info("Print job failed =( ");
    }
}

```

Notice that we override only some specific methods.

As the same way in the example implementing the interface `PrintJobListener`, we add the listener to the print job before sending it to print:

```java
printJob.addPrintJobListener(new LoggerPrintJobAdapter());

printJob.print(doc, pras);

```


---
metaTitle: "Imagick"
description: "First Steps, Convert Image into base64 String"
---

# Imagick



## First Steps


**Installation**

****Using apt on Debian based systems****

```
sudo apt-get install php5-imagick

```

****Using Homebrew on OSX/macOs****

```
brew install imagemagick

```

To see the dependencies installed using the `brew` method, visit [brewformulas.org/Imagemagick](http://brewformulas.org/Imagemagick).

****Using binary releases****

Instructions on [imagemagick website](https://www.imagemagick.org/script/binary-releases.php#macosx).

**Usage**

```
<?php

$imagen = new Imagick('imagen.jpg');
$imagen->thumbnailImage(100, 0); 
//if you put 0 in the parameter aspect ratio is maintained

echo $imagen;

?>

```



## Convert Image into base64 String


This example is how to turn an image into a Base64 string (i.e. a string you can use directly in a `src` attribute of an `img` tag). This example specifically uses the [Imagick](http://php.net/manual/en/intro.imagick.php) library (there are others available, such as [GD](http://php.net/manual/en/intro.image.php) as well).

```
<?php
/**
 * This loads in the file, image.jpg for manipulation. 
 * The filename path is releative to the .php file containing this code, so
 * in this example, image.jpg should live in the same directory as our script.
 */
$img = new Imagick('image.jpg');

/**
 * This resizes the image, to the given size in the form of width, height.
 * If you want to change the resolution of the image, rather than the size
 * then $img->resampleimage(320, 240) would be the right function to use.
 *
 * Note that for the second parameter, you can set it to 0 to maintain the
 * aspect ratio of the original image.
 */
$img->resizeImage(320, 240);

/**
 * This returns the unencoded string representation of the image
 */
$imgBuff = $img->getimageblob();

/**
 * This clears the image.jpg resource from our $img object and destroys the
 * object. Thus, freeing the system resources allocated for doing our image
 * manipulation.
 */
$img->clear(); 

/**
 * This creates the base64 encoded version of our unencoded string from
 * earlier. It is then output as an image to the page.
 * 
 * Note, that in the src attribute, the image/jpeg part may change based on
 * the image type you're using (i.e. png, jpg etc).
 */
$img = base64_encode($imgBuff);
echo "<img alt='Embedded Image' src='data:image/jpeg;base64,$img' />";

```


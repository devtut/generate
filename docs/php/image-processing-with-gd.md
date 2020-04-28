---
metaTitle: "Image Processing with GD"
description: "Image output, Creating an image, Image Cropping and Resizing"
---

# Image Processing with GD



## Image output


An image can be created using [`image*` functions](http://php.net/manual/en/function.imagepng.php#refsect1-function.imagepng-seealso), where `*` is the file format.

They have this syntax in common:

```php
bool image___(resource $im [, mixed $to [ other parameters]] )

```

### Saving to a file

If you want to save the image to a file, you can pass the filename, or an opened file stream, as `$to`. If you pass a stream, you don't need to close it, because GD will automatically close it.

For example, to save a PNG file:

```php
imagepng($image, "/path/to/target/file.png");

$stream = fopen("phar://path/to/target.phar/file.png", "wb");
imagepng($image2, $stream);
// Don't fclose($stream)

```

When using `fopen`, make sure to use the `b` flag rather than the `t` flag, because the file is a binary output.

Do **not** try to pass `fopen("php://temp", $f)` or `fopen("php://memory", $f)` to it. Since the stream is closed by the function after the call, you will be unable to use it further, such as to retrieve its contents.

### Output as an HTTP response

If you want to directly return this image as the response of the image (e.g. to create dynamic badges), you don't need to pass anything (or pass `null`) as the second argument. However, in the HTTP response, you need to specify your content type:

```php
header("Content-Type: $mimeType");

```

`$mimeType` is the MIME type of the format you are returning. Examples include `image/png`, `image/gif` and `image/jpeg`.

### Writing into a variable

There are two ways to write into a variable.

### Using OB (Output Buffering)

```php
ob_start();
imagepng($image, null, $quality); // pass null to supposedly write to stdout
$binary = ob_get_clean();

```

### Using stream wrappers

You may have many reasons that you don't want to use output buffering. For example, you may already have OB on. Therefore, an alternative is needed.

Using the `stream_wrapper_register` function, a new stream wrapper can be registered. Hence, you can pass a stream to the image output function, and retrieve it later.

```php
<?php

class GlobalStream{
        private $var;

        public function stream_open(string $path){
                $this->var =& $GLOBALS[parse_url($path)["host"]];
                return true;
        }

        public function stream_write(string $data){
                $this->var .= $data;
                return strlen($data);
        }
}

stream_wrapper_register("global", GlobalStream::class);

$image = imagecreatetruecolor(100, 100);
imagefill($image, 0, 0, imagecolorallocate($image, 0, 0, 0));

$stream = fopen("global://myImage", "");
imagepng($image, $stream);
echo base64_encode($myImage);

```

In this example, the `GlobalStream` class writes any input into the reference variable (i.e. indirectly write to the global variable of the given name). The global variable can later be retrieved directly.

There are some special things to note:

- A fully implemented stream wrapper class should look like [this](http://php.net/manual/en/stream.streamwrapper.example-1.php), but according to tests with the `__call` magic method, only `stream_open`, `stream_write` and `stream_close` are called from internal functions.
- No flags are required in the `fopen` call, but you should at least pass an empty string. This is because the `fopen` function expects such parameter, and even if you don't use it in your `stream_open` implementation, a dummy one is still required.
- According to tests, `stream_write` is called multiple times. Remember to use `.=` (concatenation assignment), not `=` (direct variable assignment).

### Example usage

In the `<img>` HTML tag, an image can be directly provided rather than using an external link:

```php
echo '<img src="data:image/png;base64,' . base64_encode($binary) . '">';

```



## Creating an image


To create a blank image, use the `imagecreatetruecolor` function:

```php
$img = imagecreatetruecolor($width, $height);

```

`$img` is now a resource variable for an image resource with `$width`x`$height` pixels. Note that width counts from left to right, and height counts from top to bottom.

Image resources can also be created from [image creation functions](http://php.net/manual/en/ref.image.php), such as:

- `imagecreatefrompng`
- `imagecreatefromjpeg`
- other `imagecreatefrom*` functions.

Image resources may be freed later when there are no more references to them. However, to free the memory immediately (this may be important if you are processing many large images), using `imagedestroy()` on an image when it is no longer used might be a good practice.

```php
imagedestroy($image);

```

### Converting an image

Images created by image conversion does not modify the image until you output it. Therefore, an image converter can be as simple as three lines of code:

```php
function convertJpegToPng(string $filename, string $outputFile) {
    $im = imagecreatefromjpeg($filename);
    imagepng($im, $outputFile);
    imagedestroy($im);
}

```



## Image Cropping and Resizing


If you have an image and want to create a new image, with new dimensions, you can use `imagecopyresampled` function:

first create a new `image` with desired dimensions:

```php
// new image
$dst_img = imagecreatetruecolor($width, $height);

```

and store the original image into a variable. To do so, you may use one of the `createimagefrom*` functions where * stands for:

- jpeg
- gif
- png
- string

For example:

```php
//original image
$src_img=imagecreatefromstring(file_get_contents($original_image_path));

```

Now, copy all (or part of) original image (src_img) into the new image (dst_img) by `imagecopyresampled`:

```php
imagecopyresampled($dst_img, $src_img, 
    $dst_x ,$dst_y, $src_x, $src_y, 
    $dst_width, $dst_height, $src_width, $src_height);

```

To set `src_*` and `dst_*` dimensions, use the below image:

[<img src="https://i.stack.imgur.com/6MFxN.jpg" alt="enter image description here" />](https://i.stack.imgur.com/6MFxN.jpg)

Now, if you want to copy entire of source (initial) image, into entire of destination area (no cropping):

```php
$src_x = $src_y = $dst_x = $dst_y = 0;
$dst_width = $width;// width of new image
$dst_height = $height; //height of new image
$src_width = imagesx($src_img); //width of initial image
$src_height = imagesy($src_img); //height of initial image

```



#### Remarks


When using `header("Content-Type: $mimeType");` and `image____` to generate only an image to the output, be sure to output nothing else, note even a blank line after `?>`.  (That can be a difficult 'bug' to track down -- you get no image and no clue as to why.)  The general advice is to not include ?> at all here.


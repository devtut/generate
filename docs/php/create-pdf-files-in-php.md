---
metaTitle: "Create PDF files in PHP"
description: "Getting Started with PDFlib"
---

# Create PDF files in PHP



## Getting Started with PDFlib


This code requires that you use the [PDFlib library](http://php.net/manual/en/ref.pdf.php) for it to function properly.

```
<?php 
$pdf = pdf_new(); //initialize new object

pdf_begin_document($pdf); //create new blank PDF
    pdf_set_info($pdf, "Author", "John Doe"); //Set info about your PDF
    pdf_set_info($pdf, "Title", "HelloWorld");
        pdf_begin_page($pdf, (72 * 8.5), (72 * 11)); //specify page width and height
            $font = pdf_findfont($pdf, "Times-Roman", "host", 0) //load a font
            pdf_setfont($pdf, $font, 48); //set the font
            pdf_set_text_pos($pdf, 50, 700); //assign text position
            pdf_show($pdf, "Hello_World!"); //print text to assigned position
        pdf_end_page($pdf); //end the page
pdf_end_document($pdf); //close the object

$document = pdf_get_buffer($pdf); //retrieve contents from buffer

$length = strlen($document); $filename = "HelloWorld.pdf"; //Finds PDF length and assigns file name

header("Content-Type:application/pdf"); 
header("Content-Length:" . $length); 
header("Content-Disposition:inline; filename=" . $filename); 

echo($document); //Send document to browser
unset($document); pdf_delete($pdf);  //Clear Memory
?>

```


---
metaTitle: "VBA - Non-Latin Characters"
description: "Non-Latin Text in VBA Code, Non-Latin Identifiers and Language Coverage"
---

# Non-Latin Characters


VBA can read and write strings in any language or script using [Unicode](http://www.unicode.org/). However, there are stricter rules in place for [Identifier Tokens](https://msdn.microsoft.com/en-us/library/ee200272.aspx).



## Non-Latin Text in VBA Code


In spreadsheet cell A1, we have the following Arabic pangram:

صِف خَلقَ خَودِ كَمِثلِ الشَمسِ إِذ بَزَغَت — يَحظى الضَجيعُ بِها نَجلاءَ مِعطارِ

VBA provides the `AscW` and `ChrW` functions to work with multi-byte character codes. We can also use `Byte` arrays to manipulate the string variable directly:

```vb
Sub NonLatinStrings()

Dim rng As Range
Set rng = Range("A1")
Do Until rng = ""
    Dim MyString As String
    MyString = rng.Value
    
    ' AscW functions
    Dim char As String
    char = AscW(Left(MyString, 1))
    Debug.Print "First char (ChrW): " & char
    Debug.Print "First char (binary): " & BinaryFormat(char, 12)
    
    ' ChrW functions
    Dim uString As String
    uString = ChrW(char)
    Debug.Print "String value (text): " & uString        ' Fails! Appears as '?'
    Debug.Print "String value (AscW): " & AscW(uString)
    
    ' Using a Byte string
    Dim StringAsByt() As Byte
    StringAsByt = MyString
    Dim i As Long
    For i = 0 To 1 Step 2
        Debug.Print "Byte values (in decimal): " & _
            StringAsByt(i) & "|" & StringAsByt(i + 1)
        Debug.Print "Byte values (binary): " & _
            BinaryFormat(StringAsByt(i)) & "|" & BinaryFormat(StringAsByt(i + 1))
    Next i
    Debug.Print ""

    ' Printing the entire string to the immediate window fails (all '?'s)
    Debug.Print "Whole String" & vbNewLine & rng.Value
    Set rng = rng.Offset(1)
Loop

End Sub

```

This produces the following output for the [Arabic Letter Sad](http://www.fileformat.info/info/unicode/char/0635/index.htm):

> 
<p>First char (ChrW): 1589<br />
First char (binary): 00011000110101<br />
String value (text): ?<br />
String value (AscW): 1589<br />
Byte values (in decimal): 53|6<br />
Byte values (binary): 00110101|00000110</p>
<p>Whole String<br />
??? ????? ????? ??????? ??????? ??? ??????? — ????? ???????? ???? ??????? ???????</p>


Note that VBA is unable to print non-Latin text to the immediate window even though the string functions work correctly. This is a limitation of the IDE and not the language.



## Non-Latin Identifiers and Language Coverage


[VBA Identifiers](https://msdn.microsoft.com/en-us/library/ee200272.aspx) (variable and function names) can use the Latin script and may also be able to use [Japanese](https://msdn.microsoft.com/en-us/library/ee199767.aspx), [Korean](https://msdn.microsoft.com/en-us/library/ee177191.aspx), [Simplified Chinese](https://msdn.microsoft.com/en-us/library/ee199765.aspx), and [Traditional Chinese](https://msdn.microsoft.com/en-us/library/ee199727.aspx) scripts.

The extended Latin script has full coverage for many languages:<br />
English, French, Spanish, German, Italian, Breton, Catalan, Danish, Estonian, Finnish, Icelandic, Indonesian, Irish, Lojban, Mapudungun, Norwegian, Portuguese, Scottish Gaelic, Swedish, Tagalog

Some languages are only partially covered:<br />
Azeri, Croatian, Czech, Esperanto, Hungarian, Latvian, Lithuanian, Polish, Romanian, Serbian, Slovak, Slovenian, Turkish, Yoruba, Welsh

Some languages have little or no coverage:<br />
Arabic, Bulgarian, Cherokee, Dzongkha, Greek, Hindi, Macedonian, Malayalam, Mongolian, Russian, Sanskrit, Thai, Tibetan, Urdu, Uyghur

The following variable declarations are all valid:

```vb
Dim Yec’hed As String 'Breton
Dim «Dóna» As String 'Catalan
Dim fræk As String 'Danish
Dim tšellomängija As String 'Estonian
Dim Törkylempijävongahdus As String 'Finnish
Dim j’examine As String 'French
Dim Paß As String 'German
Dim þjófum As String 'Icelandic
Dim hÓighe As String 'Irish
Dim sofybakni As String 'Lojban (.o’i does not work)
Dim ñizol As String 'Mapudungun
Dim Vår As String 'Norwegian
Dim «brações» As String 'Portuguese
Dim d’fhàg As String 'Scottish Gaelic

```

Note that in the VBA IDE, a single apostrophe within a variable name does not turn the line into a comment (as it does on Stack Overflow).

Also, languages that use two angles to indicate a quote «» are allowed to use those in variable names desipte the fact that the ""-type quotes are not.


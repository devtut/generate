---
metaTitle: "C# | Including Font Resources"
description: "Instantiate 'Fontfamily' from Resources, Integration method, Usage with a 'Button'"
---

# Including Font Resources




## Instantiate 'Fontfamily' from Resources


```cs
public FontFamily Maneteke = GetResourceFontFamily(Properties.Resources.manteka);

```

[<img src="https://i.stack.imgur.com/1fneu.png" alt="enter image description here" />](https://i.stack.imgur.com/1fneu.png)



## Integration method


```cs
public static FontFamily GetResourceFontFamily(byte[] fontbytes)
{
    PrivateFontCollection pfc = new PrivateFontCollection();
    IntPtr fontMemPointer = Marshal.AllocCoTaskMem(fontbytes.Length);
    Marshal.Copy(fontbytes, 0, fontMemPointer, fontbytes.Length);
    pfc.AddMemoryFont(fontMemPointer, fontbytes.Length);
    Marshal.FreeCoTaskMem(fontMemPointer);
    return pfc.Families[0];
}

```



## Usage with a 'Button'


```

   public static class Res
    {
        /// <summary>
        /// URL: https://www.behance.net/gallery/2846011/Manteka
        /// </summary>
        public static FontFamily Maneteke = GetResourceFontFamily(Properties.Resources.manteka);

        public static FontFamily GetResourceFontFamily(byte[] fontbytes)
        {
            PrivateFontCollection pfc = new PrivateFontCollection();
            IntPtr fontMemPointer = Marshal.AllocCoTaskMem(fontbytes.Length);
            Marshal.Copy(fontbytes, 0, fontMemPointer, fontbytes.Length);
            pfc.AddMemoryFont(fontMemPointer, fontbytes.Length);
            Marshal.FreeCoTaskMem(fontMemPointer);
            return pfc.Families[0];
        }
    }

    public class FlatButton : Button
    {
        public FlatButton() : base()
        {
            Font = new Font(Res.Maneteke, Font.Size);
        }

        protected override void OnFontChanged(EventArgs e)
        {
            base.OnFontChanged(e);
            this.Font = new Font(Res.Maneteke, this.Font.Size);
        }
    }

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|fontbytes|byte array from the binary .ttf


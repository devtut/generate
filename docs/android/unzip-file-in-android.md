---
metaTitle: "Android - Unzip File in Android"
description: "Unzip file"
---

# Unzip File in Android



## Unzip file


```java
private boolean unpackZip(String path, String zipname){       
 InputStream is;
 ZipInputStream zis;
 try 
 {
     String filename;
     is = new FileInputStream(path + zipname);
     zis = new ZipInputStream(new BufferedInputStream(is));          
     ZipEntry ze;
     byte[] buffer = new byte[1024];
     int count;

     while ((ze = zis.getNextEntry()) != null){
         // zapis do souboru
         filename = ze.getName();

         // Need to create directories if not exists, or
         // it will generate an Exception...
         if (ze.isDirectory()) {
            File fmd = new File(path + filename);
            fmd.mkdirs();
            continue;
         }

         FileOutputStream fout = new FileOutputStream(path + filename);

         // cteni zipu a zapis
         while ((count = zis.read(buffer)) != -1){
             fout.write(buffer, 0, count);             
         }

         fout.close();               
         zis.closeEntry();
     }

     zis.close();
 } 
 catch(IOException e){
     e.printStackTrace();
     return false;
 }

return true;}

```


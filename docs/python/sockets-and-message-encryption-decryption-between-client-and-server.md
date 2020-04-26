# Sockets And Message Encryption/Decryption Between Client and Server


Cryptography is used for security purposes. There are not so many examples of Encryption/Decryption in Python using IDEA encryption MODE CTR. **Aim of this documentation :**

Extend and implement of the RSA Digital Signature scheme in station-to-station communication. Using Hashing for integrity of message, that is SHA-1. Produce simple Key Transport protocol. Encrypt Key with IDEA encryption. Mode of Block Cipher is Counter Mode



## Server side Implementation


```
import socket
import hashlib
import os
import time
import itertools
import threading
import sys
import Crypto.Cipher.AES as AES
from Crypto.PublicKey import RSA
from CryptoPlus.Cipher import IDEA

#server address and port number input from admin
host= raw_input(&quot;Server Address - > &quot;)
port = int(input(&quot;Port - > &quot;))
#boolean for checking server and port
check = False
done = False

def animate():
    for c in itertools.cycle(['....','.......','..........','............']):
        if done:
            break
        sys.stdout.write('\rCHECKING IP ADDRESS AND NOT USED PORT '+c)
        sys.stdout.flush()
        time.sleep(0.1)
    sys.stdout.write('\r -----SERVER STARTED. WAITING FOR CLIENT-----\n')
try:
    #setting up socket
    server = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
    server.bind((host,port))
    server.listen(5)
    check = True
except BaseException:
    print &quot;-----Check Server Address or Port-----&quot;
    check = False

if check is True:
    # server Quit
    shutdown = False
# printing &quot;Server Started Message&quot;
thread_load = threading.Thread(target=animate)
thread_load.start()

time.sleep(4)
done = True
#binding client and address
client,address = server.accept()
print (&quot;CLIENT IS CONNECTED. CLIENT'S ADDRESS ->&quot;,address)
print (&quot;\n-----WAITING FOR PUBLIC KEY &amp; PUBLIC KEY HASH-----\n&quot;)

#client's message(Public Key)
getpbk = client.recv(2048)

#conversion of string to KEY
server_public_key = RSA.importKey(getpbk)

#hashing the public key in server side for validating the hash from client
hash_object = hashlib.sha1(getpbk)
hex_digest = hash_object.hexdigest()

if getpbk != &quot;&quot;:
    print (getpbk)
    client.send(&quot;YES&quot;)
    gethash = client.recv(1024)
    print (&quot;\n-----HASH OF PUBLIC KEY----- \n&quot;+gethash)
if hex_digest == gethash:
    # creating session key
    key_128 = os.urandom(16)
    #encrypt CTR MODE session key
    en = AES.new(key_128,AES.MODE_CTR,counter = lambda:key_128)
    encrypto = en.encrypt(key_128)
    #hashing sha1
    en_object = hashlib.sha1(encrypto)
    en_digest = en_object.hexdigest()

    print (&quot;\n-----SESSION KEY-----\n&quot;+en_digest)

    #encrypting session key and public key
    E = server_public_key.encrypt(encrypto,16)
    print (&quot;\n-----ENCRYPTED PUBLIC KEY AND SESSION KEY-----\n&quot;+str(E))
    print (&quot;\n-----HANDSHAKE COMPLETE-----&quot;)
    client.send(str(E))
    while True:
        #message from client
        newmess = client.recv(1024)
        #decoding the message from HEXADECIMAL to decrypt the ecrypted version of the message only
        decoded = newmess.decode(&quot;hex&quot;)
        #making en_digest(session_key) as the key
        key = en_digest[:16]
        print (&quot;\nENCRYPTED MESSAGE FROM CLIENT -> &quot;+newmess)
        #decrypting message from the client
        ideaDecrypt = IDEA.new(key, IDEA.MODE_CTR, counter=lambda: key)
        dMsg = ideaDecrypt.decrypt(decoded)
        print (&quot;\n**New Message**  &quot;+time.ctime(time.time()) +&quot; > &quot;+dMsg+&quot;\n&quot;)
        mess = raw_input(&quot;\nMessage To Client -> &quot;)
        if mess != &quot;&quot;:
            ideaEncrypt = IDEA.new(key, IDEA.MODE_CTR, counter=lambda : key)
            eMsg = ideaEncrypt.encrypt(mess)
            eMsg = eMsg.encode(&quot;hex&quot;).upper()
            if eMsg != &quot;&quot;:
                print (&quot;ENCRYPTED MESSAGE TO CLIENT-> &quot; + eMsg)
            client.send(eMsg)
    client.close()
else:
    print (&quot;\n-----PUBLIC KEY HASH DOESNOT MATCH-----\n&quot;)

```



## Client side Implementation


```
import time
import socket
import threading
import hashlib
import itertools
import sys
from Crypto import Random
from Crypto.PublicKey import RSA
from CryptoPlus.Cipher import IDEA

#animating loading
done = False
def animate():
    for c in itertools.cycle(['....','.......','..........','............']):
        if done:
            break
        sys.stdout.write('\rCONFIRMING CONNECTION TO SERVER '+c)
        sys.stdout.flush()
        time.sleep(0.1)

#public key and private key
random_generator = Random.new().read
key = RSA.generate(1024,random_generator)
public = key.publickey().exportKey()
private = key.exportKey()

#hashing the public key
hash_object = hashlib.sha1(public)
hex_digest = hash_object.hexdigest()

#Setting up socket
server = socket.socket(socket.AF_INET,socket.SOCK_STREAM)

#host and port input user
host = raw_input(&quot;Server Address To Be Connected -> &quot;)
port = int(input(&quot;Port of The Server -> &quot;))
#binding the address and port
server.connect((host, port))
# printing &quot;Server Started Message&quot;
thread_load = threading.Thread(target=animate)
thread_load.start()

time.sleep(4)
done = True

def send(t,name,key):
    mess = raw_input(name + &quot; : &quot;)
    key = key[:16]
    #merging the message and the name
    whole = name+&quot; : &quot;+mess
    ideaEncrypt = IDEA.new(key, IDEA.MODE_CTR, counter=lambda : key)
    eMsg = ideaEncrypt.encrypt(whole)
    #converting the encrypted message to HEXADECIMAL to readable
    eMsg = eMsg.encode(&quot;hex&quot;).upper()
    if eMsg != &quot;&quot;:
        print (&quot;ENCRYPTED MESSAGE TO SERVER-> &quot;+eMsg)
    server.send(eMsg)
def recv(t,key):
    newmess = server.recv(1024)
    print (&quot;\nENCRYPTED MESSAGE FROM SERVER-> &quot; + newmess)
    key = key[:16]
    decoded = newmess.decode(&quot;hex&quot;)
    ideaDecrypt = IDEA.new(key, IDEA.MODE_CTR, counter=lambda: key)
    dMsg = ideaDecrypt.decrypt(decoded)
    print (&quot;\n**New Message From Server**  &quot; + time.ctime(time.time()) + &quot; : &quot; + dMsg + &quot;\n&quot;)

while True:
    server.send(public)
    confirm = server.recv(1024)
    if confirm == &quot;YES&quot;:
        server.send(hex_digest)

    #connected msg
    msg = server.recv(1024)
    en = eval(msg)
    decrypt = key.decrypt(en)
    # hashing sha1
    en_object = hashlib.sha1(decrypt)
    en_digest = en_object.hexdigest()

    print (&quot;\n-----ENCRYPTED PUBLIC KEY AND SESSION KEY FROM SERVER-----&quot;)
    print (msg)
    print (&quot;\n-----DECRYPTED SESSION KEY-----&quot;)
    print (en_digest)
    print (&quot;\n-----HANDSHAKE COMPLETE-----\n&quot;)
    alais = raw_input(&quot;\nYour Name -> &quot;)

    while True:
        thread_send = threading.Thread(target=send,args=(&quot;------Sending Message------&quot;,alais,en_digest))
        thread_recv = threading.Thread(target=recv,args=(&quot;------Recieving Message------&quot;,en_digest))
        thread_send.start()
        thread_recv.start()

        thread_send.join()
        thread_recv.join()
        time.sleep(0.5)
    time.sleep(60)
    server.close()

```



#### Remarks


**Language Used:** Python 2.7 (Download Link: [https://www.python.org/downloads/](https://www.python.org/downloads/) )

**Library Used:**

***PyCrypto** (Download Link: [https://pypi.python.org/pypi/pycrypto](https://pypi.python.org/pypi/pycrypto) )

***PyCryptoPlus** (Download Link: [https://github.com/doegox/python-cryptoplus](https://github.com/doegox/python-cryptoplus) )

**Library Installation:**

**PyCrypto:** Unzip the file. Go to the directory and open terminal for linux(alt+ctrl+t) and
CMD(shift+right click+select command prompt open here) for windows. After that write python setup.py install (Make Sure Python Environment is set properly in Windows OS)

**PyCryptoPlus:** Same as the last library.

**Tasks Implementation:**
The task is separated into two parts. One is handshake process and another one is communication process.
Socket Setup:

<li>
<p>As the creating public and private keys as well as hashing the public key, we need
to setup the socket now. For setting up the socket, we need to import another module with “import socket” and connect(for client) or bind(for server) the IP address and the port with the socket getting from the user.</p>
**----------Client Side----------**
<pre><code>  server = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
  host = raw_input(&quot;Server Address To Be Connected -> &quot;)
  port = int(input(&quot;Port of The Server -> &quot;))
  server.connect((host, port))
</code></pre>
**----------Server Side---------**
<pre><code>  try:
  #setting up socket
  server = socket.socket(socket.AF_INET,socket.SOCK_STREAM)     
  server.bind((host,port))
  server.listen(5)
  except BaseException: print &quot;-----Check Server Address or Port-----&quot;
</code></pre>
**“ socket.AF_INET,socket.SOCK_STREAM” will allow us to use accept() function and messaging fundamentals. Instead of it, we can use “ socket.AF_INET,socket.SOCK_DGRAM” also but that time we will have to use setblocking(value) .**
</li>

**Handshake Process:**

<li>(CLIENT)The first task is to create public and private key. To create the private
and public key, we have to import some modules. They are : from Crypto import Random and from Crypto.PublicKey import RSA. To create the keys, we have to write few simple lines of codes:</li>

```
random_generator = Random.new().read
        key = RSA.generate(1024,random_generator) 
        public = key.publickey().exportKey()

```

random_generator is derived from “**from Crypto import Random**” module. Key is derived from “**from Crypto.PublicKey import RSA**” which will create a private key, size of 1024 by generating random characters. Public is exporting public key from previously generated private key.

<li>
(CLIENT)After creating the public and private key, we have to hash the public key to send over to the server using SHA-1 hash. To use the SHA-1 hash we need to import another module by writing “import hashlib” .To hash the public key we have write two lines of code:
<pre><code>  hash_object = hashlib.sha1(public) 
  hex_digest = hash_object.hexdigest()
</code></pre>
</li>

Here hash_object and hex_digest is our variable. After this, client will send hex_digest and public to the server and Server will verify them by comparing the hash got from client and new hash of the public key. If the new hash and the hash from the client matches, it will move to next procedure. As the public sent from the client is in form of string, it will not be able to be used as key in the server
side. To prevent this and converting string public key to rsa public key, we need to write `server_public_key = RSA.importKey(getpbk)` ,here getpbk is the public key from the client.

<li>
(SERVER)The next step is to create a session key. Here, I have used “os” module to create a random key “key = os.urandom(16)” which will give us a 16bit long key and after that I have encrypted that key in “AES.MODE_CTR” and hash it again with SHA-1:
<pre><code> #encrypt CTR MODE session key
 en = AES.new(key_128,AES.MODE_CTR,counter = lambda:key_128) encrypto = en.encrypt(key_128)
 #hashing sha1
 en_object = hashlib.sha1(encrypto)
 en_digest = en_object.hexdigest()
</code></pre>
</li>

So the en_digest will be our session key.

<li>
<p>(SERVER) For the final part of the handshake process is to encrypt the public key got from the client and the session key created in
server side.</p>
<pre><code> #encrypting session key and public key
 E = server_public_key.encrypt(encrypto,16)
</code></pre>
</li>

After encrypting, server will send the key to the client as string.

<li>
<p>(CLIENT) After getting the encrypted string of (public and session key) from the server, client will decrypt them using Private Key
which was created earlier along with the public key. As the encrypted
(public and session key) was in form of string, now we have to get it
back as a key by using eval() . If the decryption is done, the
handshake process is completed also as both sides confirms that they
are using same keys. To decrypt:</p>
<pre><code> en = eval(msg)
 decrypt = key.decrypt(en)
 # hashing sha1
 en_object = hashlib.sha1(decrypt) en_digest = en_object.hexdigest()
</code></pre>
</li>

I have used the SHA-1 here so that it will be readable in the output.

**Communication Process:**

For communication process, we have to use the session key from both side as the KEY for IDEA encryption MODE_CTR. Both side will encrypt and decrypt messages with IDEA.MODE_CTR using the session key.

<li>
<p>(Encryption) For IDEA encryption, we need key of 16bit in size and counter as must callable. Counter is mandatory in MODE_CTR. The session key that we encrypted and hashed is now size of 40 which will exceed the limit key of the IDEA encryption. Hence, we need to reduce the size of the session key. For reducing, we can use normal python built in function string[value:value]. Where the value can be any value according to the choice of the user. In our case, I have done “key[:16]”
where it will take from 0 to 16 values from the key. This conversion could be done in many ways like key[1:17] or key[16:]. Next part is to create new IDEA encryption function by writing IDEA.new() which will take 3 arguments for processing. The first argument will be KEY,second argument will be the mode of the IDEA encryption (in our case, IDEA.MODE_CTR) and the third argument will be the counter= which is a must callable function. The counter= will hold a size of of string which will be returned by the function. To define the counter= , we must have to use a reasonable values. In this case, I have used the size of the KEY by defining lambda. Instead of using lambda, we could use Counter.Util which generates random value for counter= . To use Counter.Util, we need to import counter module from crypto. Hence, the code will be:</p>
<pre><code>  ideaEncrypt = IDEA.new(key, IDEA.MODE_CTR, counter=lambda : key)
</code></pre>
</li>

Once defining the “ideaEncrypt” as our IDEA encryption variable, we can use the built in encrypt function to encrypt any message.

```
eMsg = ideaEncrypt.encrypt(whole)
#converting the encrypted message to HEXADECIMAL to readable eMsg =         
eMsg.encode(&quot;hex&quot;).upper()

```

In this code segment, whole is the message to be encrypted and eMsg is the encrypted message. After encrypting the message, I have converted it into HEXADECIMAL to make readable and upper() is the built in function to make the characters uppercase. After that, this encrypted message will be sent to the opposite station for decryption.

- **(Decryption)**

To decrypt the encrypted messages, we will need to create another encryption variable by using the same arguments and same key but this time the variable will decrypt the encrypted messages. The code for this same as the last time. However, before decrypting the messages, we need to decode the message from hexadecimal because in our encryption part, we encoded the encrypted message in hexadecimal to make readable. Hence, the whole code will be:

```
decoded = newmess.decode(&quot;hex&quot;)
ideaDecrypt = IDEA.new(key, IDEA.MODE_CTR, counter=lambda: key) 
dMsg = ideaDecrypt.decrypt(decoded)

```

These processes will be done in both server and client side for encrypting and decrypting.


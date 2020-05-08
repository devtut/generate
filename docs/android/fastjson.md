---
metaTitle: "Android - Fastjson"
description: "Parsing JSON with Fastjson, Convert the data of type Map to JSON String"
---

# Fastjson


**Fastjson** is a Java library that can be used to convert Java Objects into their JSON representation. It can also be used to convert a JSON string to an equivalent Java object.

**Fastjson Features:**

Provide best performance in server side and android client

Provide simple `toJSONString()` and `parseObject()` methods to convert Java objects to JSON and vice-versa

Allow pre-existing unmodifiable objects to be converted to and from JSON

Extensive support of Java Generics



## Parsing JSON with Fastjson


You can look at example in [Fastjson library](https://github.com/alibaba/fastjson/wiki/Samples-DataBind)

**Encode**

```java
import com.alibaba.fastjson.JSON;

Group group = new Group();
group.setId(0L);
group.setName("admin");

User guestUser = new User();
guestUser.setId(2L);
guestUser.setName("guest");

User rootUser = new User();
rootUser.setId(3L);
rootUser.setName("root");

group.addUser(guestUser);
group.addUser(rootUser);

String jsonString = JSON.toJSONString(group);

System.out.println(jsonString);

```

**Output**

```java
{"id":0,"name":"admin","users":[{"id":2,"name":"guest"},{"id":3,"name":"root"}]}

```

**Decode**

```java
String jsonString = ...;
Group group = JSON.parseObject(jsonString, Group.class);

```

**Group.java**

```java
public class Group {

    private Long id;
    private String name;
    private List<User> users = new ArrayList<User>();

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<User> getUsers() {
        return users;
    }

    public void setUsers(List<User> users) {
        this.users = users;
    }

    public void addUser(User user) {
        users.add(user);
    }
}

```

**User.java**

```java
public class User {

    private Long   id;
    private String name;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}

```



## Convert the data of type Map to JSON String


**Code**

```

   Group group = new Group();  
    group.setId(1);  
    group.setName("Ke");  
      
    User user1 = new User();  
    user1.setId(2);  
    user1.setName("Liu");  

    User user2 = new User();  
    user2.setId(3);  
    user2.setName("Yue");  
    group.getList().add(user1);  
    group.getList().add(user2);  
      
    Map<Integer, Object> map = new HashMap<Integer,Object>();  
    map.put(1, "No.1");  
    map.put(2, "No.2");  
    map.put(3, group.getList());  
      
    String jsonString = JSON.toJSONString(map);  
    System.out.println(jsonString);  

```

**Output**

```java
{1:"No.1",2:"No.2",3:[{"id":2,"name":"Liu"},{"id":3,"name":"Yue"}]} 

```



#### Syntax


- Object parse(String text)
- JSONObject parseObject(String text)
- T parseObject(String text, Class<T> clazz)
- JSONArray parseArray(String text)
- <T> List<T> parseArray(String text, Class<T> clazz)
- String toJSONString(Object object)
- String toJSONString(Object object, boolean prettyFormat)
- Object toJSON(Object javaObject)


---
metaTitle: "Spring Boot - Spring boot + JPA + mongoDB"
description: "Customer Controller, Customer Repository, CRUD operation in MongoDB using JPA, pom.xml, Insert data using rest client : POST method, Get Request URL, Get request result:"
---

# Spring boot + JPA + mongoDB



## Customer Controller


```java
package org.bookmytickets.controller;

import java.util.List;

import org.bookmytickets.model.Customer;
import org.bookmytickets.repository.CustomerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/customer")
public class CustomerController {

    @Autowired
    private CustomerRepository repository;
    
    @GetMapping("")
    public List<Customer> selectAll(){
        List<Customer> customerList = repository.findAll();
        return customerList;
    }
    
    @GetMapping("/{id}")
    public List<Customer> getSpecificCustomer(@PathVariable String id){
        return repository.findById(id);
    }
    
    @GetMapping("/search/lastName/{lastName}")
    public List<Customer> searchByLastName(@PathVariable String lastName){
        return repository.findByLasttName(lastName);
    }

    @GetMapping("/search/firstname/{firstname}")
    public List<Customer> searchByFirstName(@PathVariable String firstName){
        return repository.findByFirstName(firstName);
    }
    
    @PostMapping("")
    public void insert(@RequestBody Customer customer) {
        repository.save(customer);
    }

    @PatchMapping("/{id}")
    public void update(@RequestParam String id, @RequestBody Customer customer) {
        Customer oldCustomer = repository.finedById(id);
        if(customer.getFirstName() != null) {
            oldCustomer.setFristName(customer.getFirstName());
        }
        if(customer.getLastName() != null) {
            oldCustomer.setLastName(customer.getLastName());
        }
        repository.save(oldCustomer);
    }
    
    @DeleteMapping("/{id}")
    public void delete(@RequestParam String id) {
        Customer deleteCustomer = repository.findById(id);
        repository.delete(deleteCustomer);
    }
}

```



## Customer Repository


```java
package org.bookmytickets.repository;

import java.util.List;

import org.bookmytickets.model.Customer;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface CustomerRepository extends MongoRepository<Customer, String> {
    public Customer findByFirstName(String firstName);
    public List<Customer> findByLastName(String lastName);
}

```



## CRUD operation in MongoDB using JPA


**Customer Model**

```java
package org.bookmytickets.model;

import org.springframework.data.annotation.Id;

public class Customer {

    @Id
    private String id;
    private String firstName;
    private String lastName;

    public Customer() {}

    public Customer(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }
    
    public Customer(String id,String firstName, String lastName) {
        this.id = id;
        this.firstName = firstName;
        this.lastName = lastName;
    }

    @Override
    public String toString() {
        return String.format(
                "Customer[id=%s, firstName='%s', lastName='%s']",
                id, firstName, lastName);
    }
    
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

}

```



## pom.xml


Please add below dependencies in pom.xml file:

```java
<dependencies>
        
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>
        
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-mongodb</artifactId>
        </dependency>
        
</dependencies>

```



## Insert data using rest client : POST method


For testing our application, I'm using advance rest client which is chrome extension:

So, here is the snapshot for inserting the data:

[<img src="http://i.stack.imgur.com/cSfBD.png" alt="enter image description here" />](http://i.stack.imgur.com/cSfBD.png)



## Get Request URL


[<img src="http://i.stack.imgur.com/gFEZ5.png" alt="enter image description here" />](http://i.stack.imgur.com/gFEZ5.png)



## Get request result:


[<img src="http://i.stack.imgur.com/RJ1B8.png" alt="enter image description here" />](http://i.stack.imgur.com/RJ1B8.png)


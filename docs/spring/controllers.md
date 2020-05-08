---
metaTitle: "Spring Boot - Controllers"
description: "Spring boot rest controller."
---

# Controllers


In this section i will add an example for the Spring boot rest controller with Get and post request.



## Spring boot rest controller.


> 
<p>In this example i will show how to formulate an rest controller to get
and post data to the database using JPA with the most ease and least
code.</p>


In this example we will refer to the data table named buyerRequirement .

**BuyingRequirement.java**

@Entity
@Table(name = "BUYINGREQUIREMENTS")
@NamedQueries({
@NamedQuery(name = "BuyingRequirement.findAll", query = "SELECT b FROM BuyingRequirement b")})
public class BuyingRequirement extends Domain implements Serializable {
private static final long serialVersionUID = 1L;

```java
@Column(name = "PRODUCT_NAME", nullable = false)
private String productname;

@Column(name = "NAME", nullable = false)
private String name;

@Column(name = "MOBILE", nullable = false)
private String mobile;

@Column(name = "EMAIL", nullable = false)
private String email;

@Column(name = "CITY")
private String city;


public BuyingRequirement() {
}


public String getProductname() {
    return productname;
}

public void setProductname(String productname) {
    this.productname = productname;
}

public String getName() {
    return name;
}

public void setName(String name) {
    this.name = name;
}

public String getMobile() {
    return mobile;
}

public void setMobile(String mobile) {
    this.mobile = mobile;
}

public String getEmail() {
    return email;
}

public void setEmail(String email) {
    this.email = email;
}

public String getCity() {
    return city;
}

public void setCity(String city) {
    this.city = city;
}

```

}

This is the entity class which includes the parameter referring to columns in byuingRequirement table and their getters and setters.

**IBuyingRequirementsRepository.java(JPA interface)**

```java
@Repository
@RepositoryRestResource
public interface IBuyingRequirementsRepository extends JpaRepository<BuyingRequirement, UUID> {
   // Page<BuyingRequirement> findAllByOrderByCreatedDesc(Pageable pageable);
    Page<BuyingRequirement> findAllByOrderByCreatedDesc(Pageable pageable);
    Page<BuyingRequirement> findByNameContainingIgnoreCase(@Param("name") String name, Pageable pageable);
}

```

**BuyingRequirementController.java**

```java
@RestController
@RequestMapping("/api/v1")
public class BuyingRequirementController {

    @Autowired
    IBuyingRequirementsRepository iBuyingRequirementsRepository;
    Email email = new Email();

    BuyerRequirementTemplate buyerRequirementTemplate = new BuyerRequirementTemplate();

    private String To = "support@pharmerz.com";
    // private String To = "amigujarathi@gmail.com";
    private String Subject = "Buyer Request From Pharmerz ";

    @PostMapping(value = "/buyingRequirement")
    public ResponseEntity<BuyingRequirement> CreateBuyingRequirement(@RequestBody BuyingRequirement buyingRequirements) {

        String productname = buyingRequirements.getProductname();
        String name = buyingRequirements.getName();
        String mobile = buyingRequirements.getMobile();
        String emails = buyingRequirements.getEmail();
        String city = buyingRequirements.getCity();
        if (city == null) {
            city = "-";
        }

        String HTMLBODY = buyerRequirementTemplate.template(productname, name, emails, mobile, city);

        email.SendMail(To, Subject, HTMLBODY);


        iBuyingRequirementsRepository.save(buyingRequirements);
        return new ResponseEntity<BuyingRequirement>(buyingRequirements, HttpStatus.CREATED);
    }


    @GetMapping(value = "/buyingRequirements")
    public Page<BuyingRequirement> getAllBuyingRequirements(Pageable pageable) {

        Page requirements = iBuyingRequirementsRepository.findAllByOrderByCreatedDesc(pageable);
        return requirements;
    }

    @GetMapping(value = "/buyingRequirmentByName/{name}")
    public Page<BuyingRequirement> getByName(@PathVariable String name,Pageable pageable){
        Page buyersByName = iBuyingRequirementsRepository.findByNameContainingIgnoreCase(name,pageable);

        return buyersByName;
    }
}

```

It includes there method

1. Post method which post data to the database .
1. Get method which fetch all records from buyingRequirement table.
1. This is also a get method which will find the buying requirement by the person's name.


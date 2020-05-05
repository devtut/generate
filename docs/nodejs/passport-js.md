---
metaTitle: "Node.js - passport.js"
description: "Example of LocalStrategy in passport.js"
---

# passport.js


Passport is a popular authorisation module for node. In simple words it handles all the authorisation requests on your app by users. Passport supports over 300 strategies so that you can easily integrate login with Facebook / Google or any other social network using it. The strategy that we will discuss here is the Local where you authenticate an user using your own database of registered users( using username and password).



## Example of LocalStrategy in passport.js


```js
var passport = require('passport');
var LocalStrategy = require('passport-local').Strategy;

passport.serializeUser(function(user, done) { //In serialize user you decide what to store in the session. Here I'm storing the user id only.
  done(null, user.id);
});

passport.deserializeUser(function(id, done) { //Here you retrieve all the info of the user from the session storage using the user id stored in the session earlier using serialize user.
  db.findById(id, function(err, user) {
    done(err, user);
    });
});

passport.use(new LocalStrategy(function(username, password, done) {
    db.findOne({'username':username},function(err,student){
        if(err)return done(err,{message:message});//wrong roll_number or password; 
        var pass_retrieved = student.pass_word;
        bcrypt.compare(password, pass_retrieved, function(err3, correct) {
          if(err3){
            message = [{"msg": "Incorrect Password!"}];
            return done(null,false,{message:message});  // wrong password
          }       
          if(correct){
              return done(null,student);
          } 
        });
    });
}));

app.use(session({ secret: 'super secret' })); //to make passport remember the user on other pages too.(Read about session store. I used express-sessions.)
app.use(passport.initialize());
app.use(passport.session());

app.post('/',passport.authenticate('local',{successRedirect:'/users' failureRedirect: '/'}),
    function(req,res,next){
});

```


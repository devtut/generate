---
metaTitle: "React Native - Integration with Firebase for Authentication"
description: "React Native - ListView with Firebase, Authentication In React Native Using Firebase"
---

# Integration with Firebase for Authentication


//Replace firebase values with your app api values
import firebase from 'firebase';

componentWillMount() {
firebase.initializeApp({
apiKey: "yourAPIKey",
authDomain: "authDomainNAme",
databaseURL: "yourDomainBaseURL",
projectId: "yourProjectID",
storageBucket: "storageBUcketValue",
messagingSenderId: "senderIdValue"
});
firebase.auth().signInWithEmailAndPassword(email, password)
.then(this.onLoginSuccess)
})
}



## React Native - ListView with Firebase


This is what I do when I'm working with Firebase and I want to use ListView.

Use a parent component to retrieve the data from Firebase (Posts.js):

**Posts.js**

```js
import PostsList from './PostsList';

class Posts extends Component{
    constructor(props) {
        super(props);
        this.state = {
            posts: []
        }
    }
    
    componentWillMount() {
        firebase.database().ref('Posts/').on('value', function(data) {
            this.setState({ posts: data.val() });
        });
    }

    render() {
        return <PostsList posts={this.state.posts}/>
    }
}

```

**PostsList.js**

```js
class PostsList extends Component {
    constructor(props) {
        super(props);
        this.state = {
            dataSource: new ListView.DataSource({
                rowHasChanged: (row1, row2) => row1 !== row2
            }),
        }
    }

    getDataSource(posts: Array<any>): ListView.DataSource {
        if(!posts) return;
        return this.state.dataSource.cloneWithRows(posts);
    }

    componentDidMount() {
        this.setState({dataSource: this.getDataSource(this.props.posts)});
    }

    componentWillReceiveProps(props) {
        this.setState({dataSource: this.getDataSource(props.posts)});
    }

    renderRow = (post) => {
        return (
            <View>
                <Text>{post.title}</Text>
                <Text>{post.content}</Text>
            </View>
        );
    }

    render() {
        return(
            <ListView
                dataSource={this.state.dataSource}
                renderRow={this.renderRow}
                enableEmptySections={true}
            />
        );
    }
}

```

I want to point out that in `Posts.js`, I'm not importing `firebase` because you only need to import it once, in the main component of your project (where you have the navigator) and use it anywhere.

**This is the solution someone suggested in a question I asked when I was struggling with ListView. I thought it would be nice to share it.**

Source: [[http://stackoverflow.com/questions/38414289/react-native-listview-not-rendering-data-from-firebase][1]](http://stackoverflow.com/questions/38414289/react-native-listview-not-rendering-data-from-firebase%5D%5B1%5D)



## Authentication In React Native Using Firebase


Replace firebase values with your app api values:

```js
import firebase from 'firebase';
componentWillMount() {
firebase.initializeApp({
  apiKey: "yourAPIKey",
  authDomain: "authDomainNAme",
  databaseURL: "yourDomainBaseURL",
  projectId: "yourProjectID",
  storageBucket: "storageBUcketValue",
  messagingSenderId: "senderIdValue"
});
    firebase.auth().signInWithEmailAndPassword(email, password)
  .then(this.onLoginSuccess)
  .catch(() => {
    firebase.auth().createUserWithEmailAndPassword(email, password)
      .then(this.onLoginSuccess)
      .catch(this.onLoginFail)
  })
}

```


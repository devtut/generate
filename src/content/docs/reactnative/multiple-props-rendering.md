---
title: "Multiple props rendering"
description: "render multiple variables"
---

## render multiple variables


For rendering multiple props or variables we can use **``**.

```

 render() {
    let firstName = 'test';
    let lastName = 'name';
    return (
      <View style={styles.container}>
        <Text>{`${firstName} ${lastName}` } </Text>
      </View>
    );
  }

```

Output:
test name
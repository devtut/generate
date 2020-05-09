---
metaTitle: "React Native - ESLint in react-native"
description: "How to start"
---

# ESLint in react-native


This is the topic for ESLint rules explanation for react-native.



## How to start


It's highly recommended to use ESLint in your project on react-native. ESLint is a tool for code validation using specific rules provided by community.

For react-native you can use rulesets for javascript, react and react-native.

Common ESLint rules with motivation and explanations for javascript you can find here: [https://github.com/eslint/eslint/tree/master/docs/rules](https://github.com/eslint/eslint/tree/master/docs/rules) .
You can simply add ready ruleset from ESLint developers by adding in your .eslintr.json to 'extends' node 'eslint:recommended'. ( "extends": ["eslint:recommended"] )
More about ESLint configuring you can read here: [http://eslint.org/docs/developer-guide/development-environment](http://eslint.org/docs/developer-guide/development-environment) . It's recommended to read full doc about this extremely useful tool.

Next, full docs about rules for ES Lint react plugin you can find here:
[https://github.com/yannickcr/eslint-plugin-react/tree/master/docs/rules](https://github.com/yannickcr/eslint-plugin-react/tree/master/docs/rules) .
Important note: not all rules from react are relative to react-native. For example: react/display-name and react/no-unknown-property for example. Another rules are 'must have' for every project on react-native, such as react/jsx-no-bind and react/jsx-key.

Be very careful with choosing your own ruleset.

And finaly, there is a plugin explicidly for react-native:
[https://github.com/intellicode/eslint-plugin-react-native](https://github.com/intellicode/eslint-plugin-react-native)
Note: If you split your styles in separate file, rule react-native/no-inline-styles will not work.

For correct working of this tool in react-native env you might need to set value or 'env' in your config to this:
"env": {
"browser": true,
"es6": true,
"amd": true
},

ESLint is a key tool for development of high quality product.


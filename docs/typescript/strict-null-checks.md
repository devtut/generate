---
metaTitle: "TypeScript - Strict null checks"
description: "Strict null checks in action, Non-null assertions"
---

# Strict null checks




## Strict null checks in action


By default, all types in TypeScript allow `null`:

```ts
function getId(x: Element) {
  return x.id;
}
getId(null);  // TypeScript does not complain, but this is a runtime error.

```

TypeScript 2.0 adds support for strict null checks. If you set `--strictNullChecks` when running `tsc` (or set this flag in your `tsconfig.json`), then types no longer permit `null`:

```ts
function getId(x: Element) {
  return x.id;
}
getId(null);  // error: Argument of type 'null' is not assignable to parameter of type 'Element'.

```

You must permit `null` values explicitly:

```ts
function getId(x: Element|null) {
  return x.id;  // error TS2531: Object is possibly 'null'.
}
getId(null);

```

With a proper guard, the code type checks and runs correctly:

```ts
function getId(x: Element|null) {
  if (x) {
    return x.id;  // In this branch, x's type is Element
  } else {
    return null;  // In this branch, x's type is null.
  }
}
getId(null);

```



## Non-null assertions


The non-null assertion operator, `!`, allows you to assert that an expression isn't `null` or `undefined` when the TypeScript compiler can't infer that automatically:

```ts
type ListNode = { data: number; next?: ListNode; };

function addNext(node: ListNode) {
    if (node.next === undefined) {
        node.next = {data: 0};
    }
}

function setNextValue(node: ListNode, value: number) {
    addNext(node);
    
    // Even though we know `node.next` is defined because we just called `addNext`,
    // TypeScript isn't able to infer this in the line of code below:
    // node.next.data = value;
    
    // So, we can use the non-null assertion operator, !,
    // to assert that node.next isn't undefined and silence the compiler warning
    node.next!.data = value;
}

```


---
metaTitle: "React Native - ListView"
description: "Simple Example"
---

# ListView



## Simple Example


ListView - A core component designed for efficient display of vertically scrolling lists of changing data. The minimal API is to create a ListView.DataSource, populate it with a simple array of data blobs, and instantiate a ListView component with that data source and a renderRow callback which takes a blob from the data array and returns a renderable component.

Minimal example:

```js
getInitialState: function() {
  var ds = new ListView.DataSource({rowHasChanged: (r1, r2) => r1 !== r2});
  return {
    dataSource: ds.cloneWithRows(['row 1', 'row 2']),
  };
},

render: function() {
  return (
    <ListView
      dataSource={this.state.dataSource}
      renderRow={(rowData) => <Text>{rowData}</Text>}
    />
  );
},

```

ListView also supports more advanced features, including sections with sticky section headers, header and footer support, callbacks on reaching the end of the available data (onEndReached) and on the set of rows that are visible in the device viewport change (onChangeVisibleRows), and several performance optimizations.

There are a few performance operations designed to make ListView scroll smoothly while dynamically loading potentially very large (or conceptually infinite) data sets:

<li>Only re-render changed rows - the rowHasChanged function provided to
the data source tells the ListView if it needs to re-render a row
because the source data has changed - see ListViewDataSource for more
details.</li>
<li>Rate-limited row rendering - By default, only one row is rendered per
event-loop (customizable with the pageSize prop). This breaks up the
work into smaller chunks to reduce the chance of dropping frames
while rendering rows.</li>


module.exports = {
    title: "VuePress",
    description: "Vue-powered Static Site Generator",
    themeConfig: {
      repo: "devtut/devtut.github.io",
      editLinks: true,
      // docsDir: '/guide/',
      smoothScroll: true,
      editLinkText: "Edit this page on GitHub",
      lastUpdated: "Last Updated",
      nav: [
        {
          text: "Guide",
          link: "/guide/",
        },
      ],
      sidebar: {
        "/guide/": [
          {
            title: "Guide",
            collapsable: false,
            sidebarDepth: 2,
            children: [
              ["", "DEfault"], 
              "some"
            ],
          },
        ],
      },
    },
  };
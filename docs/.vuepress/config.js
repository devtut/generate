module.exports = {
    title: "DevTut",
    description: "Vue-powered Static Site Generator",
    themeConfig: {
      repo: "devtut/generate",
      editLinks: true,
      docsDir: 'docs',
      smoothScroll: true,
      editLinkText: "Edit this page on GitHub",
      lastUpdated: "Last Updated",
      nav: [
        {
          text: "Python",
          link: "/python/",
        },
      ],
      sidebar: {
        "/python/": [
          {
            title: "Python",
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
    plugins: [
      [
        '@vuepress/google-analytics',
        {
          'ga': 'UA-145300963-3'
        }
      ]
    ]
  };
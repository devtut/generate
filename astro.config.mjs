import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import vue from '@astrojs/vue';
import sidebars from './sidebars.json';

// https://astro.build/config
export default defineConfig({
  site: 'https://devtut.github.io',
  integrations: [
    starlight({
      title: 'DevTut',
      tagline: 'Example based programming tutorials for solid developers. Master 45+ programming topics.',
      logo: {
        src: './src/assets/logo.png',
      },
      favicon: '/favicon.ico',
      social: [
        { label: 'GitHub', href: 'https://github.com/devtut/generate', icon: 'github' },
      ],
      sidebar: sidebars,
      customCss: [
        './src/styles/custom.css',
      ],
    }),
    vue(),
  ],
});

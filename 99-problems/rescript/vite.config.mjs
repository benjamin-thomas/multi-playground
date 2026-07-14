import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    includeSource: ['src/**/*.res.mjs'],
    benchmark: {
      include: ['src/**/*_Bench.res.mjs'],
    },
  },
  define: {
    'import.meta.vitest': 'undefined',
  },
});

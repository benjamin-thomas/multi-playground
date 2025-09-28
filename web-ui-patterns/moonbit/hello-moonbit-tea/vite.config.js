import { defineConfig } from 'vite';
import { moonbitPlugin } from './vite-plugin-moonbit.js';

export default defineConfig({
    plugins: [
        moonbitPlugin()
    ],
    logLevel: 'debug',
});

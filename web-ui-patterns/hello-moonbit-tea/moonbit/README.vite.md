# MoonBit Vite Plugin

This document explains how the `vite-plugin-moonbit.js` plugin integrates MoonBit compilation into Vite's development workflow.

## Overview

The plugin automatically compiles MoonBit (`.mbt`) files when:
- The Vite dev server starts
- Any `.mbt` file is modified during development

## How It Works

### 1. Initial Build
When Vite starts, the plugin runs `moon build --target=js` to compile all MoonBit files. If compilation fails, Vite will display the error and refuse to start.

### 2. File Watching
The plugin hooks into Vite's file watcher system to monitor all `.mbt` files in your project. When a file changes:
- The plugin detects the change through `server.watcher`
- Runs `moon build --target=js` again
- On success: triggers a full page reload
- On failure: displays errors in the browser overlay

### 3. Error Display
Compilation errors appear directly in your browser using Vite's error overlay system. This provides:
- Immediate visual feedback when compilation fails
- The full error message from the MoonBit compiler
- A reference to the file that triggered the rebuild

## Plugin Structure

```javascript
export function moonbitPlugin() {
    return {
        name: 'vite-plugin-moonbit',
        
        // Runs during Vite startup
        buildStart() {
            // Initial MoonBit compilation
        },
        
        // Configures the dev server
        configureServer(server) {
            // Sets up file watching for .mbt files
            // Handles compilation and error reporting
        }
    };
}
```

## Key Features

1. **Automatic Compilation**: No need to manually run `moon build` while developing
2. **Hot Reload**: Browser automatically refreshes after successful compilation
3. **Error Overlay**: See compilation errors instantly in the browser
4. **Zero Configuration**: Just add the plugin to your Vite config

## Usage

Simply import and add the plugin to your `vite.config.js`:

```javascript
import { moonbitPlugin } from './vite-plugin-moonbit.js';

export default defineConfig({
    plugins: [
        moonbitPlugin()
    ]
});
```

Then run `npm run dev` and the plugin handles everything else!
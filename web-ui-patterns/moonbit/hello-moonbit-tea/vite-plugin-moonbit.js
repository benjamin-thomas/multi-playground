import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

export function moonbitPlugin() {
    let server;

    async function buildMoonbit() {
        try {
            console.log('[moonbit] Building...');
            const { stdout, stderr } = await execAsync('moon build --target=js');
            if (stdout) console.log('[moonbit]', stdout);
            if (stderr) console.error('[moonbit]', stderr);
            console.log('[moonbit] Build complete');
            return { success: true };
        } catch (error) {
            console.error('[moonbit] Build failed:', error.message);
            // Extract error output from the error object
            const errorOutput = error.stderr || error.message || 'Unknown build error';
            return { success: false, error: errorOutput };
        }
    }

    return {
        name: 'vite-plugin-moonbit',

        async buildStart() {
            console.log('===> [moonbit] Plugin loaded');
            // Initial build
            const result = await buildMoonbit();
            if (!result.success) {
                // Throw error to make Vite display it
                throw new Error(`MoonBit compilation failed:\n\n${result.error}`);
            }
        },

        configureServer(_server) {
            server = _server;

            // Watch for .mbt file changes
            server.watcher.add(['**/*.mbt']);

            server.watcher.on('change', async (file) => {
                if (file.endsWith('.mbt')) {
                    console.log(`===> [moonbit] File changed: ${file}`);
                    const result = await buildMoonbit();

                    if (server) {
                        if (result.success) {
                            // Clear any previous errors
                            server.ws.send({
                                type: 'error',
                                err: null
                            });
                            // Trigger full page reload after successful build
                            server.ws.send({
                                type: 'full-reload',
                                path: '*'
                            });
                        } else {
                            // Send error to browser overlay

                            server.ws.send({
                                type: 'error',
                                err: {
                                    message: `MoonBit compilation failed:\n\n---\n\n${result.error}`,
                                    stack: '',
                                    plugin: 'vite-plugin-moonbit',
                                    id: file,
                                    frame: '',
                                    loc: {
                                        file,
                                        line: 1,
                                        column: 1
                                    }
                                }
                            });
                        }
                    }
                }
            });
        }
    };
}
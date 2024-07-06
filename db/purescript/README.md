Run with:

```sh
spago run
```

Or:

```sh
spago build
node -e "import('./output/Main/index.js').then(m => m.main())
```

NOTE:

It seems like I made a mistake, since calling any of program0, program1, etc. blocks the process (the program exits after roughly 30s)
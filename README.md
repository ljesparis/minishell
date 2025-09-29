# mini-shell 
mini shell generated with chatgpt. I was just porting the code from this repo https://github.com/ljesparis/cli-calculator/ 

# dependencies

python3 
zig 0.15.1

# Build
```shell
zig build-exe interpreter.zig -target wasm32-freestanding -fno-entry --export=js_eval
```

# Run
python3 -m http.server

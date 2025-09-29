# mini-shell 
mini shell generated with chatgpt. I was just porting the code from this repo https://github.com/ljesparis/cli-calculator/ 

https://github.com/user-attachments/assets/74a974e5-6224-4e8d-91fd-1d58f86aee8a

# dependencies

python3 
zig 0.15.1

# Build
```shell
zig build-exe interpreter.zig -target wasm32-freestanding -fno-entry --export=js_eval
```

# Run
python3 -m http.server

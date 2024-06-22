#!/bin/bash
customasm ./main.asm
customasm ./main.asm -p > assembled.txt
customasm ./bad-apple-setup.asm
customasm ./bad-apple-setup.asm -p > bad-apple-setup.txt
cd badapple
cargo build
cd ../emu
cargo build

echo "Build complete!"
echo "Run ./emu/target/debug/emu <rom> to run the emulator. Input doesn't work correctly yet."
echo "Run ./badapple/target/debug/badapple to build the bad apple ROM. This only builds the data part - combine it with bad-apple-setup before running."
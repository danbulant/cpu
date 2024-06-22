# Bad Apple data generator

This generates the data for playing back Bad Apple on the cpu.
It generates format consumed by `bad-apple-setup.asm`.

## Usage

Ffmpeg is needed to build and **run** this project.

Rust compiler needed to build this project.

```sh
cargo build # builds the project
cargo run -- bad-apple.webm
```

Any file that ffmpeg can read can be used as input.
It is converted to 256x256 black and white video, saved as bits of Y coordinate (i.e. rotated 90 degrees if viewed in bit form, such as `xxd -b -c 32`).

Output is stored as `bad-apple.bin`.

To generate the ROM file, concat `bad-apple-setup.asm` (from the folder above) and `bad-apple.bin` together.

```sh
cat bad-apple-setup.asm ./badapple/bad-apple.bin > bad-apple-complete.bin
```

I recommend not trying to use logisim for it - the ROM is ~50MB and logisim will not like it. Furthermore, the simulation is too slow to play the video in a reasonable time - the code needs about 10 instructions per pixel and logisim runs at about 2Hz. The emulator can run at about 20MHz and is mostly limited by rendering speed.

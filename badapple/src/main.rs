use ffmpeg_next::format::{input, Pixel};
use ffmpeg_next::media::Type;
use ffmpeg_next::software::scaling::{flag::Flags};
use ffmpeg_next::util::frame::video::Video;
use std::env;
use std::fs::File;
use std::io::{Seek, SeekFrom, Write};

const WIDTH: u32 = 256;
const HEIGHT: u32 = 256;

fn main() -> Result<(), ffmpeg_next::Error> {
    ffmpeg_next::init().unwrap();

    if let Ok(mut ictx) = input("badapple.webm") {
        let input = ictx
            .streams()
            .best(Type::Video)
            .ok_or(ffmpeg_next::Error::StreamNotFound)?;
        let video_stream_index = input.index();

        let mut output_file = File::create("bad-apple.bin").unwrap();

        let context_decoder = ffmpeg_next::codec::context::Context::from_parameters(input.parameters())?;
        let mut decoder = context_decoder.decoder().video()?;

        // clion doesn't like use for this...
        let mut scaler = ffmpeg_next::software::scaling::context::Context::get(
            // input
            decoder.format(),
            decoder.width(),
            decoder.height(),
            // output
            Pixel::GRAY8,
            WIDTH as u32,
            HEIGHT as u32,
            Flags::BICUBIC
        )?;
        let mut frame_index = 0;

        let mut receive_and_process_decoded_frames =
            |decoder: &mut ffmpeg_next::decoder::Video| -> Result<(), ffmpeg_next::Error> {
                let mut decoded = Video::empty();
                while decoder.receive_frame(&mut decoded).is_ok() {
                    let mut bw_frame = Video::empty();
                    scaler.run(&decoded, &mut bw_frame)?;
                    let data = bw_frame.data(0);
                    let mut frame = vec![0u32;2048];

                    for x in 0..WIDTH {
                        for y in 0..HEIGHT {
                            let pixel = data[(y * WIDTH + x) as usize] > 40;

                            frame[(x*(256/32)+y/32) as usize] =
                                frame[(x*(256/32)+y/32) as usize] << 1 | pixel as u32;
                        }
                    }
                    
                    let iter = frame.iter().map(|&x| x.to_be_bytes());
                    for byte in iter {
                        output_file.write_all(&byte).unwrap();
                    }

                    frame_index += 1;
                    dbg!(frame_index);
                }
                Ok(())
            };

        for (stream, packet) in ictx.packets() {
            if stream.index() == video_stream_index {

                decoder.send_packet(&packet)?;
                receive_and_process_decoded_frames(&mut decoder)?;
            }
        }
        decoder.send_eof()?;
        receive_and_process_decoded_frames(&mut decoder)?;
    }

    Ok(())
}
module FlipperZeroResearchUtils.Tests.ProtocolsTests

open System
open System.IO
open FlipperZeroResearchUtils
open FlipperZeroResearchUtils.Common
open FlipperZeroResearchUtils.Protocols
open FsUnit.Xunit
open Xunit

let getFilePath name =
    Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "assets", name)

[<Fact>]
let ``GangQi should be decoded correctly`` () =
    let decoder = GangQiDecoder()
    let path = getFilePath "GangQi.sub"
    let expected = GangQi.Create(0x1EDA77818UL)

    let result = path |> Decoder.decode decoder

    result |> should haveLength 12

    for item in result do
        item |> should equal expected

// TODO: Finish test when structure and how dynamic part is encoded will be finished
[<Fact>]
let ``Ditec Dynamic should be decoded correctly`` () =
    let decoder = DitecDynamicDecoder()
    let path = getFilePath "DitecDynamic.sub"
    let expected = "001001000110001010111001000001001000011111000101010110"

    let result = path |> Decoder.decode decoder

    result |> should haveLength 6

    for item in result do
        item.ToString() |> should equal expected

/// Durations for a single synthetic frame: a leading gap, then one high per bit (long for 1, short
/// for 0) followed by that bit's low, except on the last bit whose low is the closing gap. Pulse
/// widths come from the config so only the gap varies; the gap magnitudes are deliberately literal,
/// since deriving them would make the window tests assert against themselves.
let private frameDurations leadingGap (value: uint64) =
    let decoderConfig = RogerStatic.config.DecoderConfig

    [|
        yield leadingGap

        for bit in RogerStatic.config.PayloadLength - 1 .. -1 .. 0 do
            let high, low =
                if (value >>> bit) &&& 1UL = 1UL then
                    decoderConfig.Long, -decoderConfig.Short
                else
                    decoderConfig.Short, -decoderConfig.Long

            yield high

            if bit > 0 then
                yield low

        yield -9800
    |]

let private decodeDurationsOfFrame durations =
    durations
    |> decodeDurations RogerStatic.config.DecoderConfig
    |> RogerStatic.decode

let private decodeFrame leadingGap value =
    frameDurations leadingGap value |> decodeDurationsOfFrame

[<Fact>]
let ``Roger Static should be decoded correctly`` () =
    let decoder = RogerStaticDecoder()
    let path = getFilePath "RogerStatic.sub"
    let expected = RogerStatic.Create(0x095F223UL)

    let result = path |> Decoder.decode decoder

    result |> should haveLength 113
    result |> List.distinct |> should equal [ expected ]

    expected.ToString()
    |> should equal "Value: 0x095F223; Serial: 0x095F; Button: 0x2; End: 0x23"

[<Fact>]
let ``Roger Static should decode every button of a four button capture`` () =
    let decoder = RogerStaticDecoder()
    let path = getFilePath "RogerStaticFourButtons.sub"

    let expected =
        [
            RogerStatic.Create(0x0DFE101UL), 8
            RogerStatic.Create(0x0DFE202UL), 8
            RogerStatic.Create(0x0DFE404UL), 9
            RogerStatic.Create(0x0DFE808UL), 6
        ]

    path
    |> Decoder.decode decoder
    |> List.countBy id
    |> List.sortBy fst
    |> should equal expected

[<Fact>]
let ``Roger Static should emit nothing for a truncated capture`` () =
    let decoder = RogerStaticDecoder()
    let path = getFilePath "RogerStaticTruncated.sub"

    path |> Decoder.decode decoder |> should be Empty

[<Fact>]
let ``Roger Static should split the payload into serial button and end`` () =
    let distinct = RogerStatic.Create(0x1234567UL)

    distinct.Serial |> should equal 0x1234us
    distinct.Button |> should equal 0x5uy
    distinct.End |> should equal 0x67uy

    let saturated = RogerStatic.Create(0xFFFFFFFUL)

    saturated.Serial |> should equal 0xFFFFus
    saturated.Button |> should equal 0xFuy
    saturated.End |> should equal 0xFFuy

[<Fact>]
let ``Roger Static should reject a payload wider than the configured length`` () =
    (fun () -> RogerStatic.Create(0x10000000UL) |> ignore)
    |> should throw typeof<ArgumentException>

[<Theory>]
[<InlineData(-9500)>]
[<InlineData(-8300)>]
[<InlineData(-10800)>]
let ``Roger Static should accept a gap anywhere inside the configured window`` (leadingGap: int) =
    decodeFrame leadingGap 0x1234567UL
    |> should equal [ RogerStatic.Create(0x1234567UL) ]

[<Fact>]
let ``Roger Static should reject a payload that runs on past the configured length`` () =
    // A burst picked up mid-stream yields 28 clean bits followed by another bit rather than by the
    // closing gap. Those bits are shifted against the real frame, so the payload would be plausible
    // but wrong, and only the termination check rejects it.
    let frame = frameDurations -9500 0x1234567UL

    // Drop the closing gap, then carry on with one more bit before the real gap.
    let overlong =
        Array.append frame[.. frame.Length - 2] [| -1000; 500; -9800 |]

    overlong |> decodeDurationsOfFrame |> should be Empty

[<Theory>]
[<InlineData(-7900)>]
[<InlineData(-11200)>]
let ``Roger Static should ignore a gap outside the configured window`` (leadingGap: int) =
    decodeFrame leadingGap 0x1234567UL |> should be Empty

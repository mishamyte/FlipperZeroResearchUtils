module FlipperZeroResearchUtils.Decoder

open FlipperZeroResearchUtils.Common

let decode (protocol: IProtocolDecoder<'payload>) path =
    SubGhzRawFileParser.parseToDurationsArray path
    |> decodeDurations protocol.Config.DecoderConfig
    |> protocol.Decode

let decodeAndPrint (protocol: IProtocolDecoder<'payload>) path =
    decode protocol path
    |> List.iter (fun payload -> printfn $"%s{payload.ToString()}")

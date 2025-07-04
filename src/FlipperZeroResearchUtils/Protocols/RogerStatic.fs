namespace FlipperZeroResearchUtils.Protocols

open FlipperZeroResearchUtils.Common

module RogerStatic =

    let config =
        {
            DecoderConfig =
                {
                    Short = 500
                    Long = 1000
                    Gap = 9500
                    Delta = { Duration = 250; Gap = 500 }
                }
            PayloadLength = 28
        }

    let decode durations =
        let rec decodePayload remainingDurations (currentBits: BitSequence) =
            if currentBits.Length = config.PayloadLength then
                Some(currentBits), remainingDurations
            else
                match remainingDurations with
                | [] -> None, [] // Payload is not full
                | (ShortHigh | LongHigh) :: next :: rest when next = Unknown -> None, next :: rest // Unknown value after good -> trash
                | ShortHigh :: rest -> decodePayload rest (currentBits.AddZero()) // ShortHigh -> 0
                | LongHigh :: rest -> decodePayload rest (currentBits.AddOne()) // LongHigh -> 1
                | StartBit :: _ -> None, remainingDurations // Unexpected StartBit
                | _ :: rest -> decodePayload rest currentBits // Skip Low values, caz we don't care

        let rec findPayloads remainingDurations payloads =
            match remainingDurations with
            | [] -> List.rev payloads
            | StartBit :: rest ->
                match decodePayload rest (BitSequence.Create(config.PayloadLength)) with
                | Some payload, remaining -> findPayloads remaining (payload :: payloads)
                | None, remaining -> findPayloads remaining payloads
            | _ :: rest -> findPayloads rest payloads

        findPayloads durations []

type RogerStaticDecoder() =
    interface IProtocolDecoder<BitSequence> with
        member _.Config = RogerStatic.config
        member _.Decode(durations) = RogerStatic.decode durations

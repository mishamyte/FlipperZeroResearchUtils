namespace FlipperZeroResearchUtils.Protocols

open FlipperZeroResearchUtils.Common

module DitecDynamic =

    let config: ProtocolConfig =
        {
            DecoderConfig =
                {
                    Short = 400
                    Long = 1200
                    Gap = 800
                    Delta = 100
                }
            PayloadLength = 54
        }

    let decode durations =
        let rec decodePayload remainingDurations (currentBits: BitSequence) =
            if currentBits.Length = config.PayloadLength then
                Some(currentBits), remainingDurations
            else
                match remainingDurations with
                | [] -> None, [] // Payload is not full
                | LongLow :: ShortHigh :: rest -> decodePayload rest (currentBits.AddZero()) // LongLow + ShortHigh -> 0
                | ShortLow :: LongHigh :: rest -> decodePayload rest (currentBits.AddOne()) // ShortLow + LongHigh -> 1
                | StartBit :: _ -> None, remainingDurations // Unexpected StartBit
                | _ :: rest -> decodePayload rest currentBits

        let rec findPayloads remainingDurations payloads =
            match remainingDurations with
            | [] -> List.rev payloads
            | StartBit :: rest ->
                match decodePayload rest (BitSequence.Create(config.PayloadLength)) with
                | Some payload, remaining -> findPayloads remaining (payload :: payloads)
                | None, remaining -> findPayloads remaining payloads
            | _ :: rest -> findPayloads rest payloads

        findPayloads durations []

type DitecDynamicDecoder() =
    interface IProtocolDecoder<BitSequence> with
        member _.Config = DitecDynamic.config
        member _.Decode(durations) = DitecDynamic.decode durations

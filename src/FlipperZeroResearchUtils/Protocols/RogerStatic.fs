namespace FlipperZeroResearchUtils.Protocols

open FlipperZeroResearchUtils.Common

/// Roger static code: 28 bits, MSB first, laid out as Serial[16] | Button[4] | End[8].
/// The End byte either mirrors the Button nibble, or pairs Button 0x1 with 0x20 and 0x2 with 0x23.
/// Samples decoded from our own captures:
///   0000110111111110 0100 00000100 -> 0x0DFE404; Serial 0x0DFE; Button 0x4; End 0x04
///   0101011001010110 0010 00000010 -> 0x5656202; Serial 0x5656; Button 0x2; End 0x02
///   0000100101011111 0010 00100011 -> 0x095F223; Serial 0x095F; Button 0x2; End 0x23
///   0110111111010101 0001 00100000 -> 0x6FD5120; Serial 0x6FD5; Button 0x1; End 0x20
[<Struct>]
type RogerStatic =
    {
        Value: uint64
    }

    member this.Serial = uint16 ((this.Value >>> 12) &&& 0xFFFFUL)

    member this.Button = byte ((this.Value >>> 8) &&& 0xFUL)

    member this.End = byte (this.Value &&& 0xFFUL)

    override this.ToString() =
        $"Value: 0x{this.Value:X7}; Serial: 0x{this.Serial:X4}; Button: 0x{this.Button:X}; End: 0x{this.End:X2}"

    /// Rejects anything wider than the payload: the extra bits would be read by no accessor, so two codes
    /// with identical Serial, Button and End would compare and hash unequal.
    static member Create(value: uint64) =
        if value > 0xFFFFFFFUL then
            invalidArg (nameof value) $"Roger static payload exceeds 28 bits: 0x%X{value}"

        { Value = value }

    static member Create(value: BitSequence) = RogerStatic.Create(value.ToUInt64())

module RogerStatic =

    let config =
        {
            DecoderConfig =
                {
                    Short = 500
                    Long = 1000
                    // Gap is Short * 19 and its tolerance the reference decoder's own bit delta (270) * 5.
                    // Ours is 250, kept tighter so the Short and Long windows cannot overlap at 750.
                    Gap = 9500
                    Delta = { Duration = 250; Gap = 1350 }
                }
            PayloadLength = 28
        }

    let decode durations =
        // Bit 1 is LongHigh + ShortLow, bit 0 is ShortHigh + LongLow. The final bit of a payload has its
        // low replaced by the inter-payload gap, so only the high part is matched and lows are skipped.
        let rec decodePayload remainingDurations (currentBits: BitSequence) =
            if currentBits.Length = config.PayloadLength then
                // The payload must end here: on the gap, on noise, or on end of input. A further bit-shaped
                // duration means these 28 bits were taken mid-stream out of a longer mis-synchronised burst,
                // which yields a plausible but wrong payload. Noise cannot have been crossed on the way in,
                // because Unknown inside the payload is rejected below.
                match remainingDurations with
                | []
                | StartBit :: _
                | Unknown :: _ -> Some(currentBits), remainingDurations
                | _ -> None, remainingDurations // Payload runs on past 28 bits -> trash
            else
                match remainingDurations with
                | [] -> None, [] // Payload is not full
                | Unknown :: rest -> None, rest // Corruption inside the payload -> trash
                | ShortHigh :: rest -> decodePayload rest (currentBits.AddZero()) // ShortHigh -> 0
                | LongHigh :: rest -> decodePayload rest (currentBits.AddOne()) // LongHigh -> 1
                | StartBit :: _ -> None, remainingDurations // Unexpected StartBit
                | (ShortLow | LongLow) :: rest -> decodePayload rest currentBits // Lows carry no bit

        let rec findPayloads remainingDurations payloads =
            match remainingDurations with
            | [] -> List.rev payloads
            | StartBit :: rest ->
                match decodePayload rest (BitSequence.Create(config.PayloadLength)) with
                | Some payload, remaining -> findPayloads remaining (payload :: payloads)
                | None, remaining -> findPayloads remaining payloads
            | _ :: rest -> findPayloads rest payloads

        findPayloads durations [] |> List.map RogerStatic.Create

type RogerStaticDecoder() =
    interface IProtocolDecoder<RogerStatic> with
        member _.Config = RogerStatic.config
        member _.Decode(durations) = RogerStatic.decode durations

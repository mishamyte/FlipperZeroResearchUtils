namespace FlipperZeroResearchUtils.Protocols

open FlipperZeroResearchUtils.Common

[<Struct>]
type GangQi =
    {
        Value: uint64
    }

    member this.Serial = uint16 ((this.Value >>> 18) &&& 0xFFFFUL)

    member this.Button = byte ((this.Value >>> 10) &&& 0xFUL)

    member this.Crc = byte ((this.Value >>> 2) &&& 0xFFUL)

    member private this.B0 = byte (this.Serial >>> 8)

    member private this.B1 = byte (this.Serial &&& 0xFFus)

    member private this.B2 = byte ((0xDuy <<< 4) ||| this.Button)

    member this.CalculatedCrc = byte (0xC8uy - this.B0 - this.B1 - this.B2)

    member this.CalculatedCrc2 = byte (0x02uy + this.B0 + this.B1 + this.B2)

    member this.IsValid = this.Crc = this.CalculatedCrc || this.Crc = this.CalculatedCrc2

    override this.ToString() =
        $"Value: 0x{this.Value:X}; Serial: 0x%X{this.Serial}; Button: 0x%X{this.Button}; Checksum: 0x%02X{this.Crc}; Calculated Checksums: [0x%02X{this.CalculatedCrc}; 0x%02X{this.CalculatedCrc2}]"

    static member Create(value: uint64) = { Value = value }

    static member Create(value: BitSequence) = { Value = value.ToUInt64() }

module GangQi =

    let config =
        {
            DecoderConfig =
                {
                    Short = 500
                    Long = 1200
                    Gap = 2150
                    Delta = 100
                }
            PayloadLength = 34
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

        findPayloads durations [] |> List.map GangQi.Create

type GangQiDecoder() =
    interface IProtocolDecoder<GangQi> with
        member _.Config = GangQi.config
        member _.Decode(durations) = GangQi.decode durations

module FlipperZeroResearchUtils.Common

open System

type BitSequence private (size: int) =
    let bits = ResizeArray<bool>(size)

    member this.AddOne() =
        bits.Add(true)
        this

    member this.AddZero() =
        bits.Add(false)
        this

    member this.Length = bits.Count

    member this.ToArray() = bits.ToArray()

    member this.ToUInt64() =
        if bits.Count > 64 then
            failwith "Bit sequence exceeds 64 bits, cannot convert to UInt64"

        bits
        |> Seq.fold
            (fun (acc: uint64) bit ->
                let shiftedAcc = acc <<< 1
                if bit then shiftedAcc ||| 1UL else shiftedAcc)
            0UL

    static member Create(size) = BitSequence(size)

    static member Create() = BitSequence.Create(24)

    override this.ToString() =
        bits
        |> Seq.map (fun bit -> if bit then "1" else "0")
        |> String.concat String.Empty

module SubGhzRawFileParser =

    open System.IO

    [<Literal>]
    let private rawDataHeader = "RAW_Data:"

    let parseToDurationsArray (path: string) =
        File.ReadAllLines(path)
        |> Array.collect (fun line ->
            if line.StartsWith(rawDataHeader) then
                line.Substring(rawDataHeader.Length).Trim().Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map Int32.Parse
            else
                Array.empty)

[<AutoOpen>]
module DurationDecoder =

    [<Struct>]
    type DurationType =
        | ShortLow
        | ShortHigh
        | LongLow
        | LongHigh
        | StartBit
        | Unknown

    [<Struct>]
    type Config =
        {
            Short: int
            Long: int
            Gap: int
            Delta: int
        }

    let decodeDurations (config: Config) (durations: int array) =
        let (|Short|Long|Gap|Other|) duration =
            let magnitude = abs duration
            let isInRange target value = abs (target - value) <= config.Delta

            if isInRange config.Short magnitude then Short(duration > 0)
            elif isInRange config.Long magnitude then Long(duration > 0)
            elif isInRange config.Gap magnitude then Gap
            else Other

        durations
        |> Array.map (function
            | Short true -> ShortHigh
            | Short false -> ShortLow
            | Long true -> LongHigh
            | Long false -> LongLow
            | Gap -> StartBit
            | Other -> Unknown)
        |> Array.toList

type ProtocolConfig =
    {
        DecoderConfig: Config
        PayloadLength: int
    }

type IProtocolDecoder<'payload> =
    abstract Config: ProtocolConfig with get
    abstract Decode: DurationType list -> 'payload list

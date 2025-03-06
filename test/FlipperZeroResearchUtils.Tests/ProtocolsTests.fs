module FlipperZeroResearchUtils.Tests.ProtocolsTests

open System
open System.IO
open FlipperZeroResearchUtils
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

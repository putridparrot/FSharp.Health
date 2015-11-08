namespace FSharp.Health.Tests

module Health =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open FsUnit
    open FsUnit.Xunit
    open FSharp.Health
    open FSharp.Units.Mass
    open FSharp.Units.Length

    [<Fact>]
    let ``calculate bmi for 172.7 cm tall person weighing 76 kg`` () =
        let bodyMassIndex = bmi 76.<kg> 1.727<m>
                
        bodyMassIndex 
        |> should (equalWithin 0.1) 25.4

    [<Fact>]
    let ``classify a person with bmi of 30.0`` () =
        classify 30.
        |> should equal WeightClassification.ObesityClass1

    [<Fact>]
    let ``get range for HR Zone 1`` () =
        let zone = getHeartRateZoneRange Zone1

        zone
        |> should equal (50., 60.)

    [<Fact>]
    let ``get range for HR Zone 2`` () =
        let zone = getHeartRateZoneRange Zone2

        zone
        |> should equal (60., 70.)

    [<Fact>]
    let ``get range for HR Zone 3`` () =
        let zone = getHeartRateZoneRange Zone3

        zone
        |> should equal (70., 80.)

    [<Fact>]
    let ``get range for HR Zone 4`` () =
        let zone = getHeartRateZoneRange Zone4

        zone
        |> should equal (80., 90.)

    [<Fact>]
    let ``get range for HR Zone 5`` () =
        let zone = getHeartRateZoneRange Zone5

        zone
        |> should equal (90., 100.)

    [<Fact>]
    let ``get range for HR Zone Max`` () =
        let zone = getHeartRateZoneRange ZoneMax

        zone
        |> should equal (100., 100.)

    [<Fact>]
    let ``get cycling range for HR Zone 1`` () =
        let zone = getCyclingHeartRangeZoneRange Zone1

        zone
        |> should equal (60., 65.)

    [<Fact>]
    let ``get cycling range for HR Zone 2`` () =
        let zone = getCyclingHeartRangeZoneRange Zone2

        zone
        |> should equal (65., 75.)

    [<Fact>]
    let ``get cycling range for HR Zone 3`` () =
        let zone = getCyclingHeartRangeZoneRange Zone3

        zone
        |> should equal (75., 82.)

    [<Fact>]
    let ``get cycling range for HR Zone 4`` () =
        let zone = getCyclingHeartRangeZoneRange Zone4

        zone
        |> should equal (82., 89.)

    [<Fact>]
    let ``get cycling range for HR Zone 5`` () =
        let zone = getCyclingHeartRangeZoneRange Zone5

        zone
        |> should equal (89., 94.)

    [<Fact>]
    let ``get cycling range for HR Zone Max`` () =
        let zone = getCyclingHeartRangeZoneRange ZoneMax

        zone
        |> should equal (94., 100.)

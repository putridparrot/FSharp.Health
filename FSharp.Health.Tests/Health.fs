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
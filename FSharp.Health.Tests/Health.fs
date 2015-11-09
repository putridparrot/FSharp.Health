namespace FSharp.Health.Tests

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
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
    open FSharp.Units.Health

    [<Fact>]
    let ``calculate bmi for 172.7 cm tall person weighing 76 kg`` () =
        let bodyMassIndex = bmi 76.<kg> 1.727<m>
                
        bodyMassIndex 
        |> should (equalWithin 0.1) 25.4

    [<Fact>]
    let ``classify a person with bmi of 18.5`` () =
        classify 18.4
        |> should equal WeightClassification.Underweight

    [<Fact>]
    let ``classify a person with bmi of 24.9`` () =
        classify 24.9
        |> should equal WeightClassification.Normal

    [<Fact>]
    let ``classify a person with bmi of 29.9`` () =
        classify 29.9
        |> should equal WeightClassification.Overweight

    [<Fact>]
    let ``classify a person with bmi of 34.9`` () =
        classify 34.9
        |> should equal WeightClassification.ObesityClass1

    [<Fact>]
    let ``classify a person with bmi of 39.9`` () =
        classify 39.9
        |> should equal WeightClassification.ObesityClass2

    [<Fact>]
    let ``classify a person with bmi of 40`` () =
        classify 40.
        |> should equal WeightClassification.MorbidObesity

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

    [<Fact>]
    let ``maximum heart rate - standard formula, male aged 46`` () =
        mhr Standard Male 46.
        |> should equal 174.<bpm>

    [<Fact>]
    let ``maximum heart rate - standard formula, female aged 46`` () =
        mhr Standard Female 46.
        |> should equal 174.<bpm>

    [<Fact>]
    let ``maximum heart rate - Gulati formula, male aged 46`` () =
        mhr Gulati Male 46.
        |> should equal 174.<bpm>

    [<Fact>]
    let ``maximum heart rate - Gulati formula, female aged 46`` () =
        mhr Gulati Female 46.
        |> should equal 165.52<bpm>

    [<Fact>]
    let ``maximum heart rate - Jackson formula, male aged 46`` () =
        mhr Jackson Male 46.
        |> should equal 176.08<bpm>

    [<Fact>]
    let ``maximum heart rate - Whyte formula, male aged 46`` () =
        mhr Whyte Male 46.
        |> should equal 176.7<bpm>

    [<Fact>]
    let ``maximum heart rate - Whyte formula, female aged 46`` () =
        mhr Whyte Female 46.
        |> should equal 165.86<bpm>

    [<Fact>]
    let ``maximum heart rate - Tanaka formula, male aged 46`` () =
        mhr Tanaka Male 46.
        |> should equal 175.8<bpm>

    [<Fact>]
    let ``maximum heart rate - Tanaka formula, female aged 46`` () =
        mhr Tanaka Female 46.
        |> should equal 175.8<bpm>

    [<Fact>]
    let ``maximum heart rate - Gellish formula, male aged 46`` () =
        mhr Gellish Male 46.
        |> should equal 174.8<bpm>

    [<Fact>]
    let ``maximum heart rate - Gellish formula, female aged 46`` () =
        mhr Gellish Female 46.
        |> should equal 174.8<bpm>

    [<Fact>]
    let ``maximum heart rate - LondreeMoeschberger formula, male aged 46`` () =
        mhr LondreeMoeschberger Male 46.
        |> should (equalWithin 0.001) 173.594<bpm>

    [<Fact>]
    let ``maximum heart rate - LondreeMoeschberger formula, female aged 46`` () =
        mhr LondreeMoeschberger Female 46.
        |> should (equalWithin 0.001) 173.594<bpm>

    [<Fact>]
    let ``maximum heart rate - Miller formula, male aged 46`` () =
        mhr Miller Male 46.
        |> should equal 177.9<bpm>

    [<Fact>]
    let ``maximum heart rate - Miller formula, female aged 46`` () =
        mhr Miller Female 46.
        |> should equal 177.9<bpm>

    [<Fact>]
    let ``calculate target heart rate at 60%`` () =
        thr 180.<bpm> 63.<bpm> 0.6
        |> should equal 133.2<bpm>

    [<Fact>]
    let ``calculate target heart rate at 80%`` () =
        thr 180.<bpm> 63.<bpm> 0.8
        |> should (equalWithin 0.01) 156.6<bpm>

    [<Fact>]
    let ``calculate BMR male aged 35, height 1.73m, weight 84kg`` () =
        bmr Male 84.<kg> 1.73<m> 35.
        |> should (equalWithin 0.001) 1850.594

    [<Fact>]
    let ``calculate BMR female aged 35, height 1.73m, weight 84kg`` () =
        bmr Female 84.<kg> 1.73<m> 35.
        |> should (equalWithin 0.001) 1614.782

    [<Fact>]
    let ``calculate daily calories sedentary`` () =
        calculateDailyCalories ActivityLevel.Sedentary 1850.594
        |> should (equalWithin 0.001) 2220.713

    [<Fact>]
    let ``calculate daily calories lightly active`` () =
        calculateDailyCalories ActivityLevel.LightlyActive 1850.594
        |> should (equalWithin 0.001) 2544.567

    [<Fact>]
    let ``calculate daily calories moderately active`` () =
        calculateDailyCalories ActivityLevel.ModeratelyActive 1850.594
        |> should (equalWithin 0.001) 2868.421

    [<Fact>]
    let ``calculate daily calories very active`` () =
        calculateDailyCalories ActivityLevel.VeryActive 1850.594
        |> should (equalWithin 0.001) 3192.275

    [<Fact>]
    let ``calculate daily calories extra active`` () =
        calculateDailyCalories ActivityLevel.ExtraActive 1850.594
        |> should (equalWithin 0.001) 3516.129

//    [<Fact>]
//    let ``calculate body fat male`` () =
//        calculateBodyFat Male  75.<kg> 20.<cm> 112.<cm> 105.<cm> 27.<cm>
//        |> should (equalWithin 0.001) 49.0061

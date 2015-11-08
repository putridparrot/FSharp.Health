namespace FSharp

module Health =

    open FSharp.Units.Mass
    open FSharp.Units.Length
    open FSharp.Units.Health

    type Gender =
        | Male
        | Female

    type WeightClassification =
        | Underweight
        | Normal
        | Overweight
        | ObesityClass1
        | ObesityClass2
        | MorbidObesity

    type ActivityLevel =
        // little or no excercise
        | Sedentary
        // little excercise/sports 1-3 days/week
        | LightlyActive
        // moderate excercise/sports 3-5 days/week
        | ModeratelyActive
        // hard excercise 6-7 days a week
        | VeryActive
        // very hard excercise/sports & physical job or 2x training
        | ExtraActive

    type BorgRatingOfPerceivedExcertionScale =
        | Rest = 0
        | ReallyEasy = 1
        | Easy = 2
        | Moderate = 3
        | SortOfHard = 4
        | Hard1 = 5
        | Hard2 = 6
        | ReallyHard1 = 7
        | ReallyHard2 = 8
        | ReallyReallyHard = 9
        | Maximal = 10

    type HeartRateZones =
        // Light Excercise/Recovery (aerobic)
        | Zone1
        // Moderate Excercise/Endurance (aerobic)
        | Zone2
        // Endurance Training/Stamina (aerobic)
        | Zone3
        // High Performance Training/Economy (anaerobic)
        | Zone4
        // Speed (anearobic)
        | Zone5
        | ZoneMax

    // returns the % (range) of Max HR 
    let getHeartRateZoneRange hrz =
        match hrz with
        | Zone1 -> (50., 60.)
        | Zone2 -> (60., 70.)
        | Zone3 -> (70., 80.)
        | Zone4 -> (80., 90.)
        | Zone5 -> (90., 100.)
        | ZoneMax -> (100., 100.)
    
    let getCyclingHeartRangeZoneRange hrz =
        match hrz with
        | Zone1 -> (60., 65.)
        | Zone2 -> (65., 75.)
        | Zone3 -> (75., 82.)
        | Zone4 -> (82., 89.)
        | Zone5 -> (89., 94.)
        | ZoneMax -> (94., 100.)

    // the approximate number of calories required to increase/descrease body weight by a lb
    let approximateCaloriesPerPoundOfWeight = 3500
    
    let bmi (weight : float<kg>) (height : float<m>) = float (weight / (height * height))
    
    // works out your WeightClassification based upon your BMI
    let classify bmiValue = 
        match bmiValue with
        | v when v <= 18.5 -> WeightClassification.Overweight
        | v when v > 18.5 && v < 25. -> WeightClassification.Normal
        | v when v >= 25. && v < 30. -> WeightClassification.Overweight
        | v when v >= 30. && v < 35. -> WeightClassification.ObesityClass1
        | v when v >= 35. && v < 40. -> WeightClassification.ObesityClass1
        | _ -> WeightClassification.MorbidObesity

    let bmr gender (weight : float<kg>) (height : float<m>) ageInYears =
        match gender with
        | Gender.Male -> 66. + (13.7 * float weight) + (5. * float height * 100.) - (6.8 * ageInYears)
        | Gender.Female -> 655. + (9.6 * float weight) + (1.8 * float height * 100.) - (4.7 * ageInYears)

    // uses the Harris Benedict Formula
    let calculateDailyCalories activityLevel bmrValue =
        match activityLevel with
        | ActivityLevel.Sedentary -> bmrValue * 1.2
        | ActivityLevel.LightlyActive -> bmrValue * 1.375
        | ActivityLevel.ModeratelyActive -> bmrValue * 1.55
        | ActivityLevel.VeryActive -> bmrValue * 1.725
        | ActivityLevel.ExtraActive -> bmrValue * 1.9
    
    //http://www.bmi-calculator.net/body-fat-calculator/body-fat-formula.php
    let calculateBodyFat gender weightInKg wristAtFullestPointInCm waistAtNavalInCm hipAtFullest forearmAdFullest =
        match gender with
        | Gender.Male -> 
            let factor1 = (weightInKg * 1.082) + 94.42
            let factor2 = waistAtNavalInCm * 4.15
            let leanBodyMass = factor1 - factor2
            let bodyFatWeight = weightInKg - leanBodyMass
            (bodyFatWeight * 100.) / weightInKg
        | Gender.Female ->
            let factor1 = (weightInKg * 0.732) + 8.987
            let factor2 = wristAtFullestPointInCm / 3.140
            let factor3 = waistAtNavalInCm * 0.157
            let factor4 = hipAtFullest * 0.249
            let factor5 = forearmAdFullest * 0.434
            let leanBodyMass = factor1 + factor2 - factor3 - factor4 + factor5
            let bodyFatWeight = weightInKg - leanBodyMass
            (bodyFatWeight * 100.) / weightInKg

    // the MHR "usual equation" is derived 
    let maximumHeartRate age = 220 - age

    // maximum heart rate (mhr), heart rate at rest (rhr)
    // intensity as a % (i.e. somewhere between 60% and 80% depending on goals)
    let targetHeartRate rhr mhr intensity = 
        LanguagePrimitives.FloatWithMeasure<bpm> ((mhr - rhr) * intensity) + rhr
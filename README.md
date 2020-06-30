# FSharp.Health

[![Build Status](https://travis-ci.com/putridparrot/FSharp.Health.svg?branch=master)](https://travis-ci.com/putridparrot/FSharp.Health)

Health and fitness type formulae etc.

Includes some different types and formulae for calculating various health related values.

**mhr - maximum heart rate calculation**

The _standard_ formula for this is 220 - age, but whilst researching I found several other formulae.
Some predict dofferent MHR based upon gender as well. I've combined them into the mhr function.

To use simply write something like this

```
let maxHeartRate = mhr Standard Male 46.
```

In some cases the gender is used in other's there's no variation between genders. In the case of 
Dr Gulati et al, they have defined a female only formula, so I've combined this with the standard 
formula for a male.

**thr - target heart rate calculation**

Allows the calculation of your target heart rate zone, for example with knowledge of your 
maximum heart rate (MHR) and resting heart rate (RHR) and the percentage range you want to find
you can calculate the TH for that range.

For example, if your MHR is calculated  as 180 bpm and your RHR is 63 bpm we might wish to find
the THR for the zone 60%-80%, so we calculate using

```
let sixtyPercent = thr 180.<bpm> 63.<bpm> 0.6
let eightyPercent = thr 180.<bpm> 63.<bpm> 0.8
```

**bmi - body mass index**

Calculates the body mass index of an individual based upon their weight in kg and height in metres (we can easily 
convert to kg and m using my FSharp.Units library).

**classify**

Using the individual's bmi and using standard "ranges" or classifications, for example, Underweight, Overweight etc.
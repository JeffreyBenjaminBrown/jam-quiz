# The most harmonic 41-edo scale families

These were all found in GHCI by running
```
myPrint $ allModes 41 $ nice7tone41edoFamilies !! n
```
for some value of `n`,
as of commit 844cfe0221d30fb4.


## The first four scales are what you would hope for.

Scale 0 ("the first" in computer language)
of these scales is just minor `[0,7,11,17,24,28,35]`,
scale 1 is just major `[0,7,13,17,24,30,37]`,
scale 2 is harmonic major `[0,7,13,17,24,28,37]`,
and scale 3 is harmonic minor `[0,7,11,17,24,28,37]`.
That, to me, confirms the process is decent.

All the above have three just `5:4s`, three just `6:5`s, and one Pythagorean minor third.

## Scales 4 and 5 introduce 7:6, 11:9, 13:9 (22\41) and 20:13 (25\41)

Scale 4 is a just phrygian with a half-flat 7:
`[0,4,11,17,24,28,33]`.
Scale 5 is a just phrygian with an up       7:
`[0,4,11,17,24,28,36]`.

## Scales 6 and 7 feel Pythagorean

In both the thirds include a Pythagorean major and two Pythagorean minors.
Both have five perfect fifths,
like the four most harmonic families.

Scale 6 is a just dorian with an up 6:
`[0,7,11,17,24,31,35]`.
Scale 7 is a just major with an up 6:
`[0,7,13,17,24,31,37]`.

## Scale 8 is the "melodic" family

Its thirds are nearly diatonic:
`[10, 13, 14, 13, 10, 11, 11]`.
It has two tritones and one half-sharp fifth:
`[24, 26, 24, 24, 21, 21, 24]`.

One mode is melodic major:
`[0,7,13,17,24,28,34]`.
Another is  melodic minor
`[0,7,11,17,24,31,37]`.

To construct it, remember the two consecutive half steps.

## Scales 9 and 10 are septimal "exaggerated harmonic" scales

They are the first with both septimal thirds,
and the first in which the major septimal third *is* a third
(rather than a fourth).
They have weird fifths --
four `3:2`s, a `10:7`, *an `11:8` and an `8:5`*.
In both one scale step is a giant `6:5`.
Both are "harmonic something" scales in which the "out note"
-- the one distinguishing it from a diatonic one --
is exaggerated by a half-sharp if sharp or half-flat if flat.

Scale 9 is harmonic major but with 26: `[0,7,13,17,24,26,37]`
Thirds: `[13, 15, 11, 13, 10, 11, 9]`
Fifths: `[24, 28, 21, 24, 19, 24, 24]`

Scale 10 is harmonic minor but with 39: `[0,7,11,17,24,28,39]`
Thirds: `[9, 11, 10, 13, 11, 15, 13]`
Fifths: `[19, 24, 21, 28, 24, 24, 24]`


## BIG SKIP: Scales 11 through 19

Looking at their formulas,
esp. the structure of their thirds and fifths,
they aren't obviously interesting.


## Scales 20 and 21 are the first with more than one 11:9

and they've got three of them.
Both are constructed by shifting by a parallel microtone (1\41)
two notes in a diatonic scale separated by a fifth.
Scale 20 = lydian up 2 (8\41) up 6 (32\41)
`[0,8,13,20,24,32,37]`
Scale 21 = phrygian down 4 (16\41) down 7 (33\41):
`[0,4,11,16,24,28,33]`


## Scales 22 and 23 are very septimal

They are the first with 2 `9:7`s,
the first with 3 `7:6`s, and
the first with only 3 `3:2`s.
The three fifths come right in a row.
Scale 22 = `[0,5,9,20,24,29,33]`
Scale 23 = `[0,5,9,20,24,29,37]`

# Scales discovered through data mining.

## Every scale has at least one pythagorean or neutral third

How to see that:

```
> x = filter (not . hasIntervalsIn 41 [10,12,14]) nice7tone41edoFamilies> length x
0
```

## Scales with extreme variation in thirds

The most harmonic scale families with the least and the most variation in their thirds both have five perfect fifths.

To find them, first define this:
```
x = L.sortOn (sumOfSquaredDegrees 41 2) nice7tone41edoFamilies
```

### The most harmonic family with the least variation in thirds.

```
> myPrint $ allModes 41 $ x!!0
[0,5,11,17,22,29,34]
[0,6,12,17,24,29,36]
[0,6,11,18,23,30,35]
[0,5,12,17,24,29,35]
[0,7,12,19,24,30,36]
[0,5,12,17,23,29,34]
[0,7,12,18,24,29,36]
```

### The most harmonic family with the most variation in thirds.

```
> myPrint $ allModes 41 $ last x
[0,2,9,17,19,26,34]
[0,7,15,17,24,32,39]
[0,8,10,17,25,32,34]
[0,2,9,17,24,26,33]
[0,7,15,22,24,31,39]
[0,8,15,17,24,32,34]
[0,7,9,16,24,26,33]
```

## The most harmonic scale with no pyth third

```
x = filter (not . hasIntervalsIn 41 [10,14]) nice7tone41edoFamilies
myPrint $ allModes 41 $ x !! 0

[0,4,9,17,21,28,34]
[0,5,13,17,24,30,37]
[0,8,12,19,25,32,36]
[0,4,11,17,24,28,33]
[0,7,13,20,24,29,37]
[0,6,13,17,22,30,34]
[0,7,11,16,24,28,35]
```

## The most harmonious scale with no just third

It has five perfect fifths and six neutral thirds.
```
x = filter (not . hasIntervalsIn 41 [11,13]) nice7tone41edoFamilies
myPrint $ allModes 41 $ x !! 0

[0,5,10,17,22,29,34]
[0,5,12,17,24,29,36]
[0,7,12,19,24,31,36]
[0,5,12,17,24,29,34]
[0,7,12,19,24,29,36]
[0,5,12,17,22,29,34]
[0,7,12,17,24,29,36]
```

## The most harmonic scale with no 3:2

```
x = filter (not . hasIntervalsIn 41 [24]) nice7tone41edoFamilies
myPrint $ allModes 41 $ x !! 0

[0,4,9,15,22,30,35]
[0,5,11,18,26,31,37]
[0,6,13,21,26,32,36]
[0,7,15,20,26,30,35]
[0,8,13,19,23,28,34]
[0,5,11,15,20,26,33]
[0,6,10,15,21,28,36]
```

# The most harmonic families from some lower edos, translated to 41-edo

These were all found in GHCI by running
```
myPrint $ allModes 41 $ modesOfMostHarmonicTranslation fromEdo 41 $ scaleFamilies !! 0
```
for some value of `fromEdo` and `scaleFamilies`.

## 13-edo

No `3:2`s but lots that are close.
Four major thirds and a neutral third.
```
myPrint $ modesOfMostHarmonicTranslation 13 41 $ nice7tone13edoFamilies !! 0
```

## 15-edo

Just one `3:2`.
Five minor thirds, one major, one pyth major.
```
myPrint $ modesOfMostHarmonicTranslation 15 41 $ nice7tone15edoFamilies !! 0
```

## 17-edo

4 neutral thirds, 3 perfect fifths.
```
myPrint $ modesOfMostHarmonicTranslation 17 41 $ nice7tone17edoFamilies !! 0
```

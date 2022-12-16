# Some useful-looking 41-edo scale families

These were all found using
```
myPrint $ allModes 41 $ nice7tone41edoFamilies !! n
```
for some value of n.


## The first four scales are what you would hope for.

Scale 0 ("the first" in computer language)
of these scales is just minor [0,7,11,17,24,28,35],
scale 1 is just major [0,7,13,17,24,30,37],
scale 2 is harmonic major [0,7,13,17,24,28,37],
and scale 3 is harmonic minor [0,7,11,17,24,28,37].
That, to me, confirms the process is decent.

All the above have three just 5:4s, three just 6:5s, and one Pythagorean minor third.

## Scales 4 and 5 introduce 7:6, 11:9, 13:9 (22\41) and 20:13 (25\41)

Scale 4 is a just phrygian with a half-flat 7:
`[0,4,11,17,24,28,33]`.
Scale 5 is a just phrygian with an up       7:
`[0,4,11,17,24,28,36]`.

## Scales 6 and 7 feel Pythagorean

In both the thirds include a Pythagorean major and two Pythagorean minors.
Both have five perfect fifths.

Scale 6 is just dorian with an up 6:
`[0,7,11,17,24,31,35]`.
Scale 7 is a just major with an up 6:
`[0,7,13,17,24,31,37]`.

## Scale 8 is "melodic".

Structure of thirds is nearly diatonic:
`[10, 13, 14, 13, 10, 11, 11]`.
It has two tritones and one half-sharp fifth:
`[24, 26, 24, 24, 21, 21, 24]`.

One mode is melodic major:
`[0,7,13,17,24,28,34]`.
Another is  melodic minor
`[0,7,11,17,24,31,37]`.

Remember the two consecutive half steps.

## Scales 9 and 10 are septimal

They are the first with both septimal thirds,
and the first in which the major septimal third *is* a third
(rather than a fourth).
As fifths both have four `3:2`s, one `11:8`, one `10:7`, and one *`8:5`*.
In both one scale step is a giant `6:5`.

Scale 9 is a just major with 26: `[0,7,13,17,24,26,37]`
Thirds: `[13, 15, 11, 13, 10, 11, 9]`
Fifths: `[24, 28, 21, 24, 19, 24, 24]`

Scale 10 is a just minor with 39: `[0,7,11,17,24,28,39]`
Thirds: `[9, 11, 10, 13, 11, 15, 13]`
Fifths: `[19, 24, 21, 28, 24, 24, 24]`

## BIG SKIP

## Scales 22 and 23 are very septimal

They are the first with 2 `9:7`s,
the first with 3 `7:6`s, and
the first with only 3 `3:2`s.
Scale 22 = `[0,5,9,20,24,29,33]`
Scale 23 = `[0,5,9,20,24,29,37]`

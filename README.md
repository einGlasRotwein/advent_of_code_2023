# Advent of Code 2023

This repo contains my solutions for the [Advent of
Code](https://adventofcode.com/) 2023 in `R`.

Here’s my past performance (and this year’s performance, so far):

![](README_files/figure-commonmark/unnamed-chunk-2-1.png)

However, this year is not really about performance, especially not when
it comes to completion times. Most of the code in this repo was written
using only one hand, because I’m nursing a three-month old baby with the
other.

I solved the majority of puzzles in base `R`. I’ve marked the days where
I used libraries with footnotes.

Follow me on [Bluesky](https://bsky.app/profile/juli-nagel.bsky.social)!

## TOC

Here is a table of contents showing you what each day was about, along
with some stats. Completion time refers to the time it took me to
complete the puzzle since release (part 1, and part 2 in brackets), as
tracked on the Advent of Code website. Next is my rank for the puzzle on
that day (part 1 (part 2)), and finally the runtime of my code. It’s a
crude measure of runtime[^1], where I simply calculate the difference in
`Sys.time()` at the beginning and the end of part 1, and the beginning
and end of part 2. That means in some occasions, the runtime for part 2
will be much shorter because part of the code needed to solve part 2 was
already run in part 1. Reading in the data (and sometimes, basic
preprocessing that has nothing to do with the puzzle solution) is
excluded from the run time calculations.

| Day | Title                           | Completion Time     | Rank          | Run Time           |
|-----|---------------------------------|---------------------|---------------|--------------------|
| 1   | Trebuchet?!                     | 02:15:03 (03:50:15) | 20750 (18843) | 7.92 (83.07) ms    |
| 2   | Cube Conundrum                  | 05:21:47 (05:27:15) | 34591 (32070) | 85.35 (9.00) ms    |
| 3   | Gear Ratios                     | 02:06:12 (02:39:14) | 12256 (10955) | 171.83 (517.08) ms |
| 4   | Scratchcards                    | 04:10:29 (04:47:29) | 33850 (27365) | 29.22 (10.95) ms   |
| 5   | If You Give A Seed A Fertilizer | 10:55:35 (\>24h)    | 42559 (58874) | 67.84 (227.23) ms  |
| 6   | Wait For It                     | 02:01:24 (02:05:10) | 17273 (16417) | 11.37 (478.78) ms  |
| 7   | Camel Cards                     | 03:59:51 (04:44:01) | 19094 (16453) | 128.3 (174.36) ms  |
| 8   | Haunted Wasteland[^2]           | 03:01:04 (03:36:02) | 19949 (13150) | 382.2 (5308.71) ms |
| 9   | Mirage Maintenance              | 04:07:54 (04:34:14) | 18471 (18817) | 27.62 (35.00) ms   |
| 10  | Pipe Maze                       | 05:04:38 (\>24h     | 14949 (45650) | 466.84 (46.05) ms  |
| 11  | Cosmic Expansion                | 13:37:18 (13:53:48) | 31790 (29731) | 46.61 (47.74) s    |
| 12  | Hot Springs                     | 12:33:46 (-)        | 21543 (-)     | 1.56 (-) min       |
| 13  | Point of Incidence              | 05:39:28 (05:44:05) | 13716 (10433) | 34.97 (38.82) ms   |
| 14  | Parabolic Reflector Dish        | 06:37:28 (-)        | 17927 (-)     | 294.96 (-) ms      |
| 15  | Lens Library                    | 12:21:41 (13:39:22) | 27396 (23989) | 43.83 (168.68) ms  |
| 16  | The Floor Will Be Lava          | 11:28:19 (11:55:39) | 18300 (17456) | 0.13 (10.91) s     |
| 17  | Clumsy Crucible                 | \- (-)              | \- (-)        | \- (-)             |
| 18  | Lavaduct Lagoon                 | 04:35:45 (12:32:16) | 8859 (10645)  | 18.65 (20.66) ms   |
| 19  | Aplenty                         | 10:33:24 (\>24h)    | 15489 (15091) | 168.72 (148.22) ms |
| 20  | Pulse Propagation               | \>24h (-)           | 17824 (-)     | 715.10 (-) ms      |
| 21  | Step Counter                    | \>24h (-)           | 21349 (-)     | 31.56 (-) s        |
| 22  | Sand Slabs                      | \- (-)              | \- (-)        | \- (-) ms          |
| 23  | A Long Walk[^3]                 | \>24h (\>24)        | 15363 (13284) | 6.53 s (9.90 h)    |
| 24  | Never Tell Me The Odds          | \>24h (-)           | 14619 (-)     | 557.88 (-) ms      |
| 25  | Snowverload[^4]                 | \>24h (-)           | 11479 (-)     | 219.86 (-) ms      |

[^1]: I know these stats are basically meaningless for anyone else’s
    computer, but to put things into perspective, here are my computer
    stats: Razer Blade 15 Base Model, Intel Core i7-10750H CPU @
    2.60GHz, 259 Mhz, 6 Cores, 12 Logical Processors, 16 GB RAM

[^2]: **POSSIBLE SPOILERS AHEAD!** Here, I used the function `mLCM()`
    from the package `numbers`.

[^3]: For part 1, I originally coded up a base `R` version that takes
    1.09 min to run (`day23_pt1.R` and `day23_functions.R`). Then, I
    wrote a second version using `igraph` (`day23_pt1_v2.R`) which is
    reported here. For part 2, there is only one version using
    `igraph`(`day23_pt2.R`).

[^4]: Here, I shamelessly used `igraph`.

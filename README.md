# letters-frp

This repository contains the code presented at the
[Regensburg Haskell Meetup](https://www.meetup.com/de-DE/Regensburg-Haskell-Meetup/?_locale=de-DE)
on April 21, 2020, on
[FRP](https://www.meetup.com/de-DE/Regensburg-Haskell-Meetup/events/270078582/)
and additional examples for [vty](https://hackage.haskell.org/package/vty), the "simple terminal UI library",
and [Reactive Banana](https://hackage.haskell.org/package/reactive-banana), a nice Haskell FRP library.

  - [`meetup`](app/meetup.hs) is the code from our live-coding session.
  - [`letters`](app/letters.hs) is an implementation of the "letters" game.
  - [`hello-vty`](app/hello-vty.hs) is a hello-world program for vty (without FRP).
  - [`counter-vty`](app/counter-vty.hs) is a simple counter written with vty (and again without FRP).
  - [`counter-banana`](app/counter-banana.hs) is the same counter, but using Reactive Banana.
  - [`counter`](app/counter.hs) is the same, but using the library module [`Reactive.Banana.Vty`](src/Reactive/Banana/Vty.hs).
  - [`die`](app/die.hs) is an example of how to use randomness.
  - [`Reactive.Banana.Vty`](src/Reactive/Banana/Vty.hs) is a little library automating hooking up Reactive Banana with vty.

You can build the code using stack with `stack build` and execute the example programs with `stack exec <program name>`.
Program `letters` takes an optional additional argument, the name of a text-file containing words to use for the game.

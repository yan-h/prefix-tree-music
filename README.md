[Accompanying paper](https://dl.acm.org/authorize?N680357) <!-- ACM DL Article: Representing music with prefix trees -->

[Presentation slides](https://docs.google.com/presentation/d/1bBmiAERL3_-Jdrq-zzr6QRyQqib-1JGjIYjDAKQ-Jvs/edit?usp=sharing)

A recreation of Brahms' Waltz in A Flat Major using prefix trees. The code is in Haskell, and is built on the [Euterpea](http://euterpea.com/) library.

This project uses [Stack](https://docs.haskellstack.org/en/stable/README/).

To play the Waltz, run the following in ghci:

```
play (toPlayable waltz)
```
If nothing plays, consider running `devices` to display a list of MIDI output devices, and playing from one of them using `playDev n (toPlayable waltz)`, where `n` is a device number.

If this does not work, consult Euterpea's [page on MIDI](http://euterpea.com/euterpea/setting-up-midi/).

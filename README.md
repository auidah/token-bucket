# Token Bucket

What is a Token Bucket?

A token bucket is a simulation of a bucket that is filled with tokens by a tap
at a linear rate over time, and offers the facility to withdraw tokens at any
time.

This project provides a token bucket of finite capacity; that is, when the
bucket reaches its configured capacity, depositing additional tokens will not
change the volume of tokens in the bucket.  Also, this implementation will
never give more tokens than are available in the bucket.

While this project was conceived for the purposes below, it could be adapter to
any scenario in which such facilities are useful.

## Purpose

1) Configurable rate limiting with a sliding window.
2) Avoid unnecessary multi-threading in the tap implementation.

## Usage Model

The client of the token bucket has the following requirements:

* some action(s) should only be permitted to occur at a desired rate (e.g. for
  service fairness, privacy, cost containment)
* different actions may have different costs, representable by requiring an
  appropriate number of tokens
* the rate may be difficult to express in pure terms of x actions per time
  frame: burst rate and sustained maximum rate are two such concepts which are
handled well by a token bucket.


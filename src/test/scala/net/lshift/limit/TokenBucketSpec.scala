package net.lshift.limit

import org.specs2._
import org.specs2.ScalaCheck
import org.scalacheck.Gen._
import org.scalacheck.{ Gen, Prop }
import org.scalacheck.Arbitrary
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import BucketTools.floor

@RunWith(classOf[JUnitRunner])
class TokenBucketSpec extends org.specs2.SpecificationWithJUnit with ScalaCheck {
  val sup = 1000000
  val maxSupportedRefillRate = 100
  val maxInterval = 1000

  def is = sequential ^
    "Given an empty bucket, when I try to consume any number of tokens, I should get none" ! check { (m: Int) =>
      (m >= 0) ==>
        (TokenBucket.maker.withTokens(0).make.tryConsume(m) must_== (TokenBucket.maker.withTokens(0).make, 0))
    } ^
    "Given a bucket containing n tokens, when I try to consume m tokens" ^
    "where m <= n, then I should get m tokens" ! {
      val mn0 = for { m <- choose(0, sup); n <- choose(m, sup) } yield (m, n)
      Prop.forAll(mn0) {
        case (m, n) =>
          TokenBucket.maker.withTokens(n).make.tryConsume(m) must_== (TokenBucket.maker.withTokens(n - m).make, m)
      }
    } ^
    "where m > n, then I should get n tokens" ! {
      val mn0 = for { n <- choose(0, sup); m <- choose(n, sup) } yield (n, m)
      Prop.forAll(mn0) {
        case (n, m) =>
          TokenBucket.maker.withTokens(n).make.tryConsume(m) must_== (TokenBucket.maker.withTokens(0).make, n)
      }
    } ^
    end ^
    "Given a bucket having a capacity of m + n tokens and currently containing n tokens" ^
    "when I try to deposit k + m tokens, where k >= 0, the bucket should contain (m+n) tokens" ! {
      val _mnk = for { m <- choose(0, sup); n <- choose(0, sup); k <- choose(0, sup) } yield (m, n, k)
      Prop.forAll(_mnk) {
        case (m, n, k) =>
          TokenBucket.maker.withTokens(n).ofLimitedCapacity(m + n).make.deposit(k + m) must_== (
            TokenBucket.maker.withTokens(m + n).ofLimitedCapacity(m + n).make, m)
      }
    } ^
    "when I try to deposit m - k tokens, where k > 0, the bucket should contain m + n - k tokens" ! {
      val _mnk = for { m <- choose(0, sup); n <- choose(0, sup); k <- choose(1, sup) } yield (m, n, k)
      Prop.forAll(_mnk) {
        case (m, n, k) =>
          TokenBucket.maker.withTokens(n).ofLimitedCapacity(m + n).make.deposit(m - k) must_== (
            TokenBucket.maker.withTokens(m + n - k).ofLimitedCapacity(m + n).make, m - k)
      }
    } ^
    end ^
    "Given a bucket of capacity C with a refill rate of p tokens every s milliseconds" ^
    "and given that m milliseconds ago the bucket contained i tokens and none have since been consumed" ^
    "when I try to withdraw k tokens" ^
    "where k <= C and k <= i + floor(m/s) * p, then I should get k tokens" ! {
      val clock = new FakeClock
      val params = for {
        c <- choose(0, sup)
        p <- choose(0, maxSupportedRefillRate)
        s <- choose(1, maxInterval)
        m <- choose(1, maxInterval)
        i <- choose(0, sup)
        k <- choose(0, math.min(c, i + floor(m / s) * p))
      } yield (c, p, s, m, i, k)
      Prop.forAllNoShrink(params) {
        case (c, p, s, m, i, k) => {
          val bucket = TokenBucket.maker.ofLimitedCapacity(c).withTokens(i).suppliedBy(
            Tap.maker.usingClock(clock).make.adjustDropSize(p).adjustFrequency(s)).make
          clock.setTime(m)
          bucket.tryConsume(k)._2 must_== k
        }
      }
    } ^
    "where k <= C and k > r = i + floor(m/s) * p, then I should get r tokens" ! {
      val clock = new FakeClock
      val params = for {
        c <- chooseNum(0, sup)
        p <- chooseNum(0, maxSupportedRefillRate)
        s <- chooseNum(1, maxInterval)
        m <- chooseNum(1, maxInterval)
        i <- chooseNum(0, sup)
        k <- chooseNum(1 + i + floor(m / s) * p, c)
        r <- i + floor(m / s) * p
      } yield (c, p, s, m, i, k, r)
      Prop.forAllNoShrink(params) {
        case (c, p, s, m, i, k, r) => {
          clock.setTime(0)
          val bucket = TokenBucket.maker.ofLimitedCapacity(c).withTokens(i).suppliedBy(
            Tap.maker.usingClock(clock).make.adjustDropSize(p).adjustFrequency(s)).make
          clock.setTime(m)
          bucket.tryConsume(k)._2 must_== r
        }
      }
    } ^
    "where k > C and C <= i + floor(m/s) * p, then I should get C tokens" ! {
      val clock = new FakeClock
      val params = for {
        k <- chooseNum(1, sup)
        p <- chooseNum(0, maxSupportedRefillRate)
        s <- chooseNum(1, maxInterval)
        m <- chooseNum(1, maxInterval)
        i <- chooseNum(0, sup)
        c <- chooseNum(0, math.min(k - 1, i + floor(m / s) * p))
      } yield (c, p, s, m, i, k)
      Prop.forAllNoShrink(params) {
        case (c, p, s, m, i, k) => {
          clock.setTime(0)
          val bucket = TokenBucket.maker.ofLimitedCapacity(c).withTokens(i).suppliedBy(
            Tap.maker.usingClock(clock).make.adjustDropSize(p).adjustFrequency(s)).make
          clock.setTime(m)
          bucket.tryConsume(k)._2 must_== c
        }
      }
    } ^
    "where k > C and C > r = i + floor(m/s) * p, then I should get r tokens" ! {
      val clock = new FakeClock
      val params = for {
        k <- chooseNum(1, sup)
        p <- chooseNum(0, maxSupportedRefillRate)
        s <- chooseNum(1, maxInterval)
        m <- chooseNum(1, maxInterval)
        i <- chooseNum(0, sup)
        c <- chooseNum(1 + i + floor(m / s) * p, k - 1)
        r <- i + floor(m/s) * p
      } yield (c, p, s, m, i, k, r)
      Prop.forAllNoShrink(params) {
        case (c, p, s, m, i, k, r) => {
          clock.setTime(0)
          val bucket = TokenBucket.maker.ofLimitedCapacity(c).withTokens(i).suppliedBy(
            Tap.maker.usingClock(clock).make.adjustDropSize(p).adjustFrequency(s)).make
          clock.setTime(m)
          bucket.tryConsume(k)._2 must_== r
        }
      }
    }
}

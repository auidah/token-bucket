package net.lshift.limit

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen, Prop}
import BucketTools.floor

@RunWith(classOf[JUnitRunner])
class StrictBucketSpec extends org.specs2.SpecificationWithJUnit with ScalaCheck {
  val sup = 1000000
  val maxSupportedRefillRate = 100
  val maxInterval = 1000

  def is = sequential ^
    "Given a strict bucket containing n tokens, when I try to consume m tokens" ^
    "where m <= n, then I should get m tokens" ! {
      val mn0 = for { m <- choose(0, sup); n <- choose(m, sup) } yield (m, n)
      Prop.forAll(mn0) {
        case (m, n) =>
          TokenBucket.maker.strict.withTokens(n).make.tryConsume(m) must_== (TokenBucket.maker.strict.withTokens(n - m).make, m)
      }
    } ^
    "where m > n, then I should get 0 tokens" ! {
      val mn0 = for { n <- choose(0, sup); m <- choose(n, sup) } yield (n, m)
      Prop.forAll(mn0) {
        case (n, m) =>
          TokenBucket.maker.strict.withTokens(n).make.tryConsume(m) must_== (TokenBucket.maker.strict.withTokens(n).make, 0)
      }
    } ^
    end ^
    "Given a strict bucket having a capacity of m + n tokens and currently containing n tokens" ^
    "when I try to deposit k + m tokens, where k >= 0, the bucket should contain (m+n) tokens" ! {
      val _mnk = for { m <- choose(0, sup); n <- choose(0, sup); k <- choose(0, sup) } yield (m, n, k)
      Prop.forAll(_mnk) {
        case (m, n, k) =>
          TokenBucket.maker.strict.withTokens(n).ofLimitedCapacity(m + n).make.deposit(k + m).contents must_== m + n
      }
    } ^
    "when I try to deposit m - k tokens, where k > 0, the bucket should contain m + n - k tokens" ! {
      val _mnk = for { m <- choose(0, sup); n <- choose(0, sup); k <- choose(1, sup) } yield (m, n, k)
      Prop.forAll(_mnk) {
        case (m, n, k) =>
          TokenBucket.maker.strict.withTokens(n).ofLimitedCapacity(m + n).make.deposit(m - k).contents must_== m + n - k
      }
    } ^
    end ^
    "Given a strict bucket of capacity C with a refill rate of p tokens every s milliseconds" ^
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
          clock.setTime(0)
          val bucket = TokenBucket.maker.strict.ofLimitedCapacity(c).withTokens(i).suppliedBy(
            Tap.maker.usingClock(clock).make.adjustDropSize(p).adjustFrequency(s)).make
          clock.setTime(m)
          bucket.tryConsume(k)._2 must_== k
        }
      }
    } ^
    "where k <= C and k > r = i + floor(m/s) * p, then I should get 0 tokens" ! {
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
          val bucket = TokenBucket.maker.strict.ofLimitedCapacity(c).withTokens(i).suppliedBy(
            Tap.maker.usingClock(clock).make.adjustDropSize(p).adjustFrequency(s)).make
          clock.setTime(m)
          bucket.tryConsume(k)._2 must_== 0
        }
      }
    } ^
    "where k > C and C <= i + floor(m/s) * p, then I should get 0 tokens" ! {
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
          val bucket = TokenBucket.maker.strict.ofLimitedCapacity(c).withTokens(i).suppliedBy(
            Tap.maker.usingClock(clock).make.adjustDropSize(p).adjustFrequency(s)).make
          clock.setTime(m)
          bucket.tryConsume(k)._2 must_== 0
        }
      }
    } ^
    "where k > C and C > r = i + floor(m/s) * p, then I should get 0 tokens" ! {
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
          val bucket = TokenBucket.maker.strict.ofLimitedCapacity(c).withTokens(i).suppliedBy(
            Tap.maker.usingClock(clock).make.adjustDropSize(p).adjustFrequency(s)).make
          clock.setTime(m)
          bucket.tryConsume(k)._2 must_== 0
        }
      }
    }
}

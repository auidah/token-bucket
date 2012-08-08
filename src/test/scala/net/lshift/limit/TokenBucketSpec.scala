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
  def is = sequential ^
    "Given an empty bucket, when I try to consume any number of tokens, I should get none" ! check { (m: Int) =>
      (m >= 0) ==>
        (TokenBucket.maker.withTokens(0).make.tryConsume(m) must_== (TokenBucket.maker.withTokens(0).make, 0))
    }
}

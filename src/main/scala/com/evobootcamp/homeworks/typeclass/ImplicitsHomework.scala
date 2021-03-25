package com.evobootcamp.homeworks.typeclass

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {
  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[-T] {
      def apply(value: T): SizeScore
    }

    object GetSizeScore {
      def apply[F](implicit instance: GetSizeScore[F]): GetSizeScore[F] = instance
    }

    object syntax {
      implicit class GetSizeScoreOps[T](inner: T)(implicit getSizeScore: GetSizeScore[T]) {
        def sizeScore: SizeScore = getSizeScore(inner)
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      import syntax._

      private val map = mutable.LinkedHashMap.empty[K, V]
      private var currentSize: Int = 0

      private def getCurrentSize: Int = currentSize

      private def addCurrentSize(size: Int*): Unit = currentSize += size.sum

      private def subCurrentSize(size: Int*): Unit = currentSize -= size.sum

      private def canPut(key: K, value: V): Boolean = {
        getCurrentSize + key.sizeScore + value.sizeScore <= maxSizeScore
      }

      def remove(key: K): Unit = {
        map.remove(key) match {
          case Some(value) => subCurrentSize(key.sizeScore, value.sizeScore)
          case None => ()
        }
      }

      def put(key: K, value: V): Unit = {
        @tailrec
        def recursive(): Unit = {
          if (canPut(key, value)) {
            map.put(key, value)

            addCurrentSize(key.sizeScore, value.sizeScore)
          } else {
            map.headOption match {
              case Some((k, _)) => {
                remove(k)
                recursive()
              }
              case None => ()
            }
          }
        }

        recursive()
      }

      def get(key: K): Option[V] = map.get(key)
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    object Iterate {
      def apply[F[_]: Iterate]: Iterate[F] = implicitly[Iterate[F]]
    }

    object IterateSyntax {
      implicit class IterateOps[-F[_]: Iterate, T](inner: F[T]) {
        def iterator: Iterator[T] = Iterate[F].iterator(inner)
      }
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object Iterate2 {
      def apply[F[_, _]: Iterate2]: Iterate2[F] = implicitly[Iterate2[F]]
    }

    object Iterate2Syntax {
      implicit class IterateOps[-F[_, _]: Iterate2, T, S](inner: F[T, S]) {
        def iterator1: Iterator[T] = Iterate2[F].iterator1(inner)
        def iterator2: Iterator[S] = Iterate2[F].iterator2(inner)
      }
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!

      implicit val iterableMap: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keysIterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val iterablePackedMultiMap: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map({
          case (k, _) => k
        }).iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map({
          case (_, v) => v
        }).iterator
      }

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */

      implicit val byteGetSizeScore: GetSizeScore[Byte] = _ => 1
      implicit val intGetSizeScore: GetSizeScore[Int] = (_: Int) => 4
      implicit val longGetSizeScore: GetSizeScore[Long] = _ => 8
      implicit val charGetSizeScore: GetSizeScore[Char] = (_: Char) => 2
      implicit val stringGetSizeScore: GetSizeScore[String] =
        v => 12 + v.toCharArray.map(_.sizeScore).sum

      def getIteratorSizeScore[A: GetSizeScore](acc: SizeScore)(i: Iterator[A]): SizeScore = {
        @tailrec
        def recursive(acc: SizeScore, i: Iterator[A]): SizeScore = {
          i.nextOption() match {
            case Some(value) => recursive(acc + value.sizeScore, i)
            case None => acc
          }
        }

        recursive(acc, i.iterator)
      }

      implicit def iterableGetSizeScore[A[_]: Iterate, B: GetSizeScore]: GetSizeScore[A[B]] = i => {
        import IterateSyntax._
        getIteratorSizeScore(12)(i.iterator)
      }

      implicit def mapGetSizeScore[A[_, _]: Iterate2, B: GetSizeScore]: GetSizeScore[A[B, B]] = i => {
        import Iterate2Syntax._
        getIteratorSizeScore(12)(i.iterator1) + getIteratorSizeScore(0)(i.iterator2)
      }
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._
    import SuperVipCollections4s.syntax._
    import SuperVipCollections4s.instances._

    final case class Twit(
      id: Long,
      userId: Int,
      hashTags: Vector[String],
      attributes: PackedMultiMap[String, String],
      fbiNotes: List[FbiNote],
    )

    final case class FbiNote(
      month: String,
      favouriteChar: Char,
      watchedPewDiePieTimes: Long,
    )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    implicit val fbiNodeGetSizeScore: GetSizeScore[FbiNote] =
      note => note.month.sizeScore + note.favouriteChar.sizeScore +
        note.watchedPewDiePieTimes.sizeScore

    implicit val twitGetSizeScore: GetSizeScore[Twit] =
      twit => twit.id.sizeScore + twit.userId.sizeScore + twit.hashTags.sizeScore +
        twit.attributes.sizeScore + twit.fbiNotes.sizeScore

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      private val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = cache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}

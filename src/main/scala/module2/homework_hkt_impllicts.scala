package module2

object homework_hkt_impllicts extends App{

    /**
      * 
      * Доработать сигнатуру tupleF и реализовать его
      * По итогу должны быть возможны подобные вызовы
      *   val r1 = println(tupleF(optA, optB))
      *   val r2 = println(tupleF(list1, list2))
      * 
      */
    def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit toBindableA: F[A] => Bindable[F, A], toBindableB: F[B] => Bindable[F, B]) = fa.flatMap(a => fb.map(b => (a, b)))

    implicit def option2Bindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
      def map[B](f: A=>B) = opt.map(f)
      def flatMap[B](f: A=>Option[B]) = opt.flatMap(f)
    }

    implicit def list2Bindable[A](list: List[A]): Bindable[List, A] = new Bindable[List, A] {
      def map[B](f: A=>B) = list.map(f)
      def flatMap[B](f: A=>List[B]) = list.flatMap(f)
    }

    trait Bindable[F[_], A] {
        def map[B](f: A => B): F[B]
        def flatMap[B](f: A => F[B]): F[B]
    }


  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))
}
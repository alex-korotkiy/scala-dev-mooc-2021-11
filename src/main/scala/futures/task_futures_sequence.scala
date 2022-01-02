package futures

import HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Failure

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */


  def separateSuccessAndFailure[A](list: List[Any]) = {
    val start: (List[A], List[Throwable]) = (Nil, Nil)

    list.foldRight(start)((e, r) => {
      e match {
        case Failure(t) => (r._1, t :: r._2)
        case a: A => (a :: r._1, r._2)
        case _ => r
      }
    })
  }

  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    def toSafeFuture[A](f: Future[A]) = f.recover(Failure(_))(ex)
    val seq = Future.sequence(futures.map(toSafeFuture))
    seq.map(separateSuccessAndFailure)
  }
}

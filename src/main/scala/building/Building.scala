package building

import building.floor.{Attic, CommercialAttic, CommercialFloor, Floor, MiddleFloors, ResidentialFloor}

import scala.annotation.tailrec

/** Здание должно иметь:
  *   - строковый адрес
  *   - этажи (сходящиеся к первому этажу) Этаж может быть жилым, коммерческим, либо чердаком (который сам может быть
  *     коммерческим). На каждом жилом этаже живет 2 человека и есть лестница(ссылка) ведущая на следующий этаж У
  *     каждого человека есть возраст (>0) и пол На коммерческом этаже может быть несколько заведений (используйте
  *     Array), но не меньше 1. Здание всегда должно заканчиваться чердаком На чердаке никто не живет, но это может быть
  *     и коммерческое помещение (но только 1).
  */

case class Building private (address: String, firstFloor: Floor)

object Building {

  def apply(address: String, firstFloor: Floor): Either[BuildingInstantiationException, Building] = {
    if (address == null || firstFloor == null)
      Left(NullArgumentsPassed)
    else
      Right(new Building(address, firstFloor))
  }

  /** Проходится по зданию снизу в вверх, применяя функцию [[f]] на каждом жилом этаже с начальным аккумулятором
    * [[accumulator]]
    */
  def fold[A](building: Building, startWith: A)(f: (A, Floor) => A): A = {
    @tailrec
    def recursiveFold(floor: Floor, accumulator: A): A = floor match {
      case _: Attic            => f(accumulator, floor)
      case floor: MiddleFloors => recursiveFold(floor.nextFloor, f(accumulator, floor))
    }
    recursiveFold(building.firstFloor, startWith)
  }

  /** Подсчитывает количество этаже, на которых живет хотя бы один мужчина старше [[olderThan]]. Используйте [[fold]]
    */
  def countOldManFloors(building: Building, olderThan: Int): Int = {
    def isManOlderThan(human: Guest): Boolean = human match {
      case Man(age) => age > olderThan
      case Woman(_) => false
    }
    fold(building, 0)((acc: Int, floor: Floor) =>
      floor match {
        case ResidentialFloor((guest1, guest2), _) =>
          if (isManOlderThan(guest1) || isManOlderThan(guest2)) {
            acc + 1
          } else {
            acc
          }
        case _ => acc
      }
    )
  }

  /** Находит наибольший возраст женщины, проживающей в здании. Используйте [[fold]] */
  def womanMaxAge(building: Building): Either[WomanMaxAgeError, Int] = {
    val result = fold(building, 0)((acc: Int, floor: Floor) =>
      floor match {
        case ResidentialFloor((Woman(age1), Woman(age2)), _) => List(acc, age1, age2).max
        case ResidentialFloor((Woman(age), _), _)            => acc.max(age)
        case ResidentialFloor((_, Woman(age)), _)            => acc.max(age)
        case _                                               => acc
      }
    )
    result match {
      case 0 => Left(NoWomanInBuilding)
      case _ => Right(result)
    }
  }

  /** Находит кол-во коммерческих заведений в здании. Используйте [[fold]] */
  def countCommercial(building: Building): Int = fold(building, 0)((acc: Int, floor: Floor) =>
    floor match {
      case CommercialFloor(establishments, _) => acc + establishments.length
      case CommercialAttic(_)                 => acc + 1
      case _                                  => acc
    }
  )

  /* Находит среднее кол-во коммерческих заведений зданиях. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию */
  def countCommercialAvg(buildings: Array[Building]): Either[CountCommercialAvgError, Double] = buildings.length match {
    case 0 => Left(ZeroBuildingsAmount)
    case _ =>
      Right(
        buildings
          .map(building =>
            fold(building, 0)((acc: Int, floor: Floor) =>
              floor match {
                case CommercialFloor(establishments, _) => acc + establishments.length
                case CommercialAttic(_)                 => acc + 1
                case _                                  => acc
              }
            )
          )
          .sum / buildings.length
      )
  }

  sealed trait FloorParity
  case object Even extends FloorParity
  case object Odd extends FloorParity
  /* Находит среднее кол-во мужчин на четных этажах. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию */
  def evenFloorsMenAvg(building: Building): Double = {
    val result = fold(building, (0, Odd: FloorParity, 0))((acc: (Int, FloorParity, Int), floor: Floor) =>
      (acc, floor) match {
        case ((countAcc, Even, evenFloorAmount), ResidentialFloor(guests, _)) =>
          guests match {
            case (Man(_), Man(_)) => (countAcc + 2, Odd, evenFloorAmount + 1)
            case (_, Man(_))      => (countAcc + 1, Odd, evenFloorAmount + 1)
            case (Man(_), _)      => (countAcc + 1, Odd, evenFloorAmount + 1)
            case _                => (countAcc, Odd, evenFloorAmount + 1)
          }
        case ((countAcc, Even, evenFloorAmount), _) => (countAcc, Odd, evenFloorAmount + 1)
        case ((countAcc, Odd, evenFloorAmount), _)  => (countAcc, Even, evenFloorAmount)
      }
    )
    result._1 / result._3
  }

}

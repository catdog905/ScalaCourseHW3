package building.floor

import building.Guest

sealed trait Floor
sealed trait Attic extends Floor

case object UninhabitedAttic extends Attic
case class CommercialAttic(establishment: Establishment) extends Attic

sealed trait MiddleFloors extends Floor {
  def nextFloor: Floor
}

case class ResidentialFloor(guests: (Guest, Guest), nextFloor: Floor) extends MiddleFloors

case class Establishment(name: String)
case class CommercialFloor private (establishments: Array[Establishment], nextFloor: Floor) extends MiddleFloors
object CommercialFloor {
  def apply(
    establishments: Array[Establishment],
    floor: Floor
  ): Either[CommercialFloorInstantiationException, CommercialFloor] = establishments.length match {
    case 0 => Left(ZeroAmountOfEstablishments)
    case _ => Right(new CommercialFloor(establishments, floor))
  }
}

package building

import building.floor.{CommercialAttic, CommercialFloor, Establishment, ResidentialFloor, UninhabitedAttic}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BuildingSpec extends AnyFlatSpec with Matchers {
  "countOldManFloors" should "return the number of men older than the specified age" in {
    val home = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Grocery store")),
        ResidentialFloor(
          (Man(32).getOrElse(null), Woman(26).getOrElse(null)),
          ResidentialFloor(
            (Man(64).getOrElse(null), Woman(88).getOrElse(null)),
            ResidentialFloor((Man(8).getOrElse(null), Woman(52).getOrElse(null)), UninhabitedAttic)
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    Building.countOldManFloors(home, 30) shouldEqual 2
  }

  it should "return 0 if there are no men older than the specified age" in {
    val home = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Grocery store")),
        ResidentialFloor(
          (Man(12).getOrElse(null), Woman(26).getOrElse(null)),
          ResidentialFloor(
            (Man(14).getOrElse(null), Woman(88).getOrElse(null)),
            ResidentialFloor((Man(8).getOrElse(null), Woman(52).getOrElse(null)), UninhabitedAttic)
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    Building.countOldManFloors(home, 30) shouldEqual 0
  }

  it should "return 0 if there are no men at all in the building" in {
    val home = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Grocery store")),
        ResidentialFloor(
          (Woman(32).getOrElse(null), Woman(26).getOrElse(null)),
          ResidentialFloor(
            (Woman(64).getOrElse(null), Woman(88).getOrElse(null)),
            ResidentialFloor((Woman(8).getOrElse(null), Woman(52).getOrElse(null)), UninhabitedAttic)
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    Building.countOldManFloors(home, 30) shouldEqual 0
  }

  "womanMaxAge" should "find age of the oldest woman in the building" in {
    val home = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Grocery store")),
        ResidentialFloor(
          (Man(32).getOrElse(null), Woman(26).getOrElse(null)),
          ResidentialFloor(
            (Man(64).getOrElse(null), Woman(88).getOrElse(null)),
            ResidentialFloor((Man(8).getOrElse(null), Woman(52).getOrElse(null)), UninhabitedAttic)
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    Building.womanMaxAge(home).getOrElse() shouldEqual 88
  }

  it should "throw NoWomanInBuilding if there are no women in the building" in {
    val home = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Grocery store")),
        ResidentialFloor(
          (Man(32).getOrElse(null), Man(26).getOrElse(null)),
          ResidentialFloor(
            (Man(64).getOrElse(null), Man(88).getOrElse(null)),
            ResidentialFloor((Man(8).getOrElse(null), Man(52).getOrElse(null)), UninhabitedAttic)
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    Building.womanMaxAge(home) shouldEqual Left(NoWomanInBuilding)
  }

  "countCommercial" should "return number of commercial establishments in the building" in {
    val home = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Grocery store"), Establishment("China mobile")),
        ResidentialFloor(
          (Man(32).getOrElse(null), Woman(26).getOrElse(null)),
          ResidentialFloor(
            (Man(64).getOrElse(null), Woman(88).getOrElse(null)),
            ResidentialFloor(
              (Man(8).getOrElse(null), Woman(52).getOrElse(null)),
              CommercialAttic(Establishment("Secret market"))
            )
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    Building.countCommercial(home) shouldEqual 3
  }

  it should "return 0 if there are no commercial establishments in the building" in {
    val home = Building(
      "Innopolis",
      ResidentialFloor(
        (Man(32).getOrElse(null), Woman(26).getOrElse(null)),
        ResidentialFloor(
          (Man(64).getOrElse(null), Woman(88).getOrElse(null)),
          ResidentialFloor((Man(8).getOrElse(null), Woman(52).getOrElse(null)), UninhabitedAttic)
        )
      )
    ).getOrElse(null)
    Building.countCommercial(home) shouldEqual 0
  }

  "countCommercialAvg" should "return average number of commercials in all buildings in total" in {
    val home = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Grocery store"), Establishment("China mobile")),
        ResidentialFloor(
          (Man(32).getOrElse(null), Woman(26).getOrElse(null)),
          ResidentialFloor(
            (Man(64).getOrElse(null), Woman(88).getOrElse(null)),
            ResidentialFloor(
              (Man(8).getOrElse(null), Woman(52).getOrElse(null)),
              CommercialAttic(Establishment("Secret market"))
            )
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    val superMarket = Building(
      "Innopolis",
      CommercialFloor(
        Array(
          Establishment("China mobile"),
          Establishment("China grocery store"),
          Establishment("Mister Li"),
          Establishment("Food store")
        ),
        CommercialFloor(
          Array(
            Establishment("Huawei"),
            Establishment("Hiaomi"),
            Establishment("Samsung"),
            Establishment("Grocery store"),
            Establishment("China mobile")
          ),
          UninhabitedAttic
        ).getOrElse(null)
      ).getOrElse(null)
    ).getOrElse(null)
    val skyScraper = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Cava"), Establishment("InnoMax")),
        ResidentialFloor(
          (Man(55).getOrElse(null), Woman(14).getOrElse(null)),
          ResidentialFloor(
            (Woman(88).getOrElse(null), Woman(16).getOrElse(null)),
            ResidentialFloor(
              (Man(22).getOrElse(null), Man(64).getOrElse(null)),
              ResidentialFloor(
                (Man(20).getOrElse(null), Woman(18).getOrElse(null)),
                ResidentialFloor(
                  (Man(20).getOrElse(null), Man(20).getOrElse(null)),
                  CommercialAttic(Establishment("Starbucks"))
                )
              )
            )
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    Building.countCommercialAvg(Array(home, superMarket, skyScraper)).getOrElse() shouldEqual 5.0
  }

  "evenFloorsMenAvg" should "return average number of men living at even floors" in {
    val skyScraper = Building(
      "Innopolis",
      CommercialFloor(
        Array(Establishment("Cava"), Establishment("InnoMax")),
        ResidentialFloor(
          (Man(55).getOrElse(null), Woman(14).getOrElse(null)),
          ResidentialFloor(
            (Woman(88).getOrElse(null), Woman(16).getOrElse(null)),
            ResidentialFloor(
              (Man(22).getOrElse(null), Man(64).getOrElse(null)),
              ResidentialFloor(
                (Man(20).getOrElse(null), Woman(18).getOrElse(null)),
                ResidentialFloor(
                  (Man(50).getOrElse(null), Man(20).getOrElse(null)),
                  ResidentialFloor(
                    (Man(55).getOrElse(null), Woman(14).getOrElse(null)),
                    ResidentialFloor(
                      (Woman(48).getOrElse(null), Woman(16).getOrElse(null)),
                      ResidentialFloor(
                        (Man(23).getOrElse(null), Man(24).getOrElse(null)),
                        ResidentialFloor(
                          (Man(10).getOrElse(null), Woman(18).getOrElse(null)),
                          ResidentialFloor(
                            (Man(28).getOrElse(null), Man(22).getOrElse(null)),
                            CommercialAttic(Establishment("Starbucks"))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ).getOrElse(null)
    ).getOrElse(null)
    Building.evenFloorsMenAvg(skyScraper) shouldEqual 1.0
  }
}

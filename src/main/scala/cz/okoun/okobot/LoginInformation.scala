package cz.okoun.okobot


class LoginInformation(val username:String, val password:String)



object LoginInformation {

    def apply(username:String, password:String) = new LoginInformation(username, password)

    def apply(f:java.io.File):LoginInformation =
        scala.io.Source.fromFile(f).getLines().toSeq match {
            case Seq(username:String, password:String) => new LoginInformation(username, password)
            case k => println("Shit error! "+k); throw new Exception("whatever")
        }

}

